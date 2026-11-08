// Port of lib/flow/test_workspace.ml (the checker's static half).
use crate::*;
use flow_check::{catalog::{self, Catalog, Kind, Parameter}, port_type::PortType, workspace as W};
use flow_syntax::{context, macros, syntax::{self, Node as SN, Syntax}, ty, value};
use rays_diagnostic::{self, Diagnostic, Severity};
use std::sync::OnceLock;

pub fn base_catalog() -> &'static Catalog {
    static C: OnceLock<Catalog> = OnceLock::new();
    C.get_or_init(|| {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/flow_sop/flow_manifest.sexp");
        catalog::catalog_of_manifest(&std::fs::read_to_string(path).unwrap()).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))).0
    })
}
pub fn parse(source: &str) -> Vec<Syntax> { syntax::parse(source).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }
pub fn run_in(c: &Catalog, source: &str) -> (Option<W::Workspace>, Vec<Diagnostic>) { W::check(&[], false, false, None, c, &parse(source)) }
pub fn run_src(source: &str) -> (Option<W::Workspace>, Vec<Diagnostic>) { run_in(base_catalog(), source) }
pub fn errors(ds: &[Diagnostic]) -> Vec<&Diagnostic> { ds.iter().filter(|d| d.severity == Severity::ErrorSeverity).collect() }
pub fn show_ds(ds: &[Diagnostic]) -> String { ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ") }
pub fn sop(b: &str) -> String { format!("(workspace w (graph g :context sop {b}))") }
pub fn val(b: &str) -> String { format!("(workspace w (graph g :context value {b}))") }
pub fn good(source: &str) -> W::Workspace {
    match run_src(source) { (Some(ws), ds) if errors(&ds).is_empty() => ws, (_, ds) => panic!("{source}\n  rejected: {}", show_ds(&ds)) }
}
pub fn good_in(c: &Catalog, source: &str) -> W::Workspace {
    match run_in(c, source) { (Some(ws), ds) if errors(&ds).is_empty() => ws, (_, ds) => panic!("{source}\n  rejected: {}", show_ds(&ds)) }
}
pub fn bad(source: &str, code: &str) { bad_text(source, code, "") }
pub fn bad_text(source: &str, code: &str, text: &str) {
    match run_src(source) {
        (None, ds) if ds.iter().any(|d| d.code == code && d.severity == Severity::ErrorSeverity && d.message.contains(text)) => (),
        (_, ds) => panic!("{source}\n  expected {code} {text:?}, got: {}", show_ds(&ds)),
    }
}
pub fn warns(source: &str, code: &str) {
    match run_src(source) {
        (Some(_), ds) => assert!(ds.iter().any(|d| d.code == code && d.severity == Severity::Warning), "{source}: {}", show_ds(&ds)),
        (_, ds) => panic!("{source}{}", show_ds(&ds)),
    }
}
pub fn quiet(source: &str) {
    match run_src(source) { (Some(_), ds) => assert!(ds.is_empty(), "{source}: {}", show_ds(&ds)), (_, ds) => panic!("{source}{}", show_ds(&ds)) }
}
fn p(parts: &[&str]) -> Vec<String> { parts.iter().map(|s| s.to_string()).collect() }
fn live(ws: &W::Workspace, path: &[&str]) -> bool { ws.live.contains(&p(path)) }
fn invariant(ws: &W::Workspace, path: &[&str]) -> bool { ws.invariant.contains(&p(path)) }
fn results_of(e: &Eval, g: &str) -> Value { e.results.iter().find(|(n, _)| n == g).unwrap().1.clone() }
fn static_ok(ws: &W::Workspace) -> Eval { static_eval(ws).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }
fn run_time0(ws: &W::Workspace) -> Eval { run(ws, false, None, None, None, 0.).unwrap() }

#[test]
fn vectors_widen_and_check() {
    let ws = good("(workspace vectors\n    (defn shift :context value [(p : vec2) (q : vec4)] {:p (+ p 2) :q (* 2 q)})\n    (defn count2 :context value [(a : (array vec2))] (array/count a))\n    (defn count4 :context value [(a : (array vec4))] (array/count a))\n    (graph g :context host\n      (let* [pair [t 2] quad [1 2 3 t]\n             moved (shift pair quad) [px py] pair [qx qy qz qw] quad]\n        {:pair pair :quad quad :moved moved :fields (+ px (+ py qw))})))");
    let result = results_of(&static_ok(&ws), "g");
    for time in [0., 0.25, 2.] {
        let live = input::at_time(time);
        let actual = force(&result, &live, None).unwrap();
        let reference = host::force_reference(&result, &live, None, None, None).unwrap();
        let expected = Value::Record(vec![
            ("pair".into(), Value::Vec2(time, 2.)),
            ("quad".into(), Value::Vec4(1., 2., 3., time)),
            ("moved".into(), Value::Record(vec![("p".into(), Value::Vec2(time + 2., 4.)), ("q".into(), Value::Vec4(2., 4., 6., 2. * time))])),
            ("fields".into(), Value::Float(time + 2. + time)),
        ]);
        assert!(actual == expected && reference == expected, "{time}");
    }
    for expression in ["(+ [1 2] [1 2 3])", "(* [1 2 3 4] [1 2])", "(- [1 2 3] [1 2 3 4])"] { bad_text(&val(expression), "E_TYPE", "matching vector widths"); }
    bad(&val("(let* [pair [1 2]] pair.z)"), "E_FIELD");
    bad_text(&val("(let* [[a b c] [1 2]] a)"), "E_PATTERN", "a vec2 has 2");
    bad(&val("[1 2 3 4 5]"), "E_VECTOR");
    for expression in ["[t 2]", "[1 2 3 t]"] {
        let ws = good(&val(&format!("(let* [v {expression}] 0)")));
        assert!(ws.approx.is_empty());
        assert!(!ws.packed.iter().any(|(path, _)| *path == p(&["g", "v"])));
    }
    for (source, expected) in [
        ("(+ [1 2] true)", Value::Vec2(2., 3.)),
        ("(let* [f (fn [(p : vec4)] p)] (f 2))", Value::Vec4(2., 2., 2., 2.)),
        ("(let* [f (fn [(p : vec2)] p)] (f 3))", Value::Vec2(3., 3.)),
    ] {
        let ws = good(&format!("(workspace vectors (graph g :context host {source}))"));
        assert_eq!(results_of(&run_time0(&ws), "g"), expected, "{source}");
    }
}

#[test]
fn function_ports_instantiate_bodies_and_keep_captures() {
    let field = Parameter { name: "field".into(), label: "Field".into(), fields: vec![], folder: vec![], primary: true, unit: None, ty: Some(PortType::Fn(ty::FnSignature { params: vec![Ty::Float], result: Ty::Float })) };
    let kind = Kind { qualified: "sop/test_field".into(), context: context::sop(), slots: vec![], parameters: vec![field.clone()], outputs: vec![("geo".into(), PortType::Geometry)], facts: None };
    let run_with = |signature: &ty::FnSignature, source: &str| {
        let mut parameter = field.clone();
        parameter.ty = Some(PortType::Fn(signature.clone()));
        let mut k = kind.clone(); k.parameters = vec![parameter];
        let mut kinds = vec![k]; kinds.extend(base_catalog().kinds.clone());
        run_in(&Catalog { kinds, ..base_catalog().clone() }, source)
    };
    let signature = ty::FnSignature { params: vec![Ty::Record(vec![("p".into(), Ty::Vec3)])], result: Ty::Float };
    let accepted = |signature: &ty::FnSignature, source: &str| match run_with(signature, source) {
        (Some(ws), ds) if errors(&ds).is_empty() => ws,
        (_, ds) => panic!("{source}\n{}", show_ds(&ds)),
    };
    let ws = accepted(&signature, &sop("(sop/test_field :field (fn [r] r.p.x))"));
    let root = &ws.graphs[0].body;
    let W::Node::Call(id) = root.node else { panic!("function port did not retain its checked body"); };
    let arena = root.arena();
    let args = arena.fields(arena.call(id).args);
    assert!(args.len() == 1 && root.name(args[0].name) == "field");
    let t = root.child(args[0].term);
    assert!(matches!(t.ty(), Ty::Fn(Some(certified)) if **certified == signature));
    let W::Node::Fn(id) = t.node else { panic!("{:?}", t.debug_node()); };
    let function = arena.function(id);
    let params = arena.params(function.params);
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].ty.map(|id| arena.ty(id)), Some(&signature.params[0]));
    assert_eq!(t.child(function.body).ty(), &Ty::Float);
    for (source, code) in [
        ("(sop/test_field :field 1.0)", "E_TYPE"),
        ("(sop/test_field :field (fn [r s] r.p.x))", "E_ARITY"),
        ("(sop/test_field :field (fn [r] (image/noise :width 1 :height 1)))", "E_TYPE"),
        ("(let* [stored {:f (fn [r] r.p.x)}] (sop/box))", "E_FN_ESCAPES"),
    ] {
        match run_with(&signature, &sop(source)) {
            (None, ds) if ds.iter().any(|d| d.code == code) => (),
            (_, ds) => panic!("{source}: expected {code}, got {}", show_ds(&ds)),
        }
    }
    let ws = accepted(&ty::FnSignature { params: vec![Ty::Float], result: Ty::Float }, "(workspace w (defn consume :context sop [(f : fn)] (sop/test_field :field f)) (graph g :context sop (let* [offset 7.0 f7 (fn [x] (+ x offset)) alias f7 f3 (fn [x] (* x 3.0))] (sop/merge (list (consume alias) (consume f3))))))");
    let evaluated = static_ok(&ws);
    let functions = evaluated.plan.nodes.iter().filter(|n| n.kind == "sop/test_field").map(|n| match n.args.iter().find(|(k, _)| k == "field").unwrap().1 { Value::Fn(f) => f, ref other => panic!("function port lost its callable: {other:?}") }).collect::<Vec<_>>();
    assert_eq!(functions.len(), 2);
    for (i, f) in functions.iter().enumerate() {
        let (_, body) = residual::function_body(*f).unwrap();
        assert_eq!(body.ty(), &Ty::Float);
        if i == 0 {
            assert_eq!(residual::function_bindings(*f).iter().find(|(k, _)| k == "offset").unwrap().1, Value::Float(7.));
        }
    }
}

#[test]
fn canvas_accepts_an_image_through_the_drawing_operator() {
    let ws = good("(workspace w (graph picture :context image (image/noise :width 2 :height 2)) (graph editor :context editor (ui/workspace (ui/canvas (ref picture)))))");
    let editor = ws.graphs.iter().find(|g| g.name == "editor").unwrap();
    let root = &editor.body;
    let arena = root.arena();
    let W::Node::Op(id) = root.node else { panic!("canvas image did not lower to draw/image"); };
    let op = arena.op(id);
    let args = arena.fields(op.args);
    assert!(root.name(op.op) == "ui/workspace" && args.len() == 1);
    let canvas = root.child(args[0].term);
    let W::Node::Op(id) = canvas.node else { panic!("{:?}", canvas.debug_node()); };
    let op = arena.op(id);
    let args = arena.fields(op.args);
    assert!(root.name(op.op) == "ui/canvas" && args.len() == 1 && root.name(args[0].name) == "drawing");
    let drawing = root.child(args[0].term);
    let W::Node::Op(id) = drawing.node else { panic!("{:?}", drawing.debug_node()); };
    let op = arena.op(id);
    let args = arena.fields(op.args);
    assert!(root.name(op.op) == "draw/image" && args.len() == 1 && root.name(args[0].name) == "image");
    assert!(drawing.ty() == &ty::drawing() && root.child(args[0].term).ty() == &ty::image());
    let evaluated = static_ok(&ws);
    match results_of(&evaluated, "editor") {
        Value::Struct(g) if g.name == "ui/workspace" && g.fields.len() == 1 && g.fields[0].0 == "root" => match &g.fields[0].1 {
            Value::Struct(g) if g.name == "ui/canvas" && g.fields.len() == 1 && g.fields[0].0 == "drawing" => match &g.fields[0].1 {
                Value::Deferred(t, id) => {
                    let drawing = &evaluated.plan.nodes[*id as usize];
                    assert!(*t == ty::drawing() && drawing.kind == "draw/image");
                    assert_eq!(drawing.args.iter().find(|(k, _)| k == "image").unwrap().1, results_of(&evaluated, "picture"));
                }
                other => panic!("{other:?}"),
            },
            other => panic!("{other:?}"),
        },
        other => panic!("canvas image did not evaluate as a drawing: {other:?}"),
    }
    bad("(workspace w (graph editor :context editor (ui/workspace (ui/canvas 1))))", "E_TYPE");
}

#[test]
fn image_let_in_every_context() {
    for context in ["value", "sop", "draw", "image"] {
        let tail = match context { "value" => "0", "sop" => "(sop/box)", "draw" => "(draw/background \"#000000\")", _ => "img" };
        good(&format!("(workspace w (graph g :context {context} (let* [img (image/noise :width 1 :height 1)] {tail})))"));
    }
}

fn cases_dir() -> std::path::PathBuf { std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../specification/workspace/cases") }
fn case_files() -> Vec<std::path::PathBuf> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut files = vec![cases_dir().join("bloom.lisp"), cases_dir().join("variations.lisp")];
    for name in ["facade", "garland", "kit", "orrery", "rosette", "sunflower", "tiles", "tree", "tunnel", "wave"] { files.push(root.join("sketches").join(format!("ws_{name}.rays"))); }
    files
}

#[test]
fn the_twelve_fixtures_check_with_no_diagnostics() {
    let files = case_files();
    assert_eq!(files.len(), 12);
    let failed = files.iter().filter_map(|file| {
        let (ws, ds) = run_src(&std::fs::read_to_string(file).unwrap());
        if ws.is_none() || !ds.is_empty() { Some(format!("{}: {}", file.display(), show_ds(&ds))) } else { None }
    }).collect::<Vec<_>>();
    assert!(failed.is_empty(), "{}", failed.join("\n"));
}

#[test]
fn structure_and_the_typed_ir() {
    let ws = good("; note\n(workspace w (defn f :context value [(x : float 2.0)] (* x 2)) (defmacro m [a] `(+ ~a ~a)) (graph g :context value [(n : int 3)] (let* [a (f :x n)] (+ a (m 1)))))");
    assert!(ws.name == "w" && ws.graphs.len() == 1 && ws.defs.len() == 1 && ws.macros.len() == 1);
    let g = &ws.graphs[0];
    assert!(g.context == context::value() && g.inputs.len() == 1);
    let root = &g.body;
    let arena = root.arena();
    let W::Node::Let(id) = root.node else { panic!("unexpected IR of g"); };
    let node = arena.let_node(id);
    let bs = arena.bindings(node.bindings);
    assert_eq!(bs.len(), 1);
    assert!(matches!(arena.pattern(bs[0].pattern), flow_check::term_arena::PatternNode::Name(n) if root.name(n) == "a"));
    let bound = root.child(bs[0].term);
    assert!(matches!(bound.node, W::Node::CallFn(id) if root.name(arena.call_fn(id).name) == "f" && arena.term_ids(arena.call_fn(id).args).len() == 1));
    assert_eq!(bound.path().map(|names| names.iter().map(|n| root.name(*n).to_owned()).collect::<Vec<_>>()), Some(p(&["g", "a"])));
    let r = root.child(node.body);
    assert_eq!(r.path().map(|names| names.iter().map(|n| root.name(*n).to_owned()).collect::<Vec<_>>()), Some(p(&["g", "@result"])));
    let W::Node::Op(id) = r.node else { panic!("result is not (+ a expansion)"); };
    let op = arena.op(id);
    let args = arena.fields(op.args);
    assert!(root.name(op.op) == "+" && args.len() == 2);
    assert!(matches!(root.child(args[1].term).node, W::Node::Expanded(id) if root.name(arena.expanded(id).name) == "m"));
    bad("", "E_NO_WORKSPACE");
    bad("(graph g :context sop nil)", "E_WORKSPACE");
    bad("(workspace w) (workspace v)", "E_ONE_WORKSPACE");
    bad("(workspace w (defn f :context value [(x : float)] x))", "E_NO_GRAPH");
    bad("(workspace w (thing g) (graph g :context value 1))", "E_FORM");
    bad("(workspace w (graph g :context value 1) (graph g :context value 2))", "E_NAME");
    bad("(workspace w (graph let* :context value 1))", "E_NAME");
    bad("(workspace w (graph g :context nowhere 1))", "E_CONTEXT_UNKNOWN");
    bad("(workspace w (graph g :context value [(n : bogus 1)] 1))", "E_PARAM");
    bad("(workspace w (graph g :context value [(n : int 1) (n : int 2)] 1))", "E_PARAM");
    bad("(workspace w (defn f :context value 1) (graph g :context value 1))", "E_SHAPE");
    quiet(&val("1"));
}

#[test]
fn input_defaults_shadowing_and_reserved_t() {
    bad_text("(workspace w (graph a :context value [(n : int)] n))", "E_INPUT_DEFAULT", "needs a default");
    good("(workspace w (graph a :context value [(n : int 2)] n))");
    good("(workspace w (defn f :context value [(n : int)] n) (graph a :context value (f :n 1)))");
    bad_text("(workspace w (defn f :context value [(n : int)] n) (graph a :context value (f)))", "E_ARGS", "needs :n");
    bad_text(&sop("(let* [a 1 b (for [a (range 3)] (sop/box))] (sop/merge b))"), "E_SHADOW", "shadows");
    bad(&val("(let* [a 1 f (fn [a] a)] 1)"), "E_SHADOW");
    bad(&val("(let* [x 1 y (let* [x 2] x)] y)"), "E_SHADOW");
    bad_text(&val("(fold [a 0] [a (range 3)] 1)"), "E_DUPLICATE", "bound twice");
    bad(&val("(let* [1 2] 3)"), "E_PATTERN");
    good(&val("(let* [a 1 b (for [i (range 3)] i)] a)"));
    bad_text(&sop("(let* [t 1] (sop/box))"), "E_BINDING", "context time");
    bad_text(&val("(let* [f (fn [t] t)] 1)"), "E_BINDING", "context time");
    bad(&val("(for [t (range 3)] t)"), "E_BINDING");
    bad("(workspace w (graph g :context value [(t : int 1)] 1))", "E_PARAM");
}

#[test]
fn functions_cannot_escape() {
    let msg = "cannot be stored or returned";
    bad_text(&val("(let* [f (fn [x] x)] f)"), "E_FN_ESCAPES", msg);
    bad_text(&val("(let* [f (fn [x] x) l (list f)] 1)"), "E_FN_ESCAPES", msg);
    bad_text(&val("(let* [f (fn [x] x) r {:f f}] 1)"), "E_FN_ESCAPES", msg);
    bad_text(&val("(let* [f (fn [x] x)] (fold [a f] [i (range 2)] 1))"), "E_FN_ESCAPES", msg);
    bad_text(&val("(count (map (fn [x] (fn [y] y)) (list 1)))"), "E_FN_ESCAPES", msg);
    bad_text("(workspace w (graph a :context value [(n : int 2)] n) (graph g :context value (let* [f (fn [x] x)] (ref a :n f))))", "E_FN_ESCAPES", msg);
    bad_text("(workspace w (defn h :context value [(k : fn)] k) (graph g :context value 1))", "E_FN_ESCAPES", msg);
    bad_text("(workspace w (graph g :context value [(k : fn)] 1))", "E_FN_ESCAPES", msg);
    good(&val("(let* [f (fn [a b] (+ a b))] (f 1 2))"));
    good(&val("(reduce + 0 (map (fn [x] (* x 2)) (list 1 2 3)))"));
    good("(workspace w (defn ring :context sop [(n : int 8) (make : fn)] (sop/merge (map make (range n)))) (graph g :context sop (ring :n 3 :make (fn [i] (sop/box)))))");
    bad_text(&val("(let* [f (fn [x] (f x))] (f 1))"), "E_UNKNOWN_KIND", "Unknown operator");
    bad("(workspace w (defmacro dbl [x] `(+ ~x ~x)) (graph g :context value (count (map dbl (list 1)))))", "E_MACRO_AS_VALUE");
}

#[test]
fn accumulators_iteration_bounds_branches_and_patterns() {
    bad_text(&sop("(fold [g (sop/box)] [i (range 3)] 1.0)"), "E_ACC_TYPE", "accumulator type");
    bad(&val("(scan [a 1] [i (range 3)] \"s\")"), "E_ACC_TYPE");
    good(&sop("(fold [g (sop/box)] [i (range 3)] (sop/merge g (sop/box)))"));
    good(&val("(let* [s (fold [{:keys [a b]} {:a 0 :b 1}] [i (range 10)] {:a b :b (+ a b)})] s.a)"));
    bad_text(&val("(fold [a 0 b 1] [i (range 3)] a)"), "E_ZONE", "exactly one accumulator");
    bad_text(&sop("(sop/merge (for [i (range 5000)] (sop/box)))"), "E_ITER_BOUND", "4,096");
    bad(&val("(count (range 10 5000))"), "E_ITER_BOUND");
    bad(&val("(count (linspace 0 1 5000))"), "E_ITER_BOUND");
    bad_text(&sop("(sop/merge (for [x (range 100) y (range 100)] (sop/box)))"), "E_ITER_BOUND", "4,096");
    good(&sop("(sop/merge (for [x (range 64) y (range 64)] (sop/box)))"));
    good(&sop("(sop/merge (for [i (range 4096)] (sop/box)))"));
    good("(workspace w (graph g :context sop [(n : int 9999)] (sop/merge (for [i (range n)] (sop/box)))))");
    bad_text(&val("(let* [z (for [i 3] i)] 1)"), "E_TYPE", "iterates a list");
    bad_text(&val("(cond (< 2 1) 1)"), "E_NO_ELSE", "final :else");
    bad(&val("(case 1 1 2)"), "E_NO_ELSE");
    bad(&val("(if true 1)"), "E_NO_ELSE");
    good(&val("(cond (< 2 1) 1 (> 2 1) 2 :else 3)"));
    good(&val("(case 2 1 10 2 20 :else 0)"));
    bad_text(&val("(let* [k 1] (case 1 k 2 :else 0))"), "E_CASE", "literal");
    bad_text(&val("(cond true 1 :else \"a\")"), "E_TYPE", "one type");
    bad_text(&sop("(if (< 1 2) (sop/box) 3)"), "E_TYPE", "Both branches");
    bad(&val("(if true 1 (+ 1 \"x\"))"), "E_TYPE");
    bad_text(&val("(let* [[a b c] (list 1 2)] a)"), "E_PATTERN", "needs 3 elements; the list has 2");
    bad_text(&val("(let* [[a b c d] [1 2 3]] a)"), "E_PATTERN", "a vec3 has 3");
    bad_text(&val("(let* [{:keys [a c]} {:a 1}] a)"), "E_FIELD", "no field c");
    bad_text(&val("(let* [{:keys [a]} 1] a)"), "E_PATTERN", "destructures a record");
    bad_text(&val("(let* [[a b] 1] a)"), "E_PATTERN", "destructures a list or vec3");
    bad(&val("(let* [[] (list 1)] 1)"), "E_PATTERN");
    bad_text(&val("(let* [map 1] map)"), "E_BINDING", "Invalid binding name map");
    bad(&val("(let* [[a list] (list 1 2)] a)"), "E_BINDING");
    good(&val("(let* [[x y z] [1 2 3] {:keys [a b]} {:a 10 :b 20}] (+ (* x a) (* z b)))"));
    good(&val("(let* [f (fn [[a b]] (* a b)) s (sum [[a b] (list (list 1 2) (list 3 4))] (+ (f (list a b)) 0))] s)"));
}

#[test]
fn other_static_diagnostics_of_the_study() {
    bad_text(&val("(let* [a 1] b)"), "E_UNBOUND", "not bound");
    bad(&sop("(let* [a (for [i (range 2)] (sop/box))] (sop/transform (sop/merge a) :rotate [0 0 i]))"), "E_UNBOUND");
    bad_text(&val("(+ 1)"), "E_ARITY", "takes 2 positional");
    bad_text(&val("(let* [f (fn [a b] a)] (f 1))"), "E_ARITY", "takes 2 arguments; got 1");
    bad_text(&sop("(sop/box :sizes 1)"), "E_UNKNOWN_PARAM", "Did you mean");
    bad(&sop("(sop/box :size 1 :size 2)"), "E_DUPLICATE_PARAM");
    bad_text(&sop("(sop/box :size [1 2])"), "E_TYPE", "takes vec3, but this is vec2");
    bad(&sop("(sop/box :x_divisions \"a\")"), "E_TYPE");
    bad(&sop("(sop/box :x_divisions 0)"), "E_HARD_RANGE");
    warns(&sop("(sop/box :x_divisions 99)"), "W_SOFT_RANGE");
    good(&sop("(sop/transform)"));
    good(&sop("(sop/exploded_view)"));
    good(&sop("(sop/exploded_view nil)"));
    bad(&sop("(sop/box (sop/box))"), "E_EXTRA_POSITIONAL");
    bad_text(&sop("(sop/nothing)"), "E_UNKNOWN_KIND", "Unknown operator");
    bad(&val("(sop/box)"), "E_WRONG_CONTEXT");
    bad("(workspace w (graph g :context scene (scene/geometry (sop/box))))", "E_WRONG_CONTEXT");
    bad_text(&sop("(scene/geometry (sop/box))"), "E_WRONG_CONTEXT", "scene node");
    bad_text("(workspace w (defn f :context sop [(x : float)] (f x)) (graph g :context sop (f 1)))", "E_RECURSION", "Use fold");
    bad("(workspace w (defn app :context value [(f : fn) (x : float)] (f f x)) (graph g :context value (app app 1)))", "E_RECURSION");
    bad_text("(workspace w (graph a :context value [(n : int 2)] n) (graph b :context value (ref a :m 1)))", "E_UNKNOWN_PARAM", "no input :m");
    bad(&val("(ref nope)"), "E_UNKNOWN_GRAPH");
    bad("(workspace w (defn f :context value [(x : float)] (ref g)) (graph g :context value 1))", "E_REF_IN_DEF");
    bad_text("(workspace w (graph a :context value (ref b)) (graph b :context value (ref a)))", "E_GRAPH_CYCLE", "a → b → a");
}

#[test]
fn liveness_follows_t() {
    let ws = good(&sop("(let* [a (sop/box) b (* t 2) c (+ b 1) d (sop/transform a :translate [c 0 0]) e (sop/transform d)] e)"));
    assert!(!live(&ws, &["g", "a"]));
    assert!(live(&ws, &["g", "b"]) && live(&ws, &["g", "c"]));
    assert!(live(&ws, &["g", "d"]) && live(&ws, &["g", "e"]) && live(&ws, &["g", "@result"]));
    let ws = good(&sop("(let* [k (sin t) ring (for [i (range 3)] (let* [s (* i 2) w (+ s k)] (sop/box :size w)))] (sop/merge ring))"));
    assert!(live(&ws, &["g", "k"]) && live(&ws, &["g", "ring"]) && live(&ws, &["g", "ring", "w"]));
    assert!(!live(&ws, &["g", "ring", "s"]) && !live(&ws, &["g", "ring", ":i"]));
    let ws = good(&val("(let* [f (fold [a 0] [i (range 3)] (let* [b (* a 2)] (+ b t)))] f)"));
    assert!(live(&ws, &["g", "f"]) && live(&ws, &["g", "f", "b"]) && live(&ws, &["g", "f", ":a"]));
    let ws = good(&val("(let* [f (fold [a t] [i (range 3)] (let* [b (* a 2)] b))] f)"));
    assert!(live(&ws, &["g", "f", "b"]));
    let ws = good(&val("(let* [f (fold [a 1] [i (range 3)] (let* [b (* a 2)] b))] f)"));
    assert!(!live(&ws, &["g", "f"]) && !live(&ws, &["g", "f", "b"]));
    let ws = good(&val("(let* [f (fn [x] (+ x t)) g2 (fn [x] (* x 2)) a (f 1) b (g2 1) c (g2 t)] (+ a (+ b c)))"));
    assert!(live(&ws, &["g", "a"]) && !live(&ws, &["g", "b"]) && live(&ws, &["g", "c"]));
    let ws = good("(workspace w (defn pulse :context value [(x : float)] (* x t)) (defn twice :context value [(x : float)] (* x 2)) (graph g :context value (let* [a (pulse 1) b (twice 2) c (twice a)] (+ a (+ b c)))))");
    assert!(live(&ws, &["g", "a"]) && !live(&ws, &["g", "b"]) && live(&ws, &["g", "c"]));
    let ws = good(&val("(let* [xs (map (fn [x] (* x t)) (list 1 2)) ys (map (fn [x] x) (list 1 2)) zs (map (fn [x] x) (list t))] (count xs))"));
    assert!(live(&ws, &["g", "xs"]) && !live(&ws, &["g", "ys"]) && live(&ws, &["g", "zs"]));
    let ws = good("(workspace w (graph a :context value [(n : int 2)] (* n t)) (graph b :context value [(n : int 2)] (* n 2)) (graph c :context value (+ (ref a) (+ (ref b) (ref b :n t)))))");
    assert!(live(&ws, &["c", "@result"]));
    let ws = good("(workspace w (graph a :context value [(n : int 2)] (* n t)) (graph b :context value [(n : int 2)] (* n 2)) (graph c :context value (let* [x (ref a) y (ref b) z (ref b :n 5) w (ref b :n t)] (+ x (+ y (+ z w))))))");
    assert!(live(&ws, &["c", "x"]) && !live(&ws, &["c", "y"]) && !live(&ws, &["c", "z"]) && live(&ws, &["c", "w"]));
    assert!(live(&ws, &["a", "@result"]) && !live(&ws, &["b", "@result"]));
    let m = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))";
    let ws = good(&format!("(workspace w {m} (graph g :context value (let* [x (add1 1) y (add1 t)] (+ x y))))"));
    assert!(!live(&ws, &["g", "x"]) && live(&ws, &["g", "y"]));
    let ws = good(&sop("(let* [a (sop/box) b (sop/transform a)] b)"));
    assert!(ws.live.is_empty());
}

#[test]
fn loop_invariance() {
    let ws = good(&sop("(let* [ring (for [i (range 4)] (let* [up [0 1 0] r (* i 0.1) mv (sop/transform (sop/box) :translate up) bg (sop/transform mv :uniform_scale r)] bg))] (sop/merge ring))"));
    assert!(invariant(&ws, &["g", "ring", "up"]) && invariant(&ws, &["g", "ring", "mv"]));
    assert!(!invariant(&ws, &["g", "ring", "r"]) && !invariant(&ws, &["g", "ring", "bg"]));
    good(&val("(sum [i (range 2)] (sum [j (range 3)] (let* [a (* i 2) b (* j 2)] (+ a b))))"));
}

#[test]
fn time_dependent_counts_and_branches() {
    bad(&sop("(sop/merge (for [i (range (+ 3 (floor (* 2 (sin t)))))] (sop/box)))"), "E_TIME_COUNT");
    bad(&sop("(fold [s (sop/box)] [i (range (floor t))] s)"), "E_TIME_COUNT");
    bad_text(&sop("(sop/merge (filter (fn [g] (> t 1)) (list (sop/box) (sop/box))))"), "E_TIME_COUNT", "changes length");
    bad(&sop("(let* [k (floor t) xs (take k (list (sop/box) (sop/box)))] (sop/merge xs))"), "E_TIME_COUNT");
    good(&val("(sum [x (filter (fn [s] (> s t)) (list 1 2 3))] x)"));
    good(&val("(count (range (floor t)))"));
    good(&val("(fold [total 0] [x (range (floor t))] (+ total x))"));
    good(&val("(count (scan [total 0] [x (range (floor t))] (+ total x)))"));
    good(&sop("(sop/merge (for [i (range 3)] (sop/box :size (+ 1 (sin t)))))"));
    good(&sop("(sop/merge (map (fn [i] (sop/box :size t)) (range 3)))"));
    bad_text(&sop("(if (> (sin t) 0.5) (sop/box) (sop/uv_sphere))"), "E_TIME_BRANCH", "pick a value");
    bad(&sop("(let* [k (sin t)] (cond (> k 0.5) (sop/box) :else (sop/uv_sphere)))"), "E_TIME_BRANCH");
    bad(&sop("(case (floor t) 0 (sop/box) :else (sop/uv_sphere))"), "E_TIME_BRANCH");
    bad(&sop("(sop/merge (if (> t 1) (list (sop/box)) (list (sop/box) (sop/box))))"), "E_TIME_BRANCH");
    good(&sop("(sop/box :size (if (> (sin t) 0.5) 1 0.5))"));
    good(&sop("(sop/set_color (sop/box) :color (if (> (sin t) 0) \"#d69f61\" \"#6fa6a1\"))"));
    good("(workspace w (graph g :context sop [(k : int 1)] (if (> k 0) (sop/box) (sop/uv_sphere))))");
}

#[test]
fn groups_colours_and_bypass() {
    warns(&sop("(sop/blast (sop/box) :group \"nobody\")"), "W_UNKNOWN_GROUP");
    quiet(&sop("(sop/blast (sop/group_bounds (sop/box) :name \"attic\") :group \"attic\")"));
    quiet(&sop("(let* [a (sop/group_random (sop/box) :name \"lit\") b (sop/transform a) c (sop/merge b (sop/box))] (sop/set_color c :group \"lit\" :color \"#f5cf4f\"))"));
    quiet(&sop("(sop/blast (sop/group_bounds (sop/box) :name (str \"floor_\" 2)) :group (str \"floor_\" 2))"));
    warns(&sop("(sop/blast (sop/group_bounds (sop/box) :name \"a\") :group \"b\")"), "W_UNKNOWN_GROUP");
    quiet(&sop("(sop/set_color (sop/box) :group \"\" :color [1 0 0])"));
    quiet(&sop("(sop/set_color (sop/box) :color \"#b0680f\")"));
    quiet(&sop("(sop/set_color (sop/box) :color (value/hsv 0.1 0.5 0.9))"));
    bad_text(&sop("(sop/set_color (sop/box) :color \"red\")"), "E_TYPE", "colour");
    bad(&sop("(sop/set_color (sop/box) :color 3)"), "E_TYPE");
    quiet(&sop("(sop/uv_sphere :radius 0.1)"));
    quiet(&sop("(sop/uv_sphere :radius [0.1 0.2 0.3])"));
    let ws = good(&sop("(let* [a (sop/box) b ^:bypass (sop/transform a :translate [5 0 0])] b)"));
    let root = &ws.graphs[0].body;
    let arena = root.arena();
    let W::Node::Let(id) = root.node else { panic!("bypass is not wrapped"); };
    let bs = arena.bindings(arena.let_node(id).bindings);
    assert_eq!(bs.len(), 2);
    let W::Node::Bypass(call) = root.child(bs[1].term).node else { panic!("bypass is not wrapped"); };
    assert!(matches!(root.child(call).node, W::Node::Call(id) if root.name(arena.call(id).kind) == "sop/transform"));
    bad_text(&sop("^:mute (sop/box)"), "E_META", "only metadata is ^:bypass");
    bad_text(&sop("^:bypass (sop/box :size 2)"), "E_BYPASS", "no positional input");
    bad_text(&sop("^:bypass (let* [a 1] (sop/box))"), "E_BYPASS", "only operator and function calls");
}

#[test]
fn records_lists_str_and_hofs_type_flow() {
    let ws = good(&val("(let* [r {:a 1 :b {:c [1 2 3]}} s (assoc r :a 5 :d 2) n (+ r.a r.b.c.y) l (list 1 2.5) e (list) t2 (str \"a\" 1 [1 2 3])] (+ n s.d))"));
    let ty_of = |name: &str| {
        let root = &ws.graphs[0].body;
        let arena = root.arena();
        let W::Node::Let(id) = root.node else { panic!("no let"); };
        let binding = arena.bindings(arena.let_node(id).bindings).iter().find(|binding| matches!(arena.pattern(binding.pattern), flow_check::term_arena::PatternNode::Name(n) if root.name(n) == name)).unwrap_or_else(|| panic!("{name}"));
        ty::to_string(root.child(binding.term).ty())
    };
    assert_eq!(ty_of("r"), "rec{a:int,b:rec{c:vec3}}");
    assert_eq!(ty_of("s"), "rec{a:int,b:rec{c:vec3},d:int}");
    assert!(ty_of("l") == "list:float" && ty_of("e") == "list:any" && ty_of("t2") == "text" && ty_of("n") == "float");
    bad_text(&val("(let* [r {:a 1}] r.b)"), "E_FIELD", "no field b. Fields: a");
    bad_text(&val("(let* [r {:a 1} s (assoc r :a \"x\")] 1)"), "E_TYPE", "assoc :a is int; got text");
    bad_text(&val("(count (list 1 \"a\"))"), "E_TYPE", "share one type");
    bad_text(&val("(count (filter (fn [x] \"no\") (list 1)))"), "E_TYPE", "predicate returns bool");
    bad_text(&val("(reduce (fn [a x] \"s\") 0 (list 1))"), "E_TYPE", "accumulator type");
    bad_text(&val("(sort-by (fn [x] \"k\") (list 1))"), "E_TYPE", "key returns a number");
    bad_text(&val("(let* [f (fn [(a : float)] a)] (if false (f \"x\") 1))"), "E_TYPE", "expected float, got text");
    bad(&val("(map (fn [a] a))"), "E_ARITY");
}

#[test]
fn macros_through_the_workspace() {
    let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))";
    let ws = good(&format!("(workspace w {radial} (graph g :context sop (let* [ring (radial k 12 (sop/box :size (+ 0.1 (* k 0.01))))] ring)))"));
    let root = &ws.graphs[0].body;
    let arena = root.arena();
    let W::Node::Let(id) = root.node else { panic!("radial did not expand"); };
    let bs = arena.bindings(arena.let_node(id).bindings);
    assert_eq!(bs.len(), 1);
    let W::Node::Expanded(id) = root.child(bs[0].term).node else { panic!("radial did not expand"); };
    let expanded = arena.expanded(id);
    assert_eq!(root.name(expanded.name), "radial");
    assert!(matches!(root.child(expanded.body).node, W::Node::Op(_) | W::Node::Call(_)));
    assert!(ws.live.is_empty() && ws.macros.len() == 1);
    bad_text("(workspace w (defmacro m [x] `(+ ~x y)) (graph g :context value (let* [y 1] (m 2))))", "E_MACRO_CAPTURE", "would capture");
    bad("(workspace w (defmacro m [x] `(+ ~x ~(+ 1 2))) (graph g :context value (m 2)))", "E_MACRO_UNQUOTE");
    bad_text("(workspace w (defmacro m [x] `(m ~x)) (graph g :context value (m 2)))", "E_MACRO_DEPTH", "32");
    let nest = format!("{}1{}", "(d ".repeat(12), ")".repeat(12));
    bad_text(&format!("(workspace w (defmacro d [x] `(+ ~x ~x)) (graph g :context value {nest}))"), "E_MACRO_SIZE", "5,000");
    bad(&format!("(workspace w {radial} (graph g :context sop (radial k 2)))"), "E_MACRO_ARITY");
    bad("(workspace w (defmacro m [x] `(+ ~x 1)) (defn m :context value [(x : int)] x) (graph g :context value 1))", "E_NAME");
    bad(&format!("(workspace w {radial} (graph g :context sop (let* [k 1 r (radial k 2 (sop/box))] r)))"), "E_SHADOW");
    good("(workspace w (defmacro twice [x] `(+ ~x ~x)) (graph g :context value (twice 3)))");
    good("(workspace w (defn f :context value [(x : float)] x) (defmacro a [x] `(f ~x)) (defmacro b [x] `(a ~x)) (graph g :context value (b 1)))");
}

#[test]
fn the_other_contexts() {
    good("(workspace w (graph a :context sop (sop/box)) (graph s :context scene (scene/merge (scene/geometry (ref a)) (scene/geometry (ref a) :translate [1 0 0] :name \"b\") (scene/camera :eye [0 2 6]) (scene/light :color \"#ffcc88\"))) (graph x :context world (world/sun (world/sky) :name \"L\")) (graph c :context settings (settings/config :title \"t\" :fps 30)))");
    let scene = |b: &str| format!("(workspace w (graph a :context sop (sop/box)) (graph s :context scene {b}))");
    let world = |b: &str| format!("(workspace w (graph x :context world {b}))");
    let settings = |b: &str| format!("(workspace w (graph c :context settings {b}))");
    bad(&scene("(scene/nothing)"), "E_UNKNOWN_KIND");
    good(&scene("(scene/geometry)"));
    good(&scene("(scene/geometry nil)"));
    bad_text(&scene("(scene/geometry 3)"), "E_TYPE", "takes geometry");
    bad(&scene("(scene/geometry (ref a) :nothing 1)"), "E_UNKNOWN_PARAM");
    bad(&scene("(scene/geometry (ref a) :translate \"a\")"), "E_TYPE");
    bad(&scene("(scene/geometry (ref a) :visible \"x\")"), "E_TYPE");
    bad(&scene("(scene/light :shape \"Cube\")"), "E_TYPE");
    bad(&scene("(scene/camera :fov 200)"), "E_HARD_RANGE");
    bad(&scene("(scene/merge (ref a))"), "E_TYPE");
    bad(&scene("(world/sky)"), "E_WRONG_CONTEXT");
    bad(&world("(world/sky (sop/box))"), "E_WRONG_CONTEXT");
    bad_text(&world("(world/sun 3)"), "E_TYPE", "takes world");
    bad(&world("(world/sky :turbidity -1)"), "E_HARD_RANGE");
    bad(&world("(scene/light)"), "E_WRONG_CONTEXT");
    bad(&world("(sop/box)"), "E_WRONG_CONTEXT");
    bad(&settings("(settings/config :fps 500)"), "E_HARD_RANGE");
    bad(&settings("(settings/config :nothing 1)"), "E_UNKNOWN_PARAM");
    bad(&settings("(settings/config :title 3)"), "E_TYPE");
    bad(&settings("(world/sky)"), "E_WRONG_CONTEXT");
    bad("(workspace w (graph a :context sop (sop/box)) (graph s :context scene (ref a)))", "E_TYPE");
    bad(&world("(scene/merge)"), "E_WRONG_CONTEXT");
    bad_text("(workspace w (graph e :context editor (ui/workspace (ui/split \"diagonal\" (ui/graph) (ui/lisp)))))", "E_RANGE", "axis");
    bad_text("(workspace w (graph e :context editor (ui/workspace (ui/split-at \"vertical\" 0.95 (ui/graph) (ui/lisp)))))", "E_RANGE", "ratio");
    bad_text("(workspace w (graph e :context editor (ui/workspace (ui/graph :wires \"straight\"))))", "E_UNKNOWN_PARAM", "wires");
}

#[test]
fn ir_shapes_notes_and_reporting() {
    let ws = good("(workspace w\n  ; a helper\n  (defn f :context value [(x : float 2.0) (y : float 1.0)] (+ x y))\n  (graph g :context value [(n : int 3)]\n    (let* [; loop\n           s (sum [i (range n)] (f :y i))\n           l (map (fn [k] (* k 2)) (range 3))\n           c (cond (< s 1) 1 :else 2)\n           k (case n 1 \"a\" :else \"b\")]\n      (+ s (+ (count l) c)))))");
    assert_eq!(ws.defs[0].form.notes(), vec!["a helper".to_owned()]);
    let root = &ws.graphs[0].body;
    let arena = root.arena();
    let W::Node::Let(id) = root.node else { panic!("unexpected IR"); };
    let bs = arena.bindings(arena.let_node(id).bindings);
    assert_eq!(bs.len(), 4);
    let (s, l, c, k) = (root.child(bs[0].term), root.child(bs[1].term), root.child(bs[2].term), root.child(bs[3].term));
    let W::Node::Loop(id) = s.node else { panic!("sum is not a Loop"); };
    let node = arena.loop_node(id);
    let clauses = arena.bindings(node.clauses);
    assert!(node.kind == W::LoopKind::Sum && arena.bindings(node.accs).is_empty() && clauses.len() == 1);
    assert!(matches!(arena.pattern(clauses[0].pattern), flow_check::term_arena::PatternNode::Name(n) if root.name(n) == "i"));
    assert_eq!(arena.path(node.zone).iter().map(|n| root.name(*n).to_owned()).collect::<Vec<_>>(), p(&["g", "s"]));
    assert!(matches!(root.child(node.body).node, W::Node::CallFn(id) if root.name(arena.call_fn(id).name) == "f" && arena.term_ids(arena.call_fn(id).args).len() == 2));
    let W::Node::Hof(id) = l.node else { panic!("map is not a Hof"); };
    let node = arena.hof(id);
    let ts = arena.term_ids(node.args);
    assert!(node.kind == W::HofKind::Map && ts.len() == 2);
    let W::Node::Fn(id) = root.child(ts[0]).node else { panic!("map function missing"); };
    let function = arena.function(id);
    assert_eq!(arena.path(function.zone).iter().map(|n| root.name(*n).to_owned()).collect::<Vec<_>>(), p(&["g", "l#0"]));
    let params = arena.params(function.params);
    assert!(params.len() == 1 && matches!(arena.pattern(params[0].pattern), flow_check::term_arena::PatternNode::Name(n) if root.name(n) == "k") && params[0].ty.is_none());
    assert!(matches!(c.node, W::Node::Cond(id) if arena.arms(arena.cond(id).arms).len() == 1));
    assert!(matches!(k.node, W::Node::Case(id) if arena.case_arms(arena.case(id).arms).len() == 1));
    assert!(l.ty() == &Ty::List(Box::new(Ty::Int)) && k.ty() == &Ty::Text && s.ty() == &Ty::Float);
    match run_src("(workspace w (defn f :context value [(x : float)] (+ x \"a\")) (graph g :context value (+ (f 1) (+ (f 2) (f 3)))))") {
        (None, ds) => assert_eq!(errors(&ds).len(), 1),
        (Some(_), _) => panic!("accepted"),
    }
    match run_src(&val("(let* [a 1] b)")) {
        (_, ds) if ds.len() == 1 => { let span = ds[0].span.clone().expect("no span"); assert!(span.start > 0 && span.finish > span.start); }
        (_, ds) => panic!("no span: {}", show_ds(&ds)),
    }
    match run_src(&sop("(sop/blast (sop/box) :group \"nobody\")")) {
        (Some(_), ds) if ds.len() == 1 && ds[0].severity == Severity::Warning => (),
        other => panic!("warning blocked the workspace: {:?}", other.1),
    }
}

// ---- the check.cjs suite, static half ----

fn printed(forms: &[Syntax]) -> String { flow_syntax::lisp::print(forms).unwrap().0 }
fn one(source: &str) -> Syntax { let mut f = parse(source); assert_eq!(f.len(), 1, "expected one form"); f.remove(0) }
fn workspace_children(source: &str) -> Vec<Syntax> {
    let form = one(source);
    match form.node { SN::List(xs) if form.range(xs).len() >= 2 => form.range(xs)[2..].iter().map(|&id| form.child(id)).collect(), _ => panic!("not a workspace") }
}
fn head_is(name: &str, x: &Syntax) -> bool { syntax::head(x) == Some(name) }
fn macros_of(source: &str) -> Vec<Syntax> { workspace_children(source).into_iter().filter(|x| head_is("defmacro", x)).collect() }

#[test]
fn case_studies_round_trip() {
    for file in case_files() {
        let source = std::fs::read_to_string(&file).unwrap();
        let text = printed(&parse(&source));
        good(&text);
        assert_eq!(printed(&parse(&text)), text, "canonical print is not stable for {}", file.display());
    }
}

#[test]
fn check_cjs_first_block() {
    bad_text(&sop("(let* [a 1 b (for [a (range 3)] (sop/box))] (sop/merge b))"), "E_SHADOW", "shadows");
    bad_text(&sop("(fold [g (sop/box)] [i (range 3)] 1.0)"), "E_ACC_TYPE", "accumulator type");
    bad_text(&sop("(sop/merge (for [i 3] (sop/box)))"), "E_TYPE", "iterates a list");
    bad_text(&sop("(sop/merge (for [i (range 5000)] (sop/box)))"), "E_ITER_BOUND", "4,096");
    bad_text("(workspace w (defn f :context sop [(x : float)] (f x)) (graph g :context sop (f 1)))", "E_RECURSION", "Use fold");
    bad_text(&sop("(if (< 1 2) (sop/box) 3)"), "E_TYPE", "Both branches");
    bad_text(&sop("(let* [a (for [i (range 2)] (sop/box))] (sop/transform (sop/merge a) :rotate [0 0 i]))"), "E_UNBOUND", "not bound");
    bad_text("(workspace w (graph a :context value [(n : int 2)] n) (graph b :context value (ref a :m 1)))", "E_UNKNOWN_PARAM", "no input :m");
    bad_text("(workspace w (graph a :context value [(n : int)] n))", "E_INPUT_DEFAULT", "needs a default");
    let text = printed(&parse("(workspace w (graph g :context value 2.0))"));
    assert!(text.contains("2.0"));
}

#[test]
fn check_cjs_function_values() {
    let escapes = "cannot be stored or returned";
    bad_text(&val("(let* [f (fn [a b] a)] (f 1))"), "E_ARITY", "takes 2 arguments; got 1");
    bad_text(&val("(let* [f (fn [(a : float)] a)] (if false (f \"x\") 1))"), "E_TYPE", "expected float, got text");
    bad_text(&val("(let* [f (fn [x] (f x))] (f 1))"), "E_UNKNOWN_KIND", "Unknown operator");
    bad_text(&val("(let* [f (fn [x] x)] f)"), "E_FN_ESCAPES", escapes);
    bad_text(&val("(let* [f (fn [x] x) l (list f)] 1)"), "E_FN_ESCAPES", escapes);
    bad_text(&val("(let* [f (fn [x] x) r {:f f}] 1)"), "E_FN_ESCAPES", escapes);
    bad_text(&val("(let* [f (fn [x] x)] (fold [a f] [i (range 2)] 1))"), "E_FN_ESCAPES", escapes);
    bad_text(&val("(count (map (fn [x] (fn [y] y)) (list 1)))"), "E_FN_ESCAPES", escapes);
    bad_text("(workspace w (graph a :context value [(n : int 2)] n) (graph g :context value (let* [f (fn [x] x)] (ref a :n f))))", "E_FN_ESCAPES", escapes);
    bad_text("(workspace w (defn h :context value [(k : fn)] k) (graph g :context value 1))", "E_FN_ESCAPES", escapes);
    bad_text("(workspace w (graph g :context value [(k : fn)] 1))", "E_FN_ESCAPES", escapes);
    bad_text("(workspace w (defmacro dbl [x] `(+ ~x ~x)) (graph g :context value (count (map dbl (list 1)))))", "E_MACRO_AS_VALUE", "macro is not a function value");
    bad_text("(workspace w (defn app :context value [(f : fn) (x : float)] (f f x)) (graph g :context value (app app 1)))", "E_RECURSION", "Recursive call");
    bad_text(&val("(count (filter (fn [x] \"no\") (list 1)))"), "E_TYPE", "predicate returns bool");
    bad_text(&val("(reduce (fn [a x] \"s\") 0 (list 1))"), "E_TYPE", "accumulator type");
}

#[test]
fn check_cjs_lists_records_branches() {
    bad_text(&val("(count (list 1 \"a\"))"), "E_TYPE", "share one type");
    bad_text(&val("(let* [[a b c] (list 1 2)] a)"), "E_PATTERN", "needs 3 elements; the list has 2");
    bad_text(&val("(let* [{:keys [a c]} {:a 1}] a)"), "E_FIELD", "no field c");
    bad_text(&val("(let* [map 1] map)"), "E_BINDING", "Invalid binding name map");
    bad_text(&val("(let* [[a list] (list 1 2)] a)"), "E_BINDING", "Invalid binding name list");
    bad_text(&val("(let* [r {:a 1} s (assoc r :a \"x\")] 1)"), "E_TYPE", "assoc :a is int; got text");
    bad_text(&val("(let* [r {:a 1}] r.b)"), "E_FIELD", "no field b. Fields: a");
    let tyf = |s: &str| ty::of_string(s).unwrap();
    assert!(ty::fits(&tyf("rec{a:int,b:vec3}"), &tyf("rec{b:vec3}")));
    assert!(!ty::fits(&tyf("rec{b:vec3}"), &tyf("rec{a:int,b:vec3}")));
    assert!(ty::fits(&tyf("list:rec{pos:vec3,size:int}"), &tyf("list:rec{size:float}")));
    let s = "list:rec{pos:vec3,kids:list:rec{n:int}}";
    assert_eq!(ty::to_string(&tyf(s)), s);
    assert_eq!(ty::of_string("rec{a:list:rec{x:float},b:int}"), Some(Ty::Record(vec![("a".into(), Ty::List(Box::new(Ty::Record(vec![("x".into(), Ty::Float)])))), ("b".into(), Ty::Int)])));
    let form = one("{:a 1 :b [1 2 3]}");
    match form.node {
        SN::Map(xs) if form.range(xs).len() == 4 && matches!(form.arena().row(form.range(xs)[3]).node, SN::Vec(v) if form.range(v).len() == 3) => (),
        other => panic!("not a map: {other:?}"),
    }
    assert_eq!(printed(&parse("{:a 1 :b [1 2 3]}")).trim(), "{:a 1 :b [1 2 3]}");
    bad_text(&val("(cond (< 2 1) 1)"), "E_NO_ELSE", "final :else");
    bad_text(&val("(cond true 1 :else \"a\")"), "E_TYPE", "one type");
    bad_text(&val("(case 1 1 2)"), "E_NO_ELSE", "final :else");
    bad_text(&val("(let* [k 1] (case 1 k 2 :else 0))"), "E_CASE", "literal");
}

#[test]
fn check_cjs_macros() {
    let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate (* (/ ~i ~n) 360)))))";
    let swap = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))";
    let ms = macros_of(&format!("(workspace w {swap} (graph g :context value 1))"));
    let call = one("(+ (add1 1) (add1 (add1 2)))");
    let a = printed(&[macros::expand(&ms, &call).unwrap()]);
    assert_eq!(a, printed(&[macros::expand(&ms, &call).unwrap()]));
    assert_eq!(a.trim(), "(+ (let* [t__1 1] (+ t__1 1)) (let* [t__2 (let* [t__3 2] (+ t__3 1))] (+ t__2 1)))");
    let ms = macros_of(&format!("(workspace w {swap} {radial} (graph g :context value 1))"));
    let mut st = macros::state();
    let x = one("(radial k 2 (add1 1))");
    let s1 = macros::expand_once_with_state(&ms, &x, &mut st).unwrap();
    assert_eq!(printed(&[s1.clone()]).trim(), "(sop/merge (for [k (range 2)] (sop/transform (add1 1) :rotate (* (/ k 2) 360))))");
    let s2 = macros::expand_once_with_state(&ms, &s1, &mut st).unwrap();
    assert_eq!(macros::expand_once_with_state(&ms, &s2, &mut st).unwrap(), s2);
    assert_eq!(printed(&[s2]), printed(&[macros::expand(&ms, &x).unwrap()]));
    bad_text("(workspace w (defmacro m [x] `(+ ~x y)) (graph g :context value (let* [y 1] (m 2))))", "E_MACRO_CAPTURE", "would capture a name from the call site");
    bad_text("(workspace w (defmacro m [x] `(let* [tmp ~x] tmp)) (graph g :context value (m 2)))", "E_MACRO_CAPTURE", "would capture");
    bad_text("(workspace w (defmacro m [x] `(+ ~x ~(+ 1 2))) (graph g :context value (m 2)))", "E_MACRO_UNQUOTE", "macros unquote only their parameters");
    bad_text("(workspace w (defmacro m [x] `(m ~x)) (graph g :context value (m 2)))", "E_MACRO_DEPTH", "32 nested expansions");
    let nest = format!("{}1{}", "(d ".repeat(12), ")".repeat(12));
    bad_text(&format!("(workspace w (defmacro d [x] `(+ ~x ~x)) (graph g :context value {nest}))"), "E_MACRO_SIZE", "5,000 forms");
    bad_text(&format!("(workspace w {radial} (graph g :context sop (radial k 2)))"), "E_MACRO_ARITY", "expects 3 arguments; got 2");
}

#[test]
fn check_cjs_notes_and_metadata() {
    let noted = "; lead\n(workspace w\n  ; a helper\n  (defn f :context value [(x : float)] (* x 2))\n  (graph g :context value\n    (let* [; first\n           a 1\n           ; the pattern\n           [b c] (list 1 2)\n           q (f a)]\n      ; result note\n      (+ a (+ b (* c q)))))\n  ; tail\n  )";
    let ws = one(noted);
    assert!(ws.notes() == vec!["lead".to_owned()] && ws.tail() == vec!["tail".to_owned()]);
    let defn = workspace_children(noted).into_iter().find(|x| head_is("defn", x)).unwrap();
    assert_eq!(defn.notes(), vec!["a helper".to_owned()]);
    let graph = workspace_children(noted).into_iter().find(|x| head_is("graph", x)).unwrap();
    assert!(matches!(graph.node, SN::List(_)) && graph.children().len() == 5);
    let body = graph.child(graph.children()[4]);
    assert!(matches!(body.node, SN::List(_)) && body.children().len() == 3);
    let bindings = body.child(body.children()[1]);
    assert!(matches!(bindings.node, SN::Vec(_)) && bindings.children().len() >= 3);
    assert_eq!(bindings.child(bindings.children()[0]).notes(), ["first"]);
    assert_eq!(bindings.child(bindings.children()[2]).notes(), ["the pattern"]);
    assert_eq!(body.child(body.children()[2]).notes(), ["result note"]);
    let text = printed(&parse(noted));
    assert!(text.contains("  ; a helper\n  (defn f") && text.contains("; the pattern\n           [b c]"));
    assert_eq!(printed(&parse(&text)), text);
    // setNote edits and removes notes
    fn set_notes(key: &str, notes: &[String], y: &Syntax) -> Syntax {
        fn rewrite(key: &str, notes: &[String], y: &Syntax, builder: &mut syntax::SyntaxBuilder) -> syntax::SyntaxId {
            let out = match y.node {
                SN::List(_) | SN::Vec(_) | SN::Map(_) => {
                    let children = y.children().iter().map(|&id| rewrite(key, notes, &y.child(id), builder)).collect();
                    let node = match y.node { SN::List(_) => syntax::Draft::List(children), SN::Vec(_) => syntax::Draft::Vec(children), _ => syntax::Draft::Map(children) };
                    builder.with_node(y, node)
                }
                _ => builder.import(y),
            };
            if matches!(y.node, SN::Sym(k) if y.name(k) == key) {
                let mut trivia = y.trivia().cloned().unwrap_or_default();
                trivia.notes = notes.to_vec();
                builder.with_trivia(out, Some(trivia))
            } else { out }
        }
        let mut builder = syntax::SyntaxBuilder::new();
        let root = rewrite(key, notes, y, &mut builder);
        builder.freeze().handle(root)
    }
    let x = one(&val("(let* [a 1 b 2] b)"));
    let edited = printed(&[set_notes("b", &["two".into(), "lines".into()], &x)]);
    assert!(edited.contains("; two\n") && edited.contains("; lines\n"));
    assert!(!printed(&[set_notes("b", &[], &set_notes("b", &["two".into()], &x))]).contains(';'));
    let x = one(&sop("(let* [a (sop/box) b ^:bypass (sop/subdivide a)] b)"));
    let (y, _) = syntax::renumber(1000, &x);
    assert!(printed(&[y]).contains("b ^:bypass (sop/subdivide a)"));
    bad_text(&sop("(sop/points ^:bypass (sop/point_list (sop/box)))"), "E_BYPASS", "can't bypass sop/point_list");
    bad_text(&sop("^:bypass (sop/box :size 2)"), "E_BYPASS", "can't bypass");
    bad_text(&sop("^:mute (sop/box)"), "E_META", "only metadata is ^:bypass");
}

#[test]
fn check_cjs_reserved_names_skip_and_t() {
    for k in ["fn", "cond", "case", "list", "values", "quote", "quasiquote", "unquote", "get", "assoc", "str", "map", "filter", "reduce", "sort-by", "concat"] {
        bad_text(&val(&format!("(let* [{k} 1] 1)")), "E_BINDING", "Invalid binding name");
    }
    bad(&sop("(sop/merge (for [i (range (+ 3 (floor (* 2 (sin t)))))] (sop/box)))"), "E_TIME_COUNT");
    bad(&sop("(if (> (sin t) 0.5) (sop/box) (sop/uv_sphere))"), "E_TIME_BRANCH");
    good(&sop("(sop/box :size (if (> (sin t) 0.5) 1 0.5))"));
    bad_text(&sop("(let* [t 1] (sop/box))"), "E_BINDING", "context time");
    good(&val("(let* [z (for [i (range 4)] :skip [1 [3]] i)] 1)"));
    good(&val("(let* [z (for [i (range 2) j (range 2)] :skip [[0 1] [1 0]] i)] 1)"));
    good("(workspace w (graph s :context scene (scene/merge (scene/light) (scene/light) :skip [[1]])))");
    bad(&val("(let* [z (for [i (range 4)] :skip 3 i)] 1)"), "E_SKIP");
    bad(&val("(let* [z (for [i (range 4)] :skip [-1] i)] 1)"), "E_SKIP");
    bad(&val("(let* [z (for [i (range 4)] :skip [1.5] i)] 1)"), "E_SKIP");
    bad(&val("(let* [z (for [i (range 4)] :skip [[0 x]] i)] 1)"), "E_SKIP");
    bad("(workspace w (graph s :context scene (scene/merge (scene/light) :skip [a])))", "E_SKIP");
    bad(&val("(sum [i (range 4)] :skip [1] i)"), "E_ZONE");
    bad(&val("(fold [a 0] [i (range 3)] :skip [1] a)"), "E_ZONE");
    bad(&sop("(sop/merge (for [i (range (+ 3 (floor (* 2 (sin t)))))] :skip [0] (sop/box)))"), "E_TIME_COUNT");
    let ws = good(&sop("(sop/merge (for [i (range 4)] :skip [2] (sop/box)))"));
    assert!(ws.live.is_empty());
}

#[test]
fn audit_language_cases() {
    let ws = good(&sop("(sop/merge (sop/merge (for [i (range 2)] (sop/box))) (sop/merge (for [j (range 3)] (sop/box))))"));
    fn walk(t: &W::Term, zones: &mut Vec<Vec<String>>) {
        let arena = t.arena();
        match t.node {
            W::Node::Loop(id) => { let node = arena.loop_node(id); zones.push(arena.path(node.zone).iter().map(|n| t.name(*n).to_owned()).collect()); walk(&t.child(node.body), zones); }
            W::Node::Call(id) => for arg in arena.fields(arena.call(id).args) { walk(&t.child(arg.term), zones); },
            W::Node::Op(id) => for arg in arena.fields(arena.op(id).args) { walk(&t.child(arg.term), zones); },
            _ => (),
        }
    }
    let mut zones = Vec::new();
    walk(&ws.graphs[0].body, &mut zones);
    let mut uniq = zones.clone(); uniq.sort(); uniq.dedup();
    assert!(zones.len() == 2 && uniq.len() == 2);
    good(&sop("(let* [a (sop/box)] (sop/transform :translate [1 2 3] a))"));
    bad_text(&sop("(sop/boolean :left (sop/box) (sop/torus))"), "E_DUPLICATE_PARAM", "left is given twice");
    bad(&val("(count (range (if false 3 (list 1 2 3))))"), "E_TYPE");
    bad("(workspace w (defmacro m [x] `(+ count ~x)) (graph g :context value (let* [count 2] (m 1))))", "E_MACRO_CAPTURE");
    good("(workspace w (defmacro m [xs] `(count ~xs)) (graph g :context value (m (list 1 2))))");
    warns("^:nonsense (workspace w (graph g :context value 1))", "W_UNKNOWN_META");
    warns("(workspace w ^:nonsense (graph g :context value 1))", "W_UNKNOWN_META");
    quiet("^:allow-warnings (workspace w (graph g :context value 1))");
    let clauses = (0..6).map(|i| format!("i{i} (range 4096)")).collect::<Vec<_>>().join(" ");
    bad(&val(&format!("(count (for [{clauses}] 1))")), "E_ITER_BOUND");
    bad_text(&val("t.x"), "E_FIELD", "t is a constant");
    bad_text(&sop("(sop/material (sop/box) :color \"h\u{e9}\")"), "E_TYPE", "\"h\u{e9}\"");
    bad(&sop("(sop/merge (for [i (range 2)] :skip [[0 x]] (sop/box)))"), "E_SKIP");
}

#[test]
fn workspace_candidates() {
    let candidate = |source: &str| val(&format!("(let* [candidate {source}] 0)"));
    let is_candidate = |ws: &W::Workspace, path: &[&str]| match ws.packed.iter().find(|(p, _)| *p == self::p(path)) {
        Some((_, W::PackedForm::Candidate(producers))) => !producers.is_empty(),
        _ => false,
    };
    let approximable = |source: &str| {
        let ws = good(&candidate(source));
        assert!(ws.approx.is_empty());
        is_candidate(&ws, &["g", "candidate"])
    };
    assert!(approximable("(map (fn [(x : float)] (if (> x 0) (sin x) (cos x))) (array/float 4))"));
    assert!(approximable("(for [x (array/float 4)] (* x 2.0))"));
    assert!(approximable("(map (fn [x] (+ x (frame/dt))) (array/float 4))"));
    assert!(!approximable("(for [x (array/float 4) y (array/float 4)] (+ x y))"));
    assert!(approximable("(for [x (array/float 4)] :skip [0] (+ x 1.0))"));
    assert!(!approximable("(fold [a 0.0] [x (array/float 4)] (+ a x))"));
    assert!(!approximable("(map (fn [x] (sin x)) (list 1 2 3))"));
    assert!(!approximable("(filter (fn [x] (> x 0)) (array/float 4))"));
    assert!(approximable("(map (fn [x] (floor x)) (array/float 4))"));
    assert!(approximable("(map (fn [p] (length p)) (array/vec3 4))"));
    bad_text(&candidate("(map (fn [x] (length x)) (array/float 4))"), "E_TYPE", "length needs a vec3");
    assert!(!approximable("(map (fn [(x : int)] (+ x 1)) (array/float 4))"));
    assert!(approximable("(let* [f (fn [(x : float)] (sqrt (abs x))) xs (map f (array/float 4))] xs)"));
    assert!(!approximable("(let* [xs (map sin (array/float 4))] (exact xs))"));
    let ws = good("(workspace w (defn wave :context value [(x : float)] (sin x))\n    (graph g :context value (let* [candidate (map wave (array/float 4))] 0)))");
    assert!(is_candidate(&ws, &["g", "candidate"]));
    let range = param::FloatRange { soft_min: 0., soft_max: 1., hard_min: None, hard_max: None };
    let opaque = Kind { qualified: "value/opaque".into(), context: context::value(), slots: vec![], parameters: vec![Parameter { name: "value".into(), label: "Value".into(), ty: Some(PortType::Float), fields: vec![("value".into(), param::KindView::Floating(range), param::Value::Float(0.))], folder: vec![], primary: false, unit: None }], outputs: vec![("out".into(), PortType::Float)], facts: None };
    let mut kinds = vec![opaque]; kinds.extend(base_catalog().kinds.clone());
    let c = Catalog { kinds, ..base_catalog().clone() };
    let is = |source: &str| is_candidate(&good_in(&c, source), &["g", "candidate"]);
    assert!(is(&candidate("(map (fn [(p : vec3)] (noise3 p :seed 7 :octaves 3)) (array/vec3 4))")));
    assert!(is("(workspace w (graph g :context value [(seed : int 7)]\n    (let* [candidate (map (fn [(p : vec3)] (noise3 p :seed seed)) (array/vec3 4))] 0)))"));
    assert!(is(&candidate("(map (fn [(p : vec3)] (noise3 p :octaves 99)) (array/vec3 4))")));
    assert!(is(&candidate("(map (fn [x] (value/opaque :value x)) (array/float 4))")));
    for name in flow_syntax::packed_ops::names() {
        assert!(flow_check::op::find(&[], &name, &context::value()).is_some() || flow_syntax::packed_ops::noise_names().contains(&name), "{name}");
    }
    assert!(!flow_syntax::packed_ops::supports("exact"));
}

#[test]
fn workspace_precision() {
    let producer = "(map (fn [x] (+ x 1.0)) (array/float 4))";
    let checks = |body: &str| format!("(let* [producer {producer} alias producer] {body})");
    for source in [
        sop(&checks("(sop/box :size (array/sum alias))")),
        val(&checks("(state [s alias] s)")),
        format!("(workspace w (graph consumer :context value [(xs : float 0.0)] 0)\n       (graph g :context value {}))", checks("(ref consumer :xs (array/sum alias))")),
    ] { bad_text(&source, "E_APPROX_SINK", "Producer: g/producer; sink: g/@result"); }
    for (context, body) in [("settings", "(settings/config :width (count alias))"), ("scene", "(scene/camera :fov (array/sum alias))")] {
        bad_text(&format!("(workspace w (graph g :context {context} {}))", checks(body)), "E_APPROX_SINK", "Producer: g/producer; sink: g/@result");
    }
    good(&sop(&checks("(sop/box :size (array/sum (exact alias)))")));
    let marked = |form: &str| {
        let ws = good(&format!("(workspace w (graph g :context draw (let* [producer {form}] (draw/circles producer))))"));
        matches!(ws.packed.iter().find(|(path, _)| *path == p(&["g", "producer"])), Some((_, W::PackedForm::Candidate(_))))
    };
    let vectors = "(map (fn [x] [x 0 0]) (array/float 4))";
    assert!(marked(&format!("(cond true {vectors} :else {vectors})")));
    assert!(marked(&format!("(case 2 1 {vectors} :else {vectors})")));
    assert!(marked("(map (fn [x] (cond (> x 0) [x 0 0] :else [0 0 0])) (array/float 4))"));
    assert!(marked("(map (fn [x] (case x 0 [0 0 0] :else [x 0 0])) (array/float 4))"));
    assert!(flow_check::op::is_display_kind("draw/circles") && flow_check::op::is_display_kind("ui/canvas") && !flow_check::op::is_display_kind("scene/camera"));
    let kind = Kind { qualified: "value/test_sink".into(), context: context::value(), slots: vec![catalog::Slot { name: "source".into(), required: true, rest: false, ty: Some(Ty::Array(Box::new(Ty::Float))) }], parameters: vec![], outputs: vec![("out".into(), PortType::Float)], facts: None };
    let mut kinds = vec![kind]; kinds.extend(base_catalog().kinds.clone());
    let c = Catalog { kinds, ..base_catalog().clone() };
    let checked = |body: &str| run_in(&c, &val(&checks(body)));
    match checked("(value/test_sink alias)") { (None, ds) => assert!(ds.iter().any(|d| d.code == "E_APPROX_SINK")), _ => panic!() }
    match checked("(value/test_sink (exact alias))") { (Some(_), ds) => assert!(errors(&ds).is_empty()), _ => panic!() }
}

#[test]
fn sketch_checks_match_the_ocaml_oracle() {
    let slow = std::env::var("RAYS_SLOW").is_ok_and(|v| v == "1");
    // The checker's observable output over sketches/*.rays is byte-equal to the OCaml checker's (golden/sketch_checks.txt).
    let out = std::env::var("RAYS_DUMP_CHK").ok();
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches");
    let mut files = std::fs::read_dir(&root).unwrap().map(|e| e.unwrap().path()).filter(|p| p.extension().is_some_and(|e| e == "rays")).collect::<Vec<_>>();
    files.sort_by_key(|p| p.file_name().unwrap().to_string_lossy().to_lowercase());
    let mut text = String::new();
    let path_s = |p: &Vec<String>| p.join("/");
    for file in files {
        if std::env::var("RAYS_DUMP_CHK").is_ok() { eprintln!("FILE {}", file.file_name().unwrap().to_string_lossy()); }
        text.push_str(&format!("FILE {}\n", file.file_name().unwrap().to_string_lossy()));
        let source = std::fs::read(&file).unwrap();
        let source = String::from_utf8_lossy(&source).into_owned();
        match syntax::parse(&source) {
            Err(d) => text.push_str(&format!("PARSE {}\n", rays_diagnostic::to_string(&d))),
            Ok(forms) => {
                let (ws, ds) = W::check(&[], false, false, None, base_catalog(), &forms);
                for d in &ds { text.push_str(&format!("D {}\n", rays_diagnostic::to_string(d))); }
                match ws {
                    None => text.push_str("NONE\n"),
                    Some(w) => {
                        text.push_str(&format!("W {} graphs={} defs={} macros={} nodes={}\n", w.name, w.graphs.len(), w.defs.len(), w.macros.len(), w.nodes.len()));
                        for g in w.graphs.iter() { text.push_str(&format!("G {} {} {}\n", g.name, context::name(g.context), ty::to_string(g.body.ty()))); }
                        for p in &w.live { text.push_str(&format!("L {}\n", path_s(p))); }
                        for p in &w.invariant { text.push_str(&format!("I {}\n", path_s(p))); }
                        for p in &w.approx { text.push_str(&format!("A {}\n", path_s(p))); }
                        let mut packed = w.packed.clone();
                        std::sync::Arc::make_mut(&mut packed).sort_by(|a, b| a.0.cmp(&b.0));
                        for (p, f) in packed.iter() {
                            match f {
                                W::PackedForm::Candidate(ps) => text.push_str(&format!("P {} candidate {}\n", path_s(p), ps.iter().map(path_s).collect::<Vec<_>>().join(","))),
                                W::PackedForm::Refused(rs) => text.push_str(&format!("P {} refused {}\n", path_s(p), rs.len())),
                            }
                        }
                        let heavy = file.file_name().unwrap().to_string_lossy() == "million_marks.rays";
                        if slow || out.is_some() { match if heavy { Err(rays_diagnostic::error("SKIPPED".into(), "heavy".into(), None, None, None, vec![], vec![])) } else { run(&w, false, None, None, None, 0.5) } {
                            Err(d) => text.push_str(&format!("EVAL_ERROR {}\n", rays_diagnostic::to_string(&d))),
                            Ok(e) => {
                                text.push_str(&format!("PLAN instances={} nodes={}\n", e.plan.instances.len(), e.plan.nodes.len()));
                                for (i, n) in e.plan.nodes.iter().enumerate().take(400) { text.push_str(&format!("N {} {} {}\n", i, n.kind, n.site.join("/"))); }
                                for (n, v) in &e.results {
                                    let s = String::from_utf8_lossy(&show(v)).into_owned();
                                    let s = if s.len() > 600 { String::from_utf8_lossy(&s.as_bytes()[..600]).into_owned() } else { s };
                                    text.push_str(&format!("R {n} {s}\n"));
                                }
                            }
                        } }
                    }
                }
            }
        }
    }
    match out {
        Some(out) => std::fs::write(out, text).unwrap(),
        None => {
            let golden = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/golden/sketch_checks.txt")).unwrap();
            let golden = golden.trim_start_matches('\u{feff}').replace("\r\n", "\n");
            let golden = if slow { golden } else {
                golden.lines().filter(|l| !(l.starts_with("PLAN ") || l.starts_with("N ") || l.starts_with("R ") || l.starts_with("EVAL_ERROR"))).map(|l| format!("{l}\n")).collect::<String>()
            };
            assert!(text == golden, "checker output differs from the OCaml oracle in golden/sketch_checks.txt (regenerate with zz_flowcheck_dump/chk.exe)");
        }
    }
}
