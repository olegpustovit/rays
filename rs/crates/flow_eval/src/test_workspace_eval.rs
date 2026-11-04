// Port of lib/flow/test_workspace_eval.ml (part 1: helpers, fixtures, value cases, functions,
// lists, patterns, records). Each OCaml `t "name"` case is one #[test] here.
use crate::*;
use crate::test_workspace::{base_catalog, bad_text, good, parse, run_in, run_src, show_ds, errors};
use flow_check::{op, workspace as W};
use flow_syntax::{syntax::{self, Syntax}, ty};
use std::collections::{BTreeSet, HashSet};
use rays_diagnostic::Diagnostic;

pub fn check(source: &str) -> W::Workspace {
    match run_src(source) { (Some(ws), ds) if errors(&ds).is_empty() => ws, (_, ds) => panic!("{source}\n  rejected: {}", show_ds(&ds)) }
}
pub fn sop(b: &str) -> String { format!("(workspace w (graph g :context sop {b}))") }
pub fn value(b: &str) -> String { format!("(workspace w (graph g :context value {b}))") }
pub fn ok(r: Result<Eval, Diagnostic>) -> Eval { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }
pub fn static_ws(ws: &W::Workspace) -> Eval { ok(static_eval(ws)) }
pub fn static_rec(ws: &W::Workspace) -> Eval { ok(r#static(ws, true, None)) }
pub fn run_ws(ws: &W::Workspace, time: f64) -> Eval { ok(run(ws, false, None, None, None, time)) }
pub fn run_rec(ws: &W::Workspace, time: f64) -> Eval { ok(run(ws, true, None, None, None, time)) }
pub fn result(r: &Eval, name: &str) -> Value { r.results.iter().find(|(n, _)| n == name).unwrap_or_else(|| panic!("no result {name}")).1.clone() }
pub fn g_value_at(src: &str, time: f64) -> Value { result(&run_ws(&check(src), time), "g") }
pub fn g_value(src: &str) -> Value { g_value_at(src, 0.) }

fn num(v: &Value) -> f64 { match v { Value::Int(n) => *n as f64, Value::Float(f) => *f, _ => f64::NAN } }
pub fn same(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int(_) | Value::Float(_), Value::Int(_) | Value::Float(_)) => num(a) == num(b),
        (Value::List(x), Value::List(y)) => x.len() == y.len() && x.iter().zip(y).all(|(u, v)| same(u, v)),
        (Value::Record(x), Value::Record(y)) => x.len() == y.len() && x.iter().zip(y).all(|((k, u), (l, v))| k == l && same(u, v)),
        _ => a == b,
    }
}
pub fn is(src: &str, want: Value) {
    let got = g_value(src);
    assert!(same(&got, &want), "{src}: got {}, want {}", String::from_utf8_lossy(&show(&got)), String::from_utf8_lossy(&show(&want)));
}
pub fn ints(l: &[i64]) -> Value { Value::List(l.iter().map(|n| Value::Int(*n)).collect()) }
pub fn t(s: &str) -> Value { Value::Text(s.as_bytes().to_vec()) }

/// A run-time error: the code, and text in the message.
pub fn err(src: &str, code: &str, text: &str) {
    match static_eval(&check(src)) {
        Err(d) if d.code == code && d.message.contains(text) => (),
        Err(d) => panic!("{src}\n  expected {code} {text:?}, got {}", rays_diagnostic::to_string(&d)),
        Ok(_) => panic!("{src}\n  ran; expected {code}"),
    }
}
pub fn records(src: &str, time: f64) -> Records { run_rec(&check(src), time).records }
pub fn recs<'a>(rs: &'a Records, path: &[&str]) -> &'a Vec<(Vec<i64>, Value)> {
    let p: Vec<String> = path.iter().map(|s| s.to_string()).collect();
    &rs.iter().find(|(q, _)| *q == p).unwrap_or_else(|| panic!("no records at {}", path.join("/"))).1
}
pub fn first_at(rs: &Records, path: &[&str]) -> Value { recs(rs, path)[0].1.clone() }
pub fn bound(src: &str, name: &str) -> Value { first_at(&records(src, 0.), &["g", name]) }
pub fn nodes_of<'a>(r: &'a Eval, kind: &str) -> Vec<&'a Node> { r.plan.nodes.iter().filter(|n| n.kind == kind).collect() }
pub fn count_of(r: &Eval, kind: &str) -> usize { nodes_of(r, kind).len() }
fn p(parts: &[&str]) -> Vec<String> { parts.iter().map(|s| s.to_string()).collect() }

fn walk(f: &mut dyn FnMut(&W::Term), x: &W::Term) {
    f(x);
    // The declared function body, not the call-site specializations retained for evaluation.
    let children: Vec<W::Term> = match &x.node { W::Node::CallFn { args, .. } => args.clone(), W::Node::Fn { body, .. } => vec![(**body).clone()], _ => W::children(x) };
    for c in &children { walk(f, c); }
}
pub fn ty_at(ws: &W::Workspace, path: &[&str]) -> String {
    let want = Some(p(path));
    let mut found = None;
    for g in ws.graphs.iter().chain(&ws.defs) { walk(&mut |x| { if x.path == want { found = Some(ty::to_string(&x.ty)); } }, &g.body); }
    found.unwrap_or_else(|| panic!("no term at {}", path.join("/")))
}

pub fn cases_dir() -> std::path::PathBuf { std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../specification/workspace/cases") }
pub fn read_case(name: &str) -> String { std::fs::read_to_string(cases_dir().join(name)).unwrap() }
fn fixtures() -> Vec<(String, String)> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..");
    let mut v = vec![("bloom".to_owned(), read_case("bloom.lisp")), ("variations".to_owned(), read_case("variations.lisp"))];
    for name in ["facade", "garland", "kit", "orrery", "rosette", "sunflower", "tiles", "tree", "tunnel", "wave"] {
        v.push((name.to_owned(), std::fs::read_to_string(root.join("sketches").join(format!("ws_{name}.rays"))).unwrap()));
    }
    v
}

#[test]
fn the_fixtures_run_deterministically_with_unique_plan_keys() {
    for (name, source) in fixtures() {
        let ws = check(&source);
        let (a, b) = (run_ws(&ws, 0.), run_ws(&ws, 0.));
        assert!(a.plan.nodes.len() == b.plan.nodes.len() && a.authored == b.authored && a.results.len() == b.results.len(), "{name}: two runs differ");
        assert_eq!(a.authored.len(), a.plan.nodes.len());
        let mut forms = HashSet::new();
        fn remember(f: &Syntax, forms: &mut HashSet<i64>) { forms.insert(f.id as i64); for c in syntax::children(f) { remember(&c, forms); } }
        for f in &ws.source { remember(f, &mut forms); }
        assert!(a.authored.iter().all(|id| *id == 0 || forms.contains(id)), "{name}");
        assert_eq!(a.authored, static_ws(&ws).authored, "{name}");
        let keys = a.plan.nodes.iter().map(|n| (n.inst, n.site.clone(), n.iter.clone())).collect::<Vec<_>>();
        assert_eq!(keys.iter().collect::<BTreeSet<_>>().len(), keys.len(), "{name}: plan keys collide");
        // structure never depends on t: only argument values change
        let c = run_ws(&ws, 3.1);
        let shape = |r: &Eval| r.plan.nodes.iter().map(|n| (n.kind.clone(), n.site.clone(), n.iter.clone())).collect::<Vec<_>>();
        assert_eq!(shape(&a), shape(&c), "{name}: structure changed with t");
        assert!(!(a.plan.nodes.is_empty() && name != "kit" && name != "variations" && name != "tiles"), "{name}: no nodes");
    }
}

#[test]
fn nested_data_and_lazy_selection() {
    let host = |body: &str| format!("(workspace w (graph g :context host {body}))");
    let encode = op::find(&[], "text/table_encode", &flow_syntax::context::value()).unwrap();
    let mut consume = encode.clone();
    consume.name = "test/table_record".into();
    consume.signature.pos = vec![("data".into(), Ty::Record(vec![("rows".into(), Ty::List(Box::new(Ty::List(Box::new(Ty::Text))))), ("label".into(), Ty::Text)]))];
    let inner = encode.body.clone();
    consume.body = std::sync::Arc::new(move |live, node, args| {
        match &args.iter().find(|(n, _)| n == "data").unwrap().1 {
            Value::Record(fields) => {
                let label = fields.iter().find(|(n, _)| n == "label").unwrap().1.clone();
                let rows = fields.iter().find(|(n, _)| n == "rows").unwrap().1.clone();
                match (label, inner(live, node, &[("rows".to_owned(), rows)])?) {
                    (Value::Text(mut l), Value::Text(table)) => { l.extend(table); Ok(Value::Text(l)) }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    });
    let forms = parse(&host("(test/table_record {:rows (list (list (str t) \"猫\")) :label (str t)})"));
    let workspace = match W::check(std::slice::from_ref(&consume), false, false, None, base_catalog(), &forms) { (Some(w), ds) if ds.is_empty() => w, (_, ds) => panic!("{}", show_ds(&ds)) };
    assert_eq!(result(&run_ws(&workspace, 0.25), "g"), t("0.250.25\t猫"));
    assert_eq!(g_value_at(&host("(text/table_encode (list (list (str t) \"猫\")))"), 0.25), t("0.25\t猫"));
    for body in ["(first (list t (array/nth (array/float 0) (floor t))))", "(nth (list t (array/nth (array/float 0) (floor t))) 0)", "(get {:selected t :unused (array/nth (array/float 0) (floor t))} :selected)"] {
        assert_eq!(g_value_at(&value(body), 0.25), Value::Float(0.25), "{body}");
    }
}

#[test]
fn value_cases() {
    is(&value("(sum [k (range 4)] (* k 2))"), Value::Int(12));
    is(&value("(int -2.7)"), Value::Int(-2));
    is(&value("(round 2.5)"), Value::Int(3));
    is(&value("(round -2.5)"), Value::Int(-3));
    is(&value("(ceil 2.1)"), Value::Int(3));
    is(&value("(float 3)"), Value::Float(3.));
    is(&value("(/ (float 3) 2)"), Value::Float(1.5));
    is(&value("(sum [k (range 0)] 1.5)"), Value::Int(0));
    is(&value("(fold [a 1] [i (range 5)] (* a 2))"), Value::Int(32));
    is(&value("(fold [a 7] [i (range 0)] (* a 2))"), Value::Int(7));
    is(&value("(let* [xs (scan [a 1] [i (range 3)] (+ a i))] (count xs))"), Value::Int(3));
    assert!(same(&bound(&value("(let* [z (scan [a 1] [i (range 4)] (* a 2))] 1)"), "z"), &ints(&[2, 4, 8, 16])));
    assert!(same(&bound(&value("(let* [z (scan [a 1] [i (range 0)] (* a 2))] 1)"), "z"), &ints(&[])));
    assert!(same(&bound(&value("(let* [z (for [x (range 2) y (range 3)] (+ (* x 10) y))] (count z))"), "z"), &ints(&[0, 1, 2, 10, 11, 12])));
    is(&value("(let* [z (for [x (range 4) y (range x)] y)] (count z))"), Value::Int(6));
    let ws = check("(workspace w (graph a :context value [(n : int 2)] (* n 3)) (graph b :context value (+ (ref a) (ref a :n 5))))");
    let r = static_ws(&ws);
    assert!(same(&result(&r, "b"), &Value::Int(21)));
    // a, b, and a with n=5: an override tuple is one cached instance
    assert_eq!(r.plan.instances.len(), 3);
    match (g_value(&value("(value/rand 3 4 5)")), g_value(&value("(value/rand 3 4 5)"))) {
        (Value::Float(x), Value::Float(y)) => assert!(x == y && (0. ..1.).contains(&x)),
        _ => panic!("not a float"),
    }
    let rs = records(&read_case("bloom.lisp"), 0.);
    let r = recs(&rs, &["flower", "ring", "u"]);
    assert!(r.len() == 12 && r[3].0 == vec![3]);
    let r = run_ws(&check(&read_case("bloom.lisp")), 0.);
    assert!(r.plan.nodes.iter().any(|n| n.iter == vec![5] && n.site[..2] == p(&["flower", "ring"])[..]));
    // the study runs a literal (range 9999) in the untaken arm; the checker rejects that literal, so the count is a graph input
    let src = |b: &str| format!("(workspace w (graph g :context value [(n : int 9999)] {b}))");
    is(&src("(if (< 1 2) 1 (count (range n)))"), Value::Int(1));
    err(&src("(if (> 1 2) 1 (count (range n)))"), "E_ITER_BOUND", "exceeds 4,096");
}

#[test]
fn function_values() {
    is(&value("(let* [f (fn [a b] (+ a b))] (f 1 2))"), Value::Int(3));
    assert_eq!(bound(&value("(let* [f (fn [(a : float) b] (* a b)) r (f 1 2)] 1)"), "r"), Value::Float(2.));
    is(&value("(let* [k 10 f (fn [x] (+ x k))] (f 1))"), Value::Int(11));
    is(&value("(let* [min (fn [a b] (+ a b))] (min 1 2))"), Value::Int(3));
    let src = value("(let* [f (fn [a] (let* [s (* a 2)] (+ s 1))) x (f 5) y (sum [i (range 3)] (f i))] (+ x y))");
    let ws = check(&src);
    let rs = run_rec(&ws, 0.).records;
    let at = |path: &[&str]| recs(&rs, path).clone();
    assert_eq!(at(&["g", "f", "@result"]), vec![(vec![0], Value::Int(11)), (vec![0, 1], Value::Int(1)), (vec![1, 2], Value::Int(3)), (vec![2, 3], Value::Int(5))]);
    assert_eq!(at(&["g", "f", ":a"]).into_iter().map(|x| x.1).collect::<Vec<_>>(), vec![Value::Int(5), Value::Int(0), Value::Int(1), Value::Int(2)]);
    assert_eq!(at(&["g", "f", "s"])[3], (vec![2, 3], Value::Int(4)));
    // the IR types a fn body with its declared types, or Any: call-site refinement (int) is not kept
    assert!(ty_at(&ws, &["g", "f", "s"]) == "float" && ty_at(&ws, &["g", "f"]) == "fn");
    let rs = records(&value("(let* [xs (map (fn [x] (* x 2)) (list 1 2))] (count xs))"), 0.);
    assert_eq!(recs(&rs, &["g", "xs#0", ":x"]).iter().map(|x| x.1.clone()).collect::<Vec<_>>(), vec![Value::Int(1), Value::Int(2)]);
    let ws = check("(workspace w (graph g :context draw (draw/merge (draw/background \"#0b0b0e\") (draw/background \"#ffffff\"))))");
    let evaluated = static_rec(&ws);
    for path in [["g", "@result#0"], ["g", "@result#1"]] {
        assert!(recs(&evaluated.records, &path).iter().any(|(_, v)| matches!(v, Value::Deferred(t, _) if *t == ty::drawing())));
    }
    let ws = check("(workspace w\n      (defmacro panel [color]\n        `(draw/image (image/map (fn [uv#] ~color) :width 2 :height 2)))\n      (graph g :context draw\n        (draw/merge (panel [1 0 0 1]) (panel [0 1 0 1]) (panel [0 0 1 1]))))");
    let evaluated = static_rec(&ws);
    let images = nodes_of(&evaluated, "image/map");
    let sites = images.iter().map(|n| (n.inst, n.site.clone(), n.iter.clone())).collect::<BTreeSet<_>>();
    assert!(images.len() == 3 && sites.len() == 3);
    let ws = check(&sop("(let* [cs (map (fn [x] (sop/box :size x)) (list 1 2))] (sop/merge cs))"));
    assert!(ty_at(&ws, &["g", "cs"]) == "list:geometry" && ty_at(&ws, &["g", "cs#0", "@result"]) == "geometry");
    let r = run_ws(&check(&sop("(let* [mk (fn [r] (sop/box :size r)) cs (map mk (list 0.1 0.2 0.3))] (sop/merge cs))")), 0.);
    assert_eq!(nodes_of(&r, "sop/box").iter().map(|n| (n.site.clone(), n.iter.clone())).collect::<Vec<_>>(), (0..3).map(|k| (p(&["g", "mk", "@result"]), vec![k])).collect::<Vec<_>>());
    let r = run_ws(&check(&sop("(let* [unused (list (count (map (fn [x] (sop/box :size x)) (list 0.1))) (count (map (fn [x] (sop/box :size x)) (list 0.2))))] (sop/box))")), 0.);
    let sites = nodes_of(&r, "sop/box").iter().map(|n| (n.site.clone(), n.iter.clone())).collect::<Vec<_>>();
    assert!(sites.len() == 3 && sites.iter().collect::<BTreeSet<_>>().len() == 3);
    is("(workspace w (defn twice :context value [(x : float)] (* x 2)) (graph g :context value (+ (reduce + 0 (map twice (list 1 2 3))) (first (map sin (list 0))))))", Value::Int(12));
    is(&value("(reduce + 0 (list 0.5 0.25))"), Value::Float(0.75));
    let ring = "(defn ring :context sop [(n : int 8) (make : fn)] (sop/merge (map make (range n))))";
    let r = run_ws(&check(&format!("(workspace w {ring} (defn mark :context sop [(i : int)] (sop/box :size (* 0.1 (+ i 1)))) (graph g :context sop (sop/merge (ring :n 3 :make mark) (let* [mk (fn [i] (sop/box))] (ring :n 2 :make mk)))))")), 0.);
    assert!(count_of(&r, "sop/box") == 5 && count_of(&r, "sop/merge") == 3);
    // the two ring call sites are distinct subtrees; mk's two calls are iterations 0 and 1
    let mut iters = nodes_of(&r, "sop/box").iter().map(|n| n.iter.clone()).collect::<Vec<_>>();
    iters.sort();
    assert_eq!(iters, vec![vec![0], vec![0], vec![1], vec![1], vec![2]]);
    is("(workspace w (defn tot :context value [(xs : (list float)) (r : {:k float})] (* r.k (reduce + 0 xs))) (graph g :context value (tot (list 1 2) {:k 2 :extra 1})))", Value::Int(6));
    assert!(same(&bound(&value("(let* [z (map (fn [a b c] (+ a (+ b c))) (list 1 2 3) (list 10 20) (list 100 200 300))] 1)"), "z"), &ints(&[111, 222])));
    assert!(same(&bound(&value("(let* [z (filter (fn [x] (> x 1)) (list 1 2 3))] 1)"), "z"), &ints(&[2, 3])));
    let l = |a: i64, b: i64| Value::List(vec![Value::Int(a), Value::Int(b)]);
    assert!(same(&bound(&value("(let* [z (sort-by (fn [p] (first p)) (list (list 2 0) (list 1 1) (list 2 2) (list 1 3)))] 1)"), "z"), &Value::List(vec![l(1, 1), l(1, 3), l(2, 0), l(2, 2)])));
    let src = sop("(let* [z (map (fn [x] (sop/box :size x)) (list))] (sop/merge z))");
    assert!(same(&bound(&src, "z"), &ints(&[])));
    assert_eq!(ty_at(&check(&src), &["g", "z"]), "list:geometry");
}

#[test]
fn lists_and_patterns() {
    let src = value("(let* [z (list 1 2.5)] 1)");
    assert_eq!(bound(&src, "z"), Value::List(vec![Value::Float(1.), Value::Float(2.5)]));
    assert_eq!(ty_at(&check(&src), &["g", "z"]), "list:float");
    let src = value("(let* [z (list)] 1)");
    assert!(bound(&src, "z") == Value::List(vec![]) && ty_at(&check(&src), &["g", "z"]) == "list:any");
    assert_eq!(bound(&value("(let* [s (str (first (list 4 5)) (last (list 4 5)) (rest (list 1 2 3)) (nth (list 7 8) 1) (concat (list 1) (list) (list 2 3)) (reverse (list 1 2)) (take 2 (list 1 2 3)) (drop 2 (list 1 2 3)))] 1)"), "s"), t("45[2 3]8[1 2 3][2 1][1 2][3]"));
    err(&value("(nth (list 1 2) 5)"), "E_LIST_RANGE", "index 5 is out of range for a list of length 2");
    err(&value("(nth (list 1 2) -1)"), "E_LIST_RANGE", "");
    err(&value("(first (list))"), "E_LIST_RANGE", "empty list");
    err(&value("(last (list))"), "E_LIST_RANGE", "");
    let src = value("(let* [[a b] (list 1 2)] (+ a b))");
    is(&src, Value::Int(3));
    let rs = records(&src, 0.);
    assert!(same(&first_at(&rs, &["g", "[a b]"]), &ints(&[1, 2])) && first_at(&rs, &["g", "b"]) == Value::Int(2));
    is(&value("(let* [[x y z] [1 2 3] {:keys [a b]} {:a 10 :b 20}] (+ (* x a) (* z b)))"), Value::Int(70));
    let src = value("(let* [f (fn [[a b]] (* a b)) s (sum [[a b] (list (list 1 2) (list 3 4))] (+ (f (list a b)) 0))] s)");
    is(&src, Value::Int(14));
    let rs = records(&src, 0.);
    assert!(same(&recs(&rs, &["g", "s", ":[a b]"])[1].1, &ints(&[3, 4])));
    assert!(recs(&rs, &["g", "s", ":a"]).len() == 2 && recs(&rs, &["g", "s", ":b"]).len() == 2);
    assert_eq!(recs(&rs, &["g", "f", ":[a b]"]).len(), 2);
    let ws = check(&value("(let* [z (for [[a b] (list (list 1 2)) {:keys [c]} (list {:c 1})] a)] 1)"));
    match &ws.graphs[0].body.node {
        W::Node::Let(bs, _) if bs.len() == 1 => match &bs[0].1.node {
            W::Node::Loop { clauses, .. } if clauses.len() == 2 => {
                assert!(matches!(&clauses[0].0, W::Pattern::Seq(ps) if ps.len() == 2 && matches!(&ps[0], W::Pattern::Name(a) if a == "a") && matches!(&ps[1], W::Pattern::Name(b) if b == "b")));
                assert!(matches!(&clauses[1].0, W::Pattern::Keys(ks) if ks == &vec!["c".to_owned()]));
            }
            other => panic!("unexpected clause patterns: {other:?}"),
        },
        other => panic!("unexpected clause patterns: {other:?}"),
    }
}

#[test]
fn records_literal_get_assoc() {
    let src = value("(let* [r {:a 1 :b {:c [1 2 3]}}] (+ r.a r.b.c.y))");
    is(&src, Value::Int(3));
    assert_eq!(ty_at(&check(&src), &["g", "r"]), "rec{a:int,b:rec{c:vec3}}");
    is(&value("(let* [r {:a 1} s (assoc r :a 5 :c 2)] (+ (get s :a) s.c))"), Value::Int(7));
    let _ = (good, bad_text, run_in);
}

fn arg_of<'a>(n: &'a Node, k: &str) -> &'a Value { &n.args.iter().find(|(name, _)| name == k).unwrap_or_else(|| panic!("no arg {k}")).1 }

#[test]
fn records_values_and_accumulators() {
    assert_eq!(bound(&value("(let* [r (values :a 1 :b 2.5)] 1)"), "r"), Value::Record(vec![("a".into(), Value::Int(1)), ("b".into(), Value::Float(2.5))]));
    is(&value("(let* [s (fold [{:keys [a b]} {:a 0 :b 1}] [i (range 10)] {:a b :b (+ a b)})] s.a)"), Value::Int(55));
}

#[test]
fn cond_case_str_macros_bypass() {
    is(&value("(cond (< 2 1) 1 (> 2 1) 2 :else 3)"), Value::Int(2));
    is("(workspace w (graph g :context value [(n : int 9999)] (cond true 1 :else (count (range n)))))", Value::Int(1));
    is(&value("(case 2 1 10 2 20 :else 0)"), Value::Int(20));
    is(&value("(case 9 1 10 :else 0)"), Value::Int(0));
    is(&value("(case \"b\" \"a\" 1 \"b\" 2 :else 0)"), Value::Int(2));
    is(&value("(case true false 1 true 2 :else 0)"), Value::Int(2));
    assert_eq!(bound(&value("(let* [s (str \"a\" 1 2.5 true [1 2 3] 1.23456 (list 1 2) {:a 1} (/ 1 3))] 1)"), "s"), t("a12.5true[1 2 3]1.2346[1 2]{:a 1}0.3333"));
    let r = run_ws(&check(&sop("(sop/blast (sop/group_bounds (sop/box) :name (str \"floor_\" 2)) :group (str \"floor_\" 2))")), 0.);
    assert_eq!(*arg_of(nodes_of(&r, "sop/blast")[0], "group"), t("floor_2"));
    assert_eq!(*arg_of(nodes_of(&r, "sop/group_bounds")[0], "name"), t("floor_2"));
    let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))";
    let r = run_rec(&check(&format!("(workspace w {radial} (graph g :context sop (let* [ring (radial k 12 (sop/box :size (+ 0.1 (* k 0.01))))] ring)))")), 0.);
    assert_eq!(count_of(&r, "sop/transform"), 12);
    assert_eq!(recs(&r.records, &["g", "ring", "~for", ":k"]).len(), 12);
    is("(workspace w (defmacro all [& xs] `(+ 0 (+ ~@xs))) (graph g :context value (all 1 2)))", Value::Int(3));
    is("(workspace w (defmacro twice [x] `(+ ~x ~x)) (graph g :context value (twice 3)))", Value::Int(6));
    let r = run_ws(&check(&sop("(let* [a (sop/box) b ^:bypass (sop/transform a :translate [5 0 0])] b)")), 0.);
    assert!(r.plan.nodes.len() == 1 && result(&r, "g") == Value::Deferred(ty::geometry(), 0));
    // a bypassed value call passes its first input, coerced
    is(&value("(let* [a 2 b ^:bypass (+ a 3)] b)"), Value::Int(2));
    let noted = "; lead\n(workspace w\n  ; a helper\n  (defn f :context value [(x : float)] (* x 2))\n  (graph g :context value\n    (let* [; first\n           a 1\n           ; the pattern\n           [b c] (list 1 2)\n           q (f a)]\n      ; result note\n      (+ a (+ b (* c q)))))\n  ; tail\n  )";
    let printed = flow_syntax::lisp::print(&parse(noted)).0;
    is(&printed, Value::Int(6));
}

#[test]
fn round_trip_of_every_new_construct() {
    let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))";
    let all = format!("(workspace kitchen\n  ; macros\n  {radial}\n  (defmacro all [& xs] `(sop/merge ~@xs))\n  (defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))\n  (defmacro twice [x] `(+ ~x ~x))\n  (defn ring :context sop [(n : int 8) (make : fn) (opts : {{:scale float :tags (list text)}} {{:scale 1.0 :tags (list \"a\")}})]\n    (sop/transform (sop/merge (map make (range n))) :uniform_scale opts.scale))\n  (graph g :context sop [(seed : int 3)]\n    (let* [; a local function\n           petal (fn [(i : int) [w h]]\n                   (sop/transform (sop/box :size [w h 0.1]) :rotate [0 0 (* i 0.5)]))\n           sizes (map (fn [k] (list (+ 0.1 (* k 0.01)) 0.5)) (range 12))\n           petals (map petal (range 12) sizes)\n           {{:keys [a b]}} {{:a 1 :b (twice 2)}}\n           [x y z] [a b 3]\n           kind (cond (< a 1) \"small\"\n                      (< a 5) \"medium\"\n                      :else \"large\")\n           steps (case seed 1 4 2 8 :else 12)\n           order (sort-by (fn [p] (- 0 (nth p 0))) sizes)\n           kept (filter (fn [p] (> (first p) 0.12)) (concat (take 3 sizes) (drop 9 sizes) (reverse (rest sizes))))\n           total (reduce + 0 (map (fn [p] (last p)) kept))\n           name (str \"floor_\" (add1 steps) \"_\" kind)\n           state (fold [{{:keys [n acc]}} (values :n 0 :acc 1.0)]\n                       [i (range steps)]\n                   (assoc {{:n (+ n 1) :acc (* acc 0.9)}} :acc (get {{:acc (* acc 0.9)}} :acc)))\n           ring2 (radial k 6 (sop/box :size (+ 0.1 (* k 0.01))))\n           faded ^:bypass (sop/subdivide ring2 :iterations 2)\n           shapes (all (sop/merge petals) faded (ring :n 3 :make (fn [i] (sop/box :size 0.05))) (sop/group_bounds (sop/box) :name name))\n           ; unused but checked\n           spare (count (list))]\n      ; the result\n      (sop/transform shapes :uniform_scale (* state.acc (* x (+ y (+ z total)))))))\n  ; trailing\n  )");
    let printed = flow_syntax::lisp::print(&parse(&all)).0;
    assert_eq!(flow_syntax::lisp::print(&parse(&printed)).0, printed);
    let ws = check(&printed);
    let r = run_rec(&ws, 0.);
    assert!(!r.plan.nodes.is_empty());
    assert_eq!(ty_at(&ws, &["g", "state"]), "rec{n:int,acc:float}");
    let xyz = first_at(&r.records, &["g", "[x y z]"]);
    assert!(same(&xyz, &Value::Vec3(1., 4., 3.)) || same(&xyz, &ints(&[1, 4, 3])));
}

#[test]
fn live_values_and_the_split_evaluation() {
    let ws = check(&sop("(let* [n 5] (sop/merge (for [i (range n)] (sop/box :size 0.1 :center [(+ i (sin t)) 0 0]))))"));
    let (a, b) = (run_ws(&ws, 0.), run_ws(&ws, 1.2));
    assert!(count_of(&a, "sop/box") == 5 && count_of(&b, "sop/box") == 5);
    let centre = |r: &Eval| arg_of(nodes_of(r, "sop/box")[2], "center").clone();
    assert!(centre(&a) == Value::Vec3(2., 0., 0.) && centre(&b) != centre(&a));
    let ws = check(&std::fs::read_to_string(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../sketches/ws_orrery.rays")).unwrap());
    let n = |time: f64| run_ws(&ws, time).plan.nodes.len();
    assert!(n(0.) == n(3.1) && n(0.) > 0);
}

#[test]
fn residuals_force_by_time() {
    let ws = check(&sop("(let* [a (sop/box) s (* t 2) b (sop/transform a :translate [s 0 0] :uniform_scale 2)] b)"));
    let r = static_ws(&ws);
    let n = nodes_of(&r, "sop/transform")[0];
    match (arg_of(n, "translate"), arg_of(n, "uniform_scale")) {
        (Value::Residual(res), Value::Int(2)) => {
            assert!(is_live(arg_of(n, "translate")));
            assert_eq!(residual_eval(*res, &input::at_time(3.), None), Ok(Value::Vec3(6., 0., 0.)));
            assert_eq!(residual_eval(*res, &input::at_time(0.5), None), Ok(Value::Vec3(1., 0., 0.)));
        }
        _ => panic!("translate is not a residual"),
    }
    assert_eq!(force(arg_of(n, "translate"), &input::at_time(1.), None), Ok(Value::Vec3(2., 0., 0.)));
    assert_eq!(force(&Value::Int(4), &input::at_time(1.), None), Ok(Value::Int(4)));
    let at = |src: &str, time: f64| g_value_at(src, time);
    let live_of = |src: &str| result(&static_ws(&check(src)), "g");
    assert!(is_live(&live_of(&value("(* t 2)"))));
    assert!(!is_live(&live_of(&value("(* 3 2)"))));
    assert!(same(&at(&value("(if (> (sin t) 0.5) 1 0.5)"), 1.6), &Value::Float(1.)));
    assert!(same(&at(&value("(if (> (sin t) 0.5) 1 0.5)"), 0.), &Value::Float(0.5)));
    assert!(same(&at(&value("(fold [a 0] [i (range 3)] (+ a t))"), 2.), &Value::Int(6)));
    assert!(same(&at(&value("(let* [f (fn [x] (+ x t)) a (f 1)] (* a 2))"), 3.), &Value::Float(8.)));
    assert!(same(&at("(workspace w (graph a :context value [(n : int 2)] (* n t)) (graph g :context value (+ (ref a) (ref a :n 5))))", 2.), &Value::Float(14.)));
    // a long chain that reads its predecessor twice is shared, not exponential
    let chain = (0..40).map(|i| format!("a{} (+ a{} a{})", i + 1, i, i)).collect::<Vec<_>>().join(" ");
    assert!(same(&at(&value(&format!("(let* [a0 t {chain}] (* a40 0))")), 1.), &Value::Float(0.)));
    // structure is identical at every time
    let ws = check(&sop("(sop/merge (for [i (range 4)] (sop/box :center [(sin (+ t i)) 0 0])))"));
    let shape = |time: f64| run_ws(&ws, time).plan.nodes.iter().map(|n| (n.kind.clone(), n.site.clone(), n.iter.clone())).collect::<Vec<_>>();
    assert!(shape(0.) == shape(5.3) && shape(0.) == shape(100.));
}

#[test]
fn named_arguments_and_residual_capture() {
    for count in [16usize, 17] {
        let fields: Vec<(String, Ty)> = (0..count).map(|i| (format!("slot{i}"), Ty::Float)).collect();
        let mut operator = op::find(&[], "sin", &flow_syntax::context::value()).unwrap();
        operator.name = "test/slots".into();
        operator.signature = op::Signature { pos: fields.clone(), opt: vec![], rest: None, kw: vec![] };
        operator.out = std::sync::Arc::new(|_| Ok(Ty::Float));
        operator.any_num = false; operator.arithmetic = None;
        let names = fields.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>();
        operator.body = std::sync::Arc::new(move |_, _, args| {
            assert_eq!(args.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(), names);
            let mut sum = 0.;
            for (_, v) in args { sum += flow_syntax::value::num(v)?; }
            Ok(Value::Float(sum))
        });
        let source = value(&format!("(test/slots {})", (0..count).map(|i| format!("{i}.0")).collect::<Vec<_>>().join(" ")));
        let workspace = match W::check(std::slice::from_ref(&operator), false, false, None, base_catalog(), &parse(&source)) { (Some(w), ds) if ds.is_empty() => w, (_, ds) => panic!("{}", show_ds(&ds)) };
        assert_eq!(result(&static_ws(&workspace), "g"), Value::Float((count * (count - 1) / 2) as f64));
    }
    let source = sop(&format!("(sop/merge {})", (0..17).map(|i| format!("(sop/box :center [{i} 0 0])")).collect::<Vec<_>>().join(" ")));
    let evaluation = static_ws(&check(&source));
    let nodes = nodes_of(&evaluation, "sop/box");
    assert_eq!(nodes.len(), 17);
    let keys = nodes.iter().map(|n| (n.inst, n.site.clone(), n.iter.clone())).collect::<BTreeSet<_>>();
    assert_eq!(keys.len(), 17);
    let bindings = (0..20).map(|i| format!("b{i} {i}.0")).collect::<Vec<_>>().join(" ");
    let ws = check(&value(&format!("(let* [{bindings}] (+ (+ b3 b17) t))")));
    let Value::Residual(residual) = result(&static_ws(&ws), "g") else { panic!() };
    assert_eq!(Residual::residual_view(residual).unwrap().bindings.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(), vec!["b17", "b3"]);
    assert_eq!(residual_eval(residual, &input::at_time(2.), None), Ok(Value::Float(22.)));
    let ws = check(&value("(let* [outer 7.0 unused 99.0 mapped (map (fn [x] (+ x (+ outer t))) (array/range 2))] 0.0)"));
    let Value::Residual(residual) = static_rec(&ws).records.iter().find(|(q, _)| *q == p(&["g", "mapped"])).unwrap().1[0].1.clone() else { panic!() };
    assert_eq!(Residual::residual_view(residual).unwrap().bindings.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(), vec!["outer"]);
    assert_eq!(residual_eval(residual, &input::at_time(2.), None), Ok(Value::Float_array(std::sync::Arc::new(vec![9., 10.]))));
    let ws = check(&value("(let* [outside 7.0 unused 8.0] (let* [local (+ outside t) f (fn [x] (+ x (+ local outside))) mapped (map f (list 1.0 2.0))] 0.0))"));
    let W::Node::Let(_, term) = &ws.graphs[0].body.node else { panic!() };
    assert_eq!(Residual::free_names(term), vec!["outside"]);
    let walks = Host::free_name_walks();
    let _ = Residual::free_names(term);
    assert_eq!(Host::free_name_walks(), walks);
    let ws = check(&value("(let* [source (list 3.0 4.0) initial 2.0] (let* [[x y] source total (state [s initial] (+ s t))] (+ x (+ y total))))"));
    let W::Node::Let(_, term) = &ws.graphs[0].body.node else { panic!() };
    assert_eq!(Residual::free_names(term), vec!["initial", "source"]);
}

#[test]
fn hash_vectors() {
    let h = hash;
    assert!(h(&[]) == 0.435769 && h(&[0.]) == 0.009611 && h(&[3., 4., 5.]) == 0.706664);
    assert!(h(&[1.5, -2.]) == 0.622571 && h(&[7., 0., 1.]) == 0.089347 && h(&[1e10]) == 0.736254);
    assert!(h(&[-0.25, 12345.678]) == 0.068138 && h(&[1., 2., 3., 4., 5., 6.]) == 0.659002);
    assert!(h(&[0.000001]) == 0.954845 && h(&[1e300]) == h(&[0.]));
    assert_eq!(g_value(&value("(value/rand 3 4 5)")), Value::Float(0.706664));
    assert_eq!(g_value(&value("(value/rand 1.5 -2)")), Value::Float(0.622571));
    assert_eq!(g_value(&value("(value/rand true)")), Value::Float(hash(&[1.])));
}

#[test]
fn l3_driven_bounds() {
    let src = |b: &str| format!("(workspace w (graph g :context value [(n : int 100)] {b}))");
    match static_with(&check(&src("(count (range n))")), false, Some(&[("g".to_owned(), vec![("n".to_owned(), Value::Int(5000))])]), None) {
        Err(d) => assert!(d.code == "E_ITER_BOUND" && d.message.contains("range 0‥5000 exceeds 4,096"), "{d:?}"),
        Ok(_) => panic!("ran"),
    }
    err(&src("(count (linspace 0 1 (* n 100)))"), "E_ITER_BOUND", "linspace exceeds 4,096");
    is(&src("(count (range 4096))"), Value::Int(4096));
    err(&src("(sum [x (range n) y (range n)] 1)"), "E_ITER_BOUND", "g/@result runs more than 4,096 iterations");
    is("(workspace w (graph g :context value [(n : int 64)] (sum [x (range n) y (range n)] 1)))", Value::Int(4096));
    err("(workspace w (graph g :context value [(n : int 3000)] (count (concat (range n) (range n)))))", "E_ITER_BOUND", "concat exceeds 4,096");
    is("(workspace w (graph g :context value [(n : int 2048)] (count (concat (range n) (range n)))))", Value::Int(4096));
    err("(workspace w (graph g :context value [(n : int 4000)] (sum [i (range n)] (sum [j (range n)] 1))))", "E_EVAL_BUDGET", "600000");
}

#[test]
fn d3_nonfinite_and_panel_checks() {
    err("(workspace w (graph g :context value [(n : int 2)] (let* [[a b c] (take n (list 1 2 3))] a)))", "E_PATTERN", "needs 3 elements; the list has 2");
    is("(workspace w (graph g :context value [(n : int 3)] (let* [[a b c] (take n (list 1 2 3))] c)))", Value::Int(3));
    err(&value("(pow 10 999)"), "E_NONFINITE", "pow produced a nonfinite value");
    err(&value("(* (pow 10 300) (pow 10 300))"), "E_NONFINITE", "*");
    is(&value("(pow 10 3)"), Value::Int(1000));
    // the study's rules: divide and mod by zero give 0, sqrt takes the absolute value
    is(&value("(/ 1 0)"), Value::Int(0));
    is(&value("(mod 5 0)"), Value::Int(0));
    is(&value("(sqrt -4)"), Value::Int(2));
    let ed = |b: &str| format!("(workspace w (graph g :context editor [(n : int 3)] (ui/workspace {b})))");
    let tile = ed("(ui/tile (for [i (range n)] (ui/graph)))");
    static_ws(&check(&tile));
    let with_n = |n: i64| Some(vec![("g".to_owned(), vec![("n".to_owned(), Value::Int(n))])]);
    for n in [20, 0] {
        match static_with(&check(&tile), false, with_n(n).as_deref(), None) {
            Err(d) => assert!(d.code == "E_RANGE" && d.message.contains("1–16"), "{d:?}"),
            Ok(_) => panic!("ran"),
        }
    }
    static_ws(&check(&ed("(ui/split (str \"hori\" \"zontal\") (ui/graph) (ui/lisp))")));
    err(&ed("(ui/split (str \"dia\" \"gonal\") (ui/graph) (ui/lisp))"), "E_RANGE", "axis");
    err(&ed("(ui/split-at \"vertical\" (* n 0.5) (ui/graph) (ui/lisp))"), "E_RANGE", "ratio");
    static_ws(&check(&ed("(ui/split-at \"vertical\" (* n 0.1) (ui/graph) (ui/lisp))")));
    // settings ranges are the schema's: Editor_document.Contexts checks them when it lowers
    static_ws(&check("(workspace w (graph g :context settings [(f : int 60)] (settings/config :fps f)))"));
}

#[test]
fn c2_str_formatting() {
    let s = |src: &str| match bound(&value(&format!("(let* [z (str {src})] 1)")), "z") { Value::Text(s) => String::from_utf8(s).unwrap(), _ => panic!("not text") };
    assert!(s("3") == "3" && s("-3") == "-3" && s("2.0") == "2" && s("100.0") == "100");
    assert!(s("0.1") == "0.1" && s("0.30000000000000004") == "0.3" && s("1.23456") == "1.2346");
    for (x, want) in [("0.03125", "0.0313"), ("-0.03125", "-0.0313"), ("0.00005", "0.0001"), ("0.00015", "0.0001"), ("1.00005", "1.0001"), ("0.09375", "0.0938"), ("0.15625", "0.1563"), ("2.00005", "2"), ("0.00035", "0.0003"), ("0.000125", "0.0001"), ("123456789.123456", "123456789.1235")] {
        assert_eq!(s(x), want, "{x}");
    }
    assert!(s("-0.00001") == "0" && s("0.99999") == "1" && s("12345.6789") == "12345.6789");
    assert!(s("true false") == "truefalse" && s("[1 2.5 3]") == "[1 2.5 3]" && s("(list)") == "[]");
    assert!(s("{:a 1.5 :b [1 2 3]}") == "{:a 1.5 :b [1 2 3]}" && s("\"x\" 1") == "x1");
    assert_eq!(s("[1 0.5 0.33333]"), "[1 0.5 0.3333]");
    assert_eq!(s("(list [1 2 3] [4 5 6])"), "[[1 2 3] [4 5 6]]");
    assert_eq!(s("{:p (list 1 2) :q {:r true}}"), "{:p [1 2] :q {:r true}}");
}

#[test]
fn l2_l4_l5_product_order_empty_results_ints() {
    assert!(same(&bound(&value("(let* [z (for [a (range 2) b (range 2) c (range 2)] (+ (* a 4) (+ (* b 2) c)))] 1)"), "z"), &ints(&[0, 1, 2, 3, 4, 5, 6, 7])));
    assert!(same(&bound(&value("(let* [z (for [a (range 3) b (range a 3)] (+ (* a 10) b))] 1)"), "z"), &ints(&[0, 1, 2, 11, 12, 22])));
    is(&value("(count (for [i (range 0)] i))"), Value::Int(0));
    let r = run_ws(&check(&sop("(sop/merge (for [i (range 0)] (sop/box)))")), 0.);
    // merge of nothing: one merge node with no inputs
    assert!(count_of(&r, "sop/merge") == 1 && nodes_of(&r, "sop/merge")[0].args.is_empty());
    assert_eq!(bound(&value("(let* [z (range 3)] 1)"), "z"), ints(&[0, 1, 2]));
    assert_eq!(bound(&value("(let* [z (linspace 0 1 3)] 1)"), "z"), Value::List(vec![Value::Float(0.), Value::Float(0.5), Value::Float(1.)]));
    assert_eq!(bound(&value("(let* [z (linspace 2 5 1)] 1)"), "z"), Value::List(vec![Value::Float(2.)]));
    is(&value("(count (range 2 6))"), Value::Int(4));
}

#[test]
fn l6_l14_accumulators_and_ref_overrides() {
    assert_eq!(g_value(&value("(fold [a 1] [i (range 3)] (* a 2.5))")), Value::Float(15.625));
    assert_eq!(g_value(&value("(fold [a 0] [i (range 3)] (+ a 0.5))")), Value::Float(1.5));
    assert_eq!(g_value(&value("(fold [a 0] [i (range 3)] (+ a i))")), Value::Int(3));
    assert_eq!(bound(&value("(let* [z (scan [a 0] [i (range 2)] (+ a 0.5))] 1)"), "z"), Value::List(vec![Value::Float(0.5), Value::Float(1.)]));
    assert_eq!(bound(&value("(let* [z (sum [i (range 3)] [i 0 1])] 1)"), "z"), Value::Vec3(3., 0., 3.));
    assert_eq!(g_value(&value("(sum [i (range 3)] 0.5)")), Value::Float(1.5));
    assert_eq!(g_value(&value("(sum [i (range 3)] i)")), Value::Int(3));
    let ws = check("(workspace w (graph a :context value [(n : float 2)] (* n 3)) (graph g :context value (+ (ref a :n 5) (+ (ref a :n 5) (ref a :n 5.0)))))");
    let r = static_ws(&ws);
    assert!(same(&result(&r, "g"), &Value::Int(45)));
    // a, then a with n=5 once (5 and 5.0 are the same float)
    assert_eq!(r.plan.instances.len(), 3);
    let ws = check("(workspace w (graph a :context sop [(n : int 2)] (sop/box :size n)) (graph g :context sop (sop/merge (ref a) (ref a :n 3) (ref a :n 3) (ref a))))");
    let r = static_ws(&ws);
    // a with its default, a with n=3
    assert_eq!(r.plan.instances.len(), 3);
    assert_eq!(nodes_of(&r, "sop/box").iter().map(|n| n.inst).collect::<Vec<_>>(), vec![0, 2]);
    // the two refs with the same tuple wire the same node
    let merge = nodes_of(&r, "sop/merge")[0];
    let ids = merge.args.iter().map(|(_, v)| match v { Value::Deferred(Ty::Named(n), id) if n == "geometry" => *id, o => panic!("merge inputs: {o:?}") }).collect::<Vec<_>>();
    assert!(ids.len() == 4 && ids[0] == ids[3] && ids[1] == ids[2] && ids[0] != ids[1]);
    let ws = check("(workspace w (graph g :context value [(n : float 2)] (* n 3)))");
    let r = ok(static_with(&ws, false, Some(&[("g".to_owned(), vec![("n".to_owned(), Value::Int(4))])]), None));
    assert_eq!(result(&r, "g"), Value::Float(12.));
    assert_eq!(r.plan.instances[0].inputs, vec![("n".to_owned(), Value::Float(4.))]);
}

#[test]
fn w5_records_are_bounded() {
    let rs = records(&value("(sum [i (range 100)] (sum [j (range 100)] (let* [x (* i j)] x)))"), 0.);
    let longest = rs.iter().map(|(_, l)| l.len()).max().unwrap();
    assert_eq!(longest, 4096);
    let inner = rs.iter().find(|(q, l)| l.len() == 4096 && q.contains(&"x".to_owned())).unwrap();
    assert_eq!(inner.1.len(), 4096);
    let rs = records(&sop("(let* [a (sop/box)] a)"), 0.);
    assert_eq!(first_at(&rs, &["g", "a"]), Value::Deferred(ty::geometry(), 0));
    // nothing is recorded unless asked
    assert!(run_ws(&check(&value("(sum [i (range 3)] i)")), 0.).records.is_empty());
}

#[test]
fn wave_curves_carry_live_points() {
    let ws = check(&std::fs::read_to_string(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../sketches/ws_wave.rays")).unwrap());
    let s = static_ws(&ws);
    let curves = nodes_of(&s, "sop/curve");
    assert_eq!(curves.len(), 6);
    match arg_of(curves[0], "points") {
        Value::List(pts) => assert!(pts.len() == 90 && is_live(&Value::List(pts.clone()))),
        _ => panic!("points"),
    }
    let r = run_ws(&ws, 0.5);
    match arg_of(nodes_of(&r, "sop/curve")[0], "points") {
        Value::List(pts) => assert!(pts.iter().all(|p| matches!(p, Value::Vec3(..)))),
        _ => panic!("points"),
    }
}

#[test]
fn w8_for_over_point_list_is_a_zone_node() {
    let ws = check(&sop("(let* [g (sop/grid) spots (sop/scatter g :count 5 :seed 3) dot (sop/box)\n      dots (for [p (sop/point_list spots :key \"id\")] (sop/transform dot :translate p))\n      result (sop/merge g dots)] result)"));
    let s = static_ws(&ws);
    let zone = nodes_of(&s, "zone/points")[0];
    let (lo, hi) = match (arg_of(zone, "lo"), arg_of(zone, "hi")) { (Value::Int(a), Value::Int(b)) => (*a, *b), _ => panic!("range") };
    assert_eq!(hi - lo, 1);
    assert_eq!(*arg_of(zone, "key"), t("id"));
    // the element is a residual, forced with the element bound
    let tr = nodes_of(&s, "sop/transform")[0];
    let p = arg_of(tr, "translate");
    assert!(is_live(p));
    let key = element_key(&vec!["g".to_owned(), "dots".to_owned()]);
    match force_with(p, &input::at_time(0.), None, Some(&[(key, Value::Vec3(1., 2., 3.))]), None) {
        Ok(Value::Vec3(x, y, z)) if (x, y, z) == (1., 2., 3.) => (),
        o => panic!("element not bound: {o:?}"),
    }
    assert!(force(p, &input::at_time(0.), None).is_err());
    // not a list: only sop/merge takes it, and the structure cannot read the element
    err(&sop("(let* [g (sop/grid) n (count (for [p (sop/point_list g)] (sop/box)))] g)"), "E_TYPE", "");
    err(&sop("(let* [g (sop/grid)] (sop/merge (for [p (sop/point_list g)] (if (> p.x 0) (sop/box) (sop/grid)))))"), "E_ZONE", "");
    err(&sop("(let* [g (sop/grid)] (sop/merge (for [p (sop/point_list g) i (range 2)] (sop/box))))"), "E_ZONE", "");
}

#[test]
fn l16_skip_leaves_iterations_out() {
    let nested = |l: &[&[i64]]| Value::List(l.iter().map(|x| ints(x)).collect());
    let zs = |src: &str, want: Value| assert!(same(&bound(&value(&format!("(let* [z {src}] 1)")), "z"), &want), "{src}");
    zs("(for [i (range 5)] :skip [1 3] i)", ints(&[0, 2, 4]));
    zs("(for [i (range 5)] :skip [[1] [3]] i)", ints(&[0, 2, 4]));
    is(&value("(count (for [i (range 5)] :skip [0 1 2 3 4] i))"), Value::Int(0));
    zs("(for [i (range 2) j (range 3)] :skip [4] (+ (* 10 i) j))", ints(&[0, 1, 2, 10, 12]));
    zs("(for [i (range 3)] (for [j (range 2)] :skip [[1 0] [2 1]] (+ (* 10 i) j)))", nested(&[&[0, 1], &[11], &[20]]));
    zs("(for [i (range 2)] (for [j (range 2)] (for [k (range 2)] :skip [[1 0 1] [0 1 0]] (+ (* 100 i) (+ (* 10 j) k)))))", Value::List(vec![Value::List(vec![ints(&[0, 1]), ints(&[11])]), Value::List(vec![ints(&[100]), ints(&[110, 111])])]));
    let all = run_ws(&check(&sop("(sop/merge (for [i (range 4)] (sop/box :size (+ 1 i))))")), 0.);
    let some = run_ws(&check(&sop("(sop/merge (for [i (range 4)] :skip [2] (sop/box :size (+ 1 i))))")), 0.);
    let keys = |r: &Eval| nodes_of(r, "sop/box").iter().map(|n| (n.site.clone(), n.iter.clone())).collect::<Vec<_>>();
    assert!(count_of(&all, "sop/box") == 4 && count_of(&some, "sop/box") == 3);
    assert_eq!(keys(&some), keys(&all).into_iter().filter(|(_, it)| *it != vec![2]).collect::<Vec<_>>());
    for n in nodes_of(&some, "sop/box") {
        let twin = nodes_of(&all, "sop/box").into_iter().find(|m| m.site == n.site && m.iter == n.iter).unwrap();
        assert_eq!(n.args, twin.args);
    }
    let rs = records(&value("(let* [xs (for [i (range 4)] :skip [2] (* i 2))] (count xs))"), 0.);
    assert_eq!(recs(&rs, &["g", "xs", ":i"]).len(), 4);
    assert_eq!(recs(&rs, &["g", "xs", "@result"]).iter().map(|x| x.0.clone()).collect::<Vec<_>>(), vec![vec![0], vec![1], vec![3]]);
    let scene = |body: &str| format!("(workspace w (graph s :context scene {body}))");
    let r = |body: &str| result(&run_ws(&check(&scene(body)), 0.), "s");
    let args = |v: &Value| match v { Value::Struct(_, _, a) => a.len() as i64, _ => -1 };
    assert_eq!(args(&r("(scene/merge (scene/light) (scene/light) (scene/light))")), 3);
    assert_eq!(args(&r("(scene/merge (scene/light) (scene/light) (scene/light) :skip [1])")), 2);
    assert_eq!(args(&r("(scene/merge (scene/light) (scene/light) :skip [[0] [1]])")), 0);
    // inside a loop the tuple starts with the loop's iteration
    match r("(scene/merge (for [i (range 2)] (scene/merge (scene/light) (scene/light) :skip [[1 0]])))") {
        Value::Struct(_, _, f) if f.len() == 2 => assert!(args(&f[0].1) == 2 && args(&f[1].1) == 1),
        o => panic!("the merge of merges: {o:?}"),
    }
}

#[test]
fn live_packed_accumulators_defer_the_complete_iteration() {
    for body in ["(fold [a 0] [x (array/range 2051)] (+ a t))", "(reduce (fn [a x] (+ a t)) 0.0 (array/range 2051))"] {
        let workspace = check(&value(body));
        let v = result(&static_ws(&workspace), "g");
        match &v {
            Value::Residual(r) => match &Residual::residual_view(*r).unwrap().term.node {
                W::Node::Loop { kind: W::LoopKind::Fold, .. } | W::Node::Hof(W::HofKind::Reduce, _) => (),
                _ => panic!("accumulator was unrolled into per-element residuals"),
            },
            _ => panic!("live accumulator did not defer"),
        }
        assert_eq!(force(&v, &input::at_time(0.5), None), Ok(Value::Float(1025.5)));
    }
}

#[test]
fn image_constructors_preserve_live_arguments() {
    for (body, field, expected) in [
        ("(image/noise :width 2 :height 2 :frequency (+ 0.3 t))", "frequency", Value::Float(2.3)),
        ("(image/noise :width 2 :height 2 :freq (+ 0.3 t))", "frequency", Value::Float(2.3)),
        ("(image/noise :width 2 :height 2 :freq 0.7 :frequency (+ 0.3 t))", "frequency", Value::Float(2.3)),
        ("(image/load (if (< t 1.0) \"before.png\" \"after.png\"))", "path", t("after.png")),
        ("(image/render (ref drawing) :width (if (< t 1.0) 2 3) :height 2)", "width", Value::Int(3)),
    ] {
        let evaluated = static_ws(&check(&format!("(workspace w (graph drawing :context draw (draw/background \"#000000\")) (graph g :context image {body}))")));
        let id = match result(&evaluated, "g") { Value::Deferred(ty_, id) if ty_ == ty::image() => id, _ => panic!("live image constructor did not create a deferred image node") };
        let argument = arg_of(&evaluated.plan.nodes[id as usize], field).clone();
        assert!(is_live(&argument), "{body}");
        assert_eq!(force(&argument, &input::at_time(2.), None), Ok(expected), "{body}");
    }
}

#[test]
fn equality_and_updates_use_captured_field_tags() {
    for (body, want) in [
        ("(let* [a {:v t} b {:v t}] (equal? a b))", true),
        ("(let* [a (list {:v t}) b (list {:v t})] (equal? a b))", true),
        ("(let* [a {:v {:x t}}] (equal? a {:v {:x t}}))", true),
        ("(let* [a {:x t :y 2.} b {:y 2. :x t}] (equal? a b))", false),
        ("(let* [a {:x t} b {:x (+ t 1.)}] (equal? a b))", false),
    ] {
        let v = result(&static_ws(&check(&value(body))), "g");
        for time in [0., 0.37, 2.] { assert_eq!(force(&v, &input::at_time(time), None), Ok(Value::Bool(want)), "{body}"); }
    }
    for body in [
        "(let* [r {:n (int t)} updated (assoc r :n 1.7)] updated.n)",
        "(let* [r {:v {:n (int t)} :unused (exp (* t 100000.))} updated (assoc r :v {:n 1.7})] updated.v.n)",
        "(let* [r {:n (int t)} updated (fold [a r] [x (array/range 2)] (assoc a :n 1.7))] updated.n)",
        "(let* [r {:n (int t)} xs (scan [a r] [x (range 2)] (assoc a :n 1.7))] (get (nth xs 0) :n))",
        "(let* [r {:n (int t)} updated (reduce (fn [(a : {:n int}) x] (assoc a :n 1.7)) r (list 0))] updated.n)",
    ] {
        let v = result(&static_ws(&check(&value(body))), "g");
        for time in [-2.5, 0., 0.37, 2.] { assert_eq!(force(&v, &input::at_time(time), None), Ok(Value::Int(2)), "{body} at {time}"); }
    }
    for body in [
        "(let* [r {:n (int t)} updated (reduce (fn [a x] (assoc a :n 1.7)) r (list 0))] updated.n)",
        "(let* [r {:n (int t)} f (fn [a x] (assoc a :n 1.7)) updated (reduce f r (list 0))] updated.n)",
        "(get (first (map (fn [a] (assoc a :n 1.7)) (list {:n (int t)}))) :n)",
        "(get (first (filter (fn [a] (= (get (assoc a :n 1.7) :n) 2)) (list {:n 2}))) :n)",
        "(get (first (sort-by (fn [a] (get (assoc a :n 1.7) :n)) (list {:n 2} {:n (int t)}))) :n)",
    ] {
        let v = result(&static_ws(&check(&value(body))), "g");
        for time in [-2.5, 0., 0.37, 2.] {
            let live = input::at_time(time);
            assert_eq!(force(&v, &live, None), Ok(Value::Int(2)), "{body}");
            assert_eq!(Host::force_reference(&v, &live, None, None, None), Ok(Value::Int(2)), "{body}");
        }
    }
    is(&value("(reduce (fn [a x] (+ a x)) 0 (list 0.5 0.25))"), Value::Float(0.75));
    is(&value("(let* [lengths (map array/count (list (array/float 2) (array/float 4)))] (+ (first lengths) (last lengths)))"), Value::Int(6));
}

#[test]
fn bypass_nonfinite_and_whole_indices() {
    is(&value("(let* [f (fn [a b] (+ a b))] ^:bypass (f 2 3))"), Value::Int(2));
    for source in ["(value/lerp 0 (pow 10 308) 100)", "(* [1 2 3] (* (pow 10 308) 10))"] {
        match static_eval(&check(&format!("(workspace w (graph g :context host {source}))"))) { Err(d) => assert_eq!(d.code, "E_NONFINITE", "{source}"), Ok(_) => panic!("{source} was accepted") }
    }
    match static_eval(&check("(workspace w (graph g :context value (nth (list 1 2 3 4) 2.5)))")) { Err(d) => assert_eq!(d.code, "E_LIST_RANGE"), Ok(_) => panic!("nth 2.5 read an element") }
}

// A static packed map of more than 4,096 elements: the host's whole value is taken only when it
// agrees with the elements the walker made; the evaluation is the ordinary one either way.
#[test]
fn static_bulk_maps() {
    use std::sync::{Arc, Mutex};
    let ws = check("(workspace bulk\n      (graph g :context host\n        (let* [f (fn [i] (let* [twice (* i 2)] [twice (+ i 0.5) 0]))\n               large (map f (array/range 5000))\n               again (map f (array/range 4100))\n               small (map f (array/range 4096))\n               floats (map (fn [i] (* i 0.25)) (array/range 4097))]\n          {:large large :again again :small small :floats floats})))");
    let ordinary = static_rec(&ws);
    let field = |r: &Eval, name: &str| match result(r, "g") { Value::Record(fields) => fields.into_iter().find(|(n, _)| n == name).unwrap().1, _ => panic!("g is a record") };
    let data = |v: &Value| match v { Value::Float_array(d) | Value::Vec2_array(d) | Value::Vec3_array(d) | Value::Vec4_array(d) => d.clone(), _ => panic!("packed") };
    let record_counts = |r: &Eval| r.records.iter().map(|(q, v)| (q.clone(), v.len())).collect::<Vec<_>>();
    let same_as_ordinary = |r: &Eval| {
        for name in ["large", "again", "small", "floats"] { assert_eq!(field(r, name), field(&ordinary, name), "{name}"); }
        assert_eq!(record_counts(r), record_counts(&ordinary));
    };
    // the true arrays, by element count
    let whole = |n: usize| ["large", "again", "floats"].iter().find_map(|name| { let v = field(&ordinary, name); (flow_syntax::value::array_length(&v).unwrap() == n).then_some(v) });
    let offered = Arc::new(Mutex::new(Vec::<usize>::new()));
    type Bulk = Arc<dyn Fn(usize) -> Option<Value> + Send + Sync>;
    let run_bulk = |bulk: Bulk| {
        offered.lock().unwrap().clear();
        let off = offered.clone();
        let handlers = ResidualHandlers {
            bulk: Some(Arc::new(move |id| {
                let view = Residual::residual_view(id).unwrap();
                let n = match &view.term.node {
                    W::Node::Hof(W::HofKind::Map, terms) if terms.len() == 2 => match Residual::eval_term(id, &terms[1], &input::at_time(0.), None, None, None) {
                        Ok(v) => flow_syntax::value::array_length(&v).unwrap(),
                        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
                    },
                    _ => panic!("a map is offered"),
                };
                off.lock().unwrap().push(n);
                bulk(n)
            })),
            ..Default::default()
        };
        ok(Residual::handle(handlers, &mut || Host::static_with_requests(None, true, None, &ws)))
    };
    // Supplied whole and correct: used. Only the maps over 4,096 elements are offered, once each.
    let w2 = ordinary.clone();
    let supplied = run_bulk(Arc::new(move |n| { let f = |name: &str| match result(&w2, "g") { Value::Record(fs) => fs.into_iter().find(|(k, _)| k == name).unwrap().1, _ => panic!() }; ["large", "again", "floats"].iter().find_map(|name| { let v = f(name); (flow_syntax::value::array_length(&v).unwrap() == n).then_some(v) }) }));
    let mut o = offered.lock().unwrap().clone(); o.sort();
    assert_eq!(o, vec![4097, 4100, 5000]);
    same_as_ordinary(&supplied);
    // The arrays are the host's own, not copies: it was not walked past the first 4,096.
    assert!(std::sync::Arc::ptr_eq(&arc_data(&field(&supplied, "large")), &arc_data(&whole(5000).unwrap())));
    // Refused by the host: the walker finishes.
    let walked = run_bulk(Arc::new(|_| None));
    same_as_ordinary(&walked);
    assert!(!std::sync::Arc::ptr_eq(&arc_data(&field(&walked, "large")), &arc_data(&whole(5000).unwrap())));
    // A value that differs inside the walked elements, has another length or another element kind is not taken.
    let o2 = ordinary.clone();
    let wrong = run_bulk(Arc::new(move |n| {
        let f = |name: &str| match result(&o2, "g") { Value::Record(fs) => fs.into_iter().find(|(k, _)| k == name).unwrap().1, _ => panic!() };
        ["large", "again", "floats"].iter().find_map(|name| { let v = f(name); (flow_syntax::value::array_length(&v).unwrap() == n).then_some(v) }).map(|v| {
            let mut d = (*arc_data(&v)).clone(); d[7] += 1.;
            match v { Value::Float_array(_) => Value::Float_array(Arc::new(d)), _ => Value::Vec3_array(Arc::new(d)) }
        })
    }));
    same_as_ordinary(&wrong);
    let o3 = ordinary.clone();
    same_as_ordinary(&run_bulk(Arc::new(move |_| match result(&o3, "g") { Value::Record(fs) => fs.into_iter().find(|(k, _)| k == "again").map(|x| x.1), _ => None })));
    let o4 = ordinary.clone();
    same_as_ordinary(&run_bulk(Arc::new(move |n| {
        let f = |name: &str| match result(&o4, "g") { Value::Record(fs) => fs.into_iter().find(|(k, _)| k == name).unwrap().1, _ => panic!() };
        ["large", "again", "floats"].iter().find_map(|name| { let v = f(name); (flow_syntax::value::array_length(&v).unwrap() == n).then_some(v) }).map(|v| Value::Vec2_array(arc_data(&v)))
    })));
    same_as_ordinary(&run_bulk(Arc::new(|_| Some(Value::Int(1)))));
    let _ = data;
    // A failure past the walked elements is the walker's own when the host has no value.
    let failing = check("(workspace bulk (graph g :context host (map (fn [i] (exp (- i 4300))) (array/range 5200))))");
    let a = static_eval(&failing);
    let b = Residual::handle(ResidualHandlers { bulk: Some(Arc::new(|_| None)), ..Default::default() }, &mut || Host::static_with_requests(None, false, None, &failing));
    match (a, b) { (Err(a), Err(b)) => assert!(a.code == "E_NONFINITE" && a == b), _ => panic!("a nonfinite element past the walked ones fails either way") }
    // A map that reads the frame is never offered: it becomes a residual before any host call.
    let live = check("(workspace bulk (graph g :context host (map (fn [i] (+ i t)) (array/range 5000))))");
    let asked = Arc::new(Mutex::new(0));
    let ak = asked.clone();
    let r = ok(Residual::handle(ResidualHandlers { bulk: Some(Arc::new(move |_| { *ak.lock().unwrap() += 1; None })), ..Default::default() }, &mut || Host::static_with_requests(None, false, None, &live)));
    assert!(matches!(result(&r, "g"), Value::Residual(_)), "a live map stays a residual");
    assert_eq!(*asked.lock().unwrap(), 0);
}
fn arc_data(v: &Value) -> std::sync::Arc<Vec<f64>> { match v { Value::Float_array(d) | Value::Vec2_array(d) | Value::Vec3_array(d) | Value::Vec4_array(d) => d.clone(), _ => panic!("packed") } }
