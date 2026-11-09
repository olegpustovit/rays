//! Port of lib/flow_sop/test_workspace_edit.ml (W3): every graph gesture of the study as a pure
//! rewrite of the workspace text: apply the op, compare the printed text. A refused edit changes
//! nothing.
//!
//! Uses the editor's complete kinds catalog, including scene/world/layout gestures.
use flow_sop::lisp_sop::support::*;
use flow_sop::{manifest, lisp_sop};
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{self as e, ArgKey, Loop, Op};
use flow_graph::projection as p;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::{lisp, ty};

fn catalog() -> Catalog {
    crate::kinds::catalog(1, &lisp_sop::factories()).unwrap()
}

fn canon(forms: &[Syntax]) -> String {
    lisp::print(forms).unwrap().0
}

fn norm(text: &str) -> String {
    canon(&parse(text))
}

fn apply(text: &str, op: Op) -> String {
    match e::apply(&catalog(), &parse(text), &op) {
        Ok(forms) => canon(&forms),
        Err(d) => panic!("{}: {}", e::label(&op), rays_diagnostic::to_string(&d)),
    }
}

fn same(name: &str, text: &str, op: Op, expected: &str) {
    let got = apply(text, op);
    let want = norm(expected);
    assert!(got == want, "{name}\n--- got\n{got}--- want\n{want}");
}

fn refused(code: &str, name: &str, text: &str, op: Op) {
    match e::apply(&catalog(), &parse(text), &op) {
        Ok(forms) => panic!("{name} was accepted:\n{}", canon(&forms)),
        Err(d) => assert!(code.is_empty() || d.code == code, "{name}: code {} message {}", d.code, d.message),
    }
}

fn has(text: &str, sub: &str) -> bool {
    text.contains(sub)
}

fn g(body: &str) -> String {
    format!("(workspace w\n  (graph g :context sop [(n : int 4)]\n    {body}))")
}

fn v(body: &str) -> String {
    format!("(workspace w\n  (graph g :context value [(n : int 4)]\n    {body}))")
}

fn base() -> String {
    g("(let* [a (sop/uv_sphere :radius 0.5)\n           b (sop/transform a :translate [1 2 3])\n           c (sop/subdivide b :iterations 1)]\n      c)")
}

fn num(t: &str) -> Syntax {
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let name = b.intern(t);
    let id = b.push(0, Node::Num(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    b.freeze().handle(id)
}

fn call(name: &str) -> Syntax {
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let name = b.intern(name);
    let head = b.push(0, Node::Sym(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    let children = b.range(&[head]);
    let id = b.push(0, Node::List(children), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    b.freeze().handle(id)
}

fn node(l: &[&str]) -> Vec<String> {
    let mut out = vec!["g".to_owned()];
    out.extend(l.iter().map(|x| (*x).to_owned()));
    out
}

fn path(l: &[&str]) -> Vec<String> {
    l.iter().map(|x| (*x).to_owned()).collect()
}

fn kw(name: &str) -> ArgKey {
    ArgKey::Kw(name.to_owned())
}

fn set_arg(n: Vec<String>, key: ArgKey, sub: Vec<i64>, value: Syntax) -> Op {
    Op::SetArg { node: n, key, sub, value }
}

fn connect(n: Vec<String>, key: ArgKey, src: &str, iter: bool) -> Op {
    Op::Connect { node: n, key, src: src.to_owned(), iter }
}

fn rename(n: Vec<String>, to: &str) -> Op {
    Op::Rename { node: n, to_: to.to_owned() }
}

#[test]
fn part1_scrub() {
    same(
        "scrub",
        &base(),
        set_arg(node(&["a"]), kw("radius"), vec![], num("0.8")),
        &g("(let* [a (sop/uv_sphere :radius 0.8) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)"),
    );
    same(
        "vector component",
        &base(),
        set_arg(node(&["b"]), kw("translate"), vec![1], num("9")),
        &g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 9 3]) c (sop/subdivide b :iterations 1)] c)"),
    );
    same(
        "missing keyword is added",
        &base(),
        set_arg(node(&["b"]), kw("scale"), vec![], num("2")),
        &g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3] :scale 2) c (sop/subdivide b :iterations 1)] c)"),
    );
    let scrub = |x: &str| set_arg(node(&["a"]), kw("radius"), vec![], num(x));
    assert!(e::gesture(&scrub("1")) == e::gesture(&scrub("2")) && e::gesture(&scrub("1")).is_some(), "scrub merges into one gesture");
    assert!(e::gesture(&Op::ToggleBypass { node: node(&["a"]) }).is_none(), "other ops do not merge");
    let default = |input: &str, value: &str| Op::SetInputDefault { form: "g".into(), input: input.into(), value: num(value) };
    assert!(
        e::gesture(&default("n", "1")) == e::gesture(&default("n", "2"))
            && e::gesture(&default("n", "1")).is_some()
            && e::gesture(&default("n", "1")) != e::gesture(&default("m", "1")),
        "scrubbing an input default is one gesture per input"
    );
    refused("E_UNKNOWN_PARAM", "unknown keyword", &base(), set_arg(node(&["a"]), kw("nope"), vec![], num("1")));
}

#[test]
fn part2_connect_disconnect_input_defaults() {
    let fwd = g("(let* [a (sop/uv_sphere) c (sop/subdivide a) b (sop/transform a)] c)");
    same(
        "connect reorders",
        &fwd,
        connect(node(&["c"]), ArgKey::Pos(0), "b", false),
        &g("(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/subdivide b)] c)"),
    );
    refused(
        "E_GRAPH_CYCLE",
        "connect cycle",
        &g("(let* [a (sop/transform nil) b (sop/transform a)] b)"),
        connect(node(&["a"]), ArgKey::Pos(0), "b", false),
    );
    same(
        "connect to result",
        &fwd,
        connect(node(&["@result"]), ArgKey::Whole, "b", false),
        &g("(let* [a (sop/uv_sphere) c (sop/subdivide a) b (sop/transform a)] b)"),
    );
    let lp = g("(let* [ring (for [i (range n)] (sop/uv_sphere :radius 0.5)) m (sop/merge ring)] m)");
    same(
        "iteration step",
        &lp,
        connect(path(&["g", "ring", "@result"]), kw("radius"), "i", true),
        &g("(let* [ring (for [i (range n)] (sop/uv_sphere :radius (* i 0.5))) m (sop/merge ring)] m)"),
    );
    same(
        "iteration by name",
        &lp,
        connect(path(&["g", "ring", "@result"]), kw("radius"), "i", false),
        &g("(let* [ring (for [i (range n)] (sop/uv_sphere :radius i)) m (sop/merge ring)] m)"),
    );
    same(
        "disconnect keyword",
        &g("(let* [k 0.4 a (sop/uv_sphere :radius k)] a)"),
        Op::Disconnect { node: node(&["a"]), key: kw("radius"), fallback: None },
        &g("(let* [k 0.4 a (sop/uv_sphere)] a)"),
    );
    same(
        "disconnect keyword onto a written default",
        &g("(let* [k 0.4 a (sop/uv_sphere :radius k)] a)"),
        Op::Disconnect { node: node(&["a"]), key: kw("radius"), fallback: Some(num("1.0")) },
        &g("(let* [k 0.4 a (sop/uv_sphere :radius 1.0)] a)"),
    );
    same(
        "disconnect positional with fallback",
        &base(),
        Op::Disconnect { node: node(&["b"]), key: ArgKey::Pos(0), fallback: e::default_for(&ty::geometry(), "geo", None) },
        &g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform nil :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)"),
    );
    same(
        "input default",
        &base(),
        Op::SetInputDefault { form: "g".into(), input: "n".into(), value: num("8") },
        "(workspace w (graph g :context sop [(n : int 8)] (let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)))",
    );
    refused("", "missing input", &base(), Op::SetInputDefault { form: "g".into(), input: "zz".into(), value: num("8") });
}

// the shared-name case writes scene/* kinds, which only editor_document declares
#[test]
fn part2_set_arg_beside_a_self_named_graph_ref() {
    // a binding named like the graph it [ref]s does not read itself: no cycle on reorder
    let shared = |fov: &str| {
        format!(
            "(workspace w\n  (graph cage :context sop (sop/box))\n  (graph scene :context scene\n    (let* [cage (scene/geometry (ref cage) :name \"cage\") camera (scene/camera :fov {fov})]\n      (scene/merge cage camera))))"
        )
    };
    same("set arg beside a self-named graph ref", &shared("30"), set_arg(path(&["scene", "camera"]), kw("fov"), vec![], num("40")), &shared("40"));
}

#[test]
fn part3_unfold_and_fold_round_trip() {
    let conditional = g("(let* [b (if true (sop/uv_sphere :radius 0.5) (sop/box))] b)");
    let unfolded_conditional = g("(let* [uv_sphere (sop/uv_sphere :radius 0.5) b (if true uv_sphere (sop/box))] b)");
    same("unfold conditional arm", &conditional, Op::Unfold { node: node(&["b"]), key: ArgKey::Arm(0), sub: vec![] }, &unfolded_conditional);
    same("fold into conditional arm", &unfolded_conditional, Op::FoldInto { node: node(&["uv_sphere"]) }, &conditional);
    let nested = g("(let* [b (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3])] b)");
    let unfolded = g("(let* [uv_sphere (sop/uv_sphere :radius 0.5) b (sop/transform uv_sphere :translate [1 2 3])] b)");
    same("unfold", &nested, Op::Unfold { node: node(&["b"]), key: ArgKey::Pos(0), sub: vec![] }, &unfolded);
    same("fold", &unfolded, Op::FoldInto { node: node(&["uv_sphere"]) }, &nested);
    same(
        "unfold a loop",
        &g("(let* [m (sop/merge (for [i (range 3)] (sop/uv_sphere)))] m)"),
        Op::Unfold { node: node(&["m"]), key: ArgKey::Pos(0), sub: vec![] },
        &g("(let* [m_each (for [i (range 3)] (sop/uv_sphere)) m (sop/merge m_each)] m)"),
    );
    same(
        "unfold part of a vector",
        &g("(let* [b (sop/transform nil :translate [(+ 1 2) 0 0])] b)"),
        Op::Unfold { node: node(&["b"]), key: kw("translate"), sub: vec![0] },
        &g("(let* [sum_2 (+ 1 2) b (sop/transform nil :translate [sum_2 0 0])] b)"),
    );
    same(
        "fold into the result",
        &g("(let* [a (sop/uv_sphere) b (sop/transform a)] b)"),
        Op::FoldInto { node: node(&["b"]) },
        &g("(let* [a (sop/uv_sphere)] (sop/transform a))"),
    );
    assert!(
        has(&apply(&g("(let* [a (sop/uv_sphere)\n ; keep me\n b (sop/transform a)] (sop/subdivide b))"), Op::FoldInto { node: node(&["b"]) }), "keep me"),
        "fold dropped the binding's note"
    );
    refused("", "fold needs one use", &g("(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/merge a b)] c)"), Op::FoldInto { node: node(&["a"]) });
    refused("", "unfold an atom", &base(), Op::Unfold { node: node(&["c"]), key: kw("iterations"), sub: vec![] });
}

// a call written in an input (a step of a ->) is a node: its leaf is the holder's, #, the input
#[test]
fn nested_nodes() {
    let chain = g("(-> (sop/uv_sphere :radius 0.5) (sop/transform :translate [1 2 3]) (sop/subdivide :iterations 1) (sop/normals))");
    let sub = node(&["@result#0"]);
    let tr = node(&["@result#0#0"]);
    let ball = node(&["@result#0#0#0"]);
    same(
        "set an input of a step",
        &chain,
        set_arg(sub.clone(), kw("iterations"), vec![], num("2")),
        &g("(-> (sop/uv_sphere :radius 0.5) (sop/transform :translate [1 2 3]) (sop/subdivide :iterations 2) (sop/normals))"),
    );
    same(
        "set a vector part of a deep step",
        &chain,
        set_arg(tr.clone(), kw("translate"), vec![1], num("7")),
        &g("(-> (sop/uv_sphere :radius 0.5) (sop/transform :translate [1 7 3]) (sop/subdivide :iterations 1) (sop/normals))"),
    );
    assert!(has(&apply(&chain, Op::ToggleBypass { node: sub.clone() }), "^:bypass (sop/subdivide"), "a step takes the bypass flag");
    same(
        "naming a step binds it",
        &chain,
        rename(sub.clone(), "smooth"),
        &g("(let* [smooth (sop/subdivide (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]) :iterations 1)] (sop/normals smooth))"),
    );
    same(
        "a deleted step hands its place to its input",
        &chain,
        Op::DeleteNodes { nodes: vec![sub.clone()] },
        &g("(sop/normals (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]))"),
    );
    same(
        "two deleted steps, the inner first",
        &chain,
        Op::DeleteNodes { nodes: vec![sub.clone(), tr.clone()] },
        &g("(sop/normals (sop/uv_sphere :radius 0.5))"),
    );
    same(
        "a deleted source leaves nil",
        &chain,
        Op::DeleteNodes { nodes: vec![ball] },
        &g("(-> nil (sop/transform :translate [1 2 3]) (sop/subdivide :iterations 1) (sop/normals))"),
    );
    // wires never delete nodes: the output of a step read twice is named, a step a wire replaces stays
    let two = g("(let* [m (sop/merge (sop/transform (sop/uv_sphere)))] m)");
    same(
        "the output of a step feeds a second input",
        &two,
        connect(node(&["m"]), ArgKey::Pos(1), "m#0#0", false),
        &g("(let* [uv_sphere (sop/uv_sphere) m (sop/merge (sop/transform uv_sphere) uv_sphere)] m)"),
    );
    same(
        "a wire onto a step's place keeps the step",
        &g("(let* [a (sop/box) m (sop/normals (sop/transform (sop/uv_sphere)))] m)"),
        connect(node(&["m"]), ArgKey::Pos(0), "a", false),
        &g("(let* [a (sop/box) transform (sop/transform (sop/uv_sphere)) m (sop/normals a)] m)"),
    );
    same(
        "viewing a step makes it the result",
        &chain,
        connect(node(&["@result"]), ArgKey::Whole, "@result#0", false),
        &g("(let* [subdivide (sop/subdivide (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]) :iterations 1) normals (sop/normals subdivide)] subdivide)"),
    );
    refused("", "a step cannot be folded", &chain, Op::FoldInto { node: sub.clone() });
    refused("", "a step is not duplicated", &chain, Op::Duplicate { nodes: vec![sub.clone()] });
    assert!(
        e::remap(&rename(sub.clone(), "smooth"), &tr) == Some(node(&["smooth#0"]))
            && e::remap(&Op::DeleteNodes { nodes: vec![sub.clone()] }, &tr) == Some(sub.clone())
            && e::remap(&Op::DeleteNodes { nodes: vec![sub.clone()] }, &sub).is_none()
            && e::remap(&rename(node(&["b"]), "z"), &node(&["b#0"])) == Some(node(&["z#0"])),
        "layout keys follow a renamed or deleted step"
    );
}

#[test]
fn part4_repeat_iterate() {
    let two = g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :scale 0.8)] b)");
    same(
        "repeat geometry",
        &two,
        Op::Wrap { nodes: vec![node(&["a"])], loop_: Loop::For },
        &g("(let* [a_each (for [i (range 6)] (sop/uv_sphere :radius 0.5)) a (sop/merge a_each) b (sop/transform a :scale 0.8)] b)"),
    );
    same(
        "repeat numbers",
        &g("(let* [x (+ 1 2) y (* x 2) a (sop/uv_sphere :radius y)] a)"),
        Op::Wrap { nodes: vec![node(&["x"])], loop_: Loop::For },
        &g("(let* [x (sum [i (range 6)] (+ 1 2)) y (* x 2) a (sop/uv_sphere :radius y)] a)"),
    );
    same(
        "iterate",
        &two,
        Op::Wrap { nodes: vec![node(&["b"])], loop_: Loop::Fold },
        &g("(let* [a (sop/uv_sphere :radius 0.5) b (fold [prev a] [i (range 4)] (sop/transform prev :scale 0.8))] b)"),
    );
    refused("", "iterate needs an input to feed back", &g("(let* [a (sop/uv_sphere)] a)"), Op::Wrap { nodes: vec![node(&["a"])], loop_: Loop::Fold });
    refused(
        "",
        "repeat needs one result",
        &g("(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/merge a b)] c)"),
        Op::Wrap { nodes: vec![node(&["a"]), node(&["b"])], loop_: Loop::For },
    );
    same(
        "repeat two bindings",
        &g("(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/merge b)] c)"),
        Op::Wrap { nodes: vec![node(&["a"]), node(&["b"])], loop_: Loop::For },
        &g("(let* [b_each (for [i (range 6)] (let* [a (sop/uv_sphere) b (sop/transform a)] b)) b (sop/merge b_each) c (sop/merge b)] c)"),
    );
}

#[test]
fn part5_hoist() {
    let src = g("(let* [ring (for [i (range n)] (let* [u (/ i n) k (sop/uv_sphere :radius 0.2) s (sop/transform k :translate [u 0 0])] s)) m (sop/merge ring)] m)");
    same(
        "hoist",
        &src,
        Op::Hoist { node: path(&["g", "ring", "k"]) },
        &g("(let* [k (sop/uv_sphere :radius 0.2) ring (for [i (range n)] (let* [u (/ i n) s (sop/transform k :translate [u 0 0])] s)) m (sop/merge ring)] m)"),
    );
    refused("", "hoist a loop-dependent node", &src, Op::Hoist { node: path(&["g", "ring", "u"]) });
    refused("", "hoist from the top", &src, Op::Hoist { node: node(&["m"]) });
    same(
        "hoist the only binding",
        &g("(let* [ring (for [i (range 3)] (let* [k (sop/uv_sphere)] k)) m (sop/merge ring)] m)"),
        Op::Hoist { node: path(&["g", "ring", "k"]) },
        &g("(let* [k (sop/uv_sphere) ring (for [i (range 3)] k) m (sop/merge ring)] m)"),
    );
}

#[test]
fn part6_rename() {
    let src = g("(let* [a (sop/uv_sphere :radius 0.5)\n           ; the moved one\n           b (sop/transform a :translate [1 2 3])] b)");
    let want = g("(let* [a (sop/uv_sphere :radius 0.5)\n           ; the moved one\n           moved (sop/transform a :translate [1 2 3])] moved)");
    same("rename", &src, rename(node(&["b"]), "moved"), &want);
    same(
        "rename a source",
        &g("(let* [a (sop/uv_sphere) b (sop/transform a)] b)"),
        rename(node(&["a"]), "ball"),
        &g("(let* [ball (sop/uv_sphere) b (sop/transform ball)] b)"),
    );
    refused("", "rename to a used name", &src, rename(node(&["b"]), "a"));
    refused("", "rename to an operator", &src, rename(node(&["b"]), "sin"));
    refused("", "rename to a bad name", &src, rename(node(&["b"]), "Two"));
    let op = rename(path(&["g", "ring"]), "petals");
    assert!(e::remap(&op, &path(&["g", "ring", "u"])) == Some(path(&["g", "petals", "u"])), "rename remaps nested layout keys");
    assert!(e::remap(&op, &path(&["g", "other"])) == Some(path(&["g", "other"])), "rename leaves other keys");
    assert!(e::remap(&Op::Hoist { node: path(&["g", "ring", "k"]) }, &path(&["g", "ring", "k"])) == Some(path(&["g", "k"])), "hoist remaps");
    assert!(e::remap(&Op::DeleteNodes { nodes: vec![path(&["g", "a"])] }, &path(&["g", "a"])).is_none(), "delete drops layout keys");
}

#[test]
fn part7_functions_and_macros() {
    same(
        "make lambda",
        &base(),
        Op::MakeLocalFn { nodes: vec![node(&["b"])] },
        &g("(let* [a (sop/uv_sphere :radius 0.5) b_fn (fn [in_a] (sop/transform in_a :translate [1 2 3])) b (b_fn a) c (sop/subdivide b :iterations 1)] c)"),
    );
    let made = [
        "(workspace w\n  (defmacro shifted [dx a] `(sop/transform ~a :translate [~dx 2 3]))\n",
        "  (graph g :context sop [(n : int 4)] (let* [a (sop/uv_sphere :radius 0.5) b (shifted 1 a) c (sop/subdivide b :iterations 1)] c)))",
    ]
    .concat();
    same("make macro", &base(), Op::MakeMacro { nodes: vec![node(&["b"])], name: "shifted".into(), holes: vec![(vec![3, 0], "dx".into())] }, &made);
    same(
        "inline macro",
        &made,
        Op::InlineMacro { node: node(&["b"]) },
        &[
            "(workspace w\n  (defmacro shifted [dx a] `(sop/transform ~a :translate [~dx 2 3]))\n",
            "  (graph g :context sop [(n : int 4)] (let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)))",
        ]
        .concat(),
    );
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let span = rays_diagnostic::Span { start: 0, finish: 0 };
    let mut children = Vec::new();
    for (text, keyword) in [("sop/transform", false), ("a", false), ("translate", true)] {
        let name = b.intern(text);
        children.push(b.push(0, if keyword { Node::Kw(name) } else { Node::Sym(name) }, span.clone(), None));
    }
    let numbers: Vec<_> = ["1", "2", "3"].iter().map(|text| {
        let name = b.intern(text);
        b.push(0, Node::Num(name), span.clone(), None)
    }).collect();
    let numbers = b.range(&numbers);
    children.push(b.push(0, Node::Vec(numbers), span.clone(), None));
    let children = b.range(&children);
    let root = b.push(0, Node::List(children), span, None);
    let lits = e::literals(&b.freeze().handle(root));
    assert_eq!(lits.iter().map(|(p, _)| p.clone()).collect::<Vec<_>>(), vec![vec![3, 0], vec![3, 1], vec![3, 2]], "literals of an expression");
    refused("", "macro name taken", &base(), Op::MakeMacro { nodes: vec![node(&["b"])], name: "range".into(), holes: vec![] });
    // several bindings: their own names become fresh names in the template
    let two = g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a) c (sop/subdivide b)] c)");
    let out = apply(&two, Op::MakeMacro { nodes: vec![node(&["a"]), node(&["b"])], name: "pair".into(), holes: vec![(vec![1, 1, 2], "r".into())] });
    assert!(has(&out, "a#"), "multi-binding macro uses fresh names:\n{out}");
}

// W9: what the make-macro dialog answers is one Make_macro, and its draft lists the holes' candidates
#[test]
fn dialog() {
    let src = parse(&base());
    let draft = e::macro_draft(&src, &[node(&["b"])]).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    assert!(
        draft.literals.iter().map(|(p, _)| p.clone()).collect::<Vec<_>>() == vec![vec![3, 0], vec![3, 1], vec![3, 2]] && draft.free == vec!["a".to_owned()] && draft.name == "b_tpl",
        "the draft of b"
    );
    let op = e::macro_op(&draft, &[node(&["b"])], "shifted", &[(true, "dx".into()), (false, "dy".into()), (true, "dz".into())]);
    match &op {
        Op::MakeMacro { nodes, name, holes } => assert!(
            *nodes == vec![node(&["b"])] && name == "shifted" && *holes == vec![(vec![3, 0], "dx".to_owned()), (vec![3, 2], "dz".to_owned())],
            "unticked literals are not holes"
        ),
        _ => panic!("unticked literals are not holes"),
    }
    let out = apply(&base(), op);
    assert!(has(&out, "(defmacro shifted [dx dz a]") && has(&out, "b (shifted 1 3 a)"), "the dialog's macro:\n{out}");
    match e::macro_draft(&src, &[node(&["b"]), node(&["@result"])]) {
        Err(d) => assert_eq!(d.code, "E_EDIT", "a result cannot be templated"),
        Ok(_) => panic!("a result was templated"),
    }
    // notes: typing merges into one history entry per node
    let note = |n: &str, t: &str| Op::SetNote { node: node(&[n]), text: t.to_owned() };
    assert!(
        e::gesture(&note("b", "x")) == e::gesture(&note("b", "xy")) && e::gesture(&note("b", "x")).is_some() && e::gesture(&note("b", "x")) != e::gesture(&note("c", "x")),
        "note typing is one gesture per node"
    );
}

#[test]
fn part8_bypass_and_notes() {
    let by = apply(&base(), Op::ToggleBypass { node: node(&["b"]) });
    assert!(
        norm(&by) == norm(&g("(let* [a (sop/uv_sphere :radius 0.5) b ^:bypass (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)")),
        "bypass on"
    );
    same("bypass off", &by, Op::ToggleBypass { node: node(&["b"]) }, &base());
    refused("", "bypass a scope", &g("(let* [a (let* [x 1] x)] (sop/uv_sphere))"), Op::ToggleBypass { node: node(&["a"]) });
    let noted = apply(&base(), Op::SetNote { node: node(&["b"]), text: "the moved one\nsecond line".into() });
    assert!(
        norm(&noted)
            == norm(&g("(let* [a (sop/uv_sphere :radius 0.5)\n           ; the moved one\n           ; second line\n           b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)")),
        "note"
    );
    same("note removed", &noted, Op::SetNote { node: node(&["b"]), text: " ".into() }, &base());
    // a note travels with its binding through a rename and a reorder
    let renamed = apply(&noted, rename(node(&["b"]), "moved"));
    assert!(has(&renamed, "; the moved one\n"), "note survives rename");
    let fwd = g("(let* [a (sop/uv_sphere)\n           ; keep me\n           c (sop/subdivide a)\n           b (sop/transform a)] c)");
    let moved = apply(&fwd, connect(node(&["c"]), ArgKey::Pos(0), "b", false));
    assert!(
        norm(&moved) == norm(&g("(let* [a (sop/uv_sphere) b (sop/transform a)\n           ; keep me\n           c (sop/subdivide b)] c)")),
        "note follows its binding on reorder"
    );
    let root_note = apply(&base(), Op::SetNote { node: path(&["g"]), text: "the graph".into() });
    assert!(has(&root_note, "; the graph\n  (graph g"), "graph note prints");
    let with_result = apply(&base(), Op::SetNote { node: node(&["@result"]), text: "out".into() });
    assert!(has(&with_result, "; out\n      c)"), "result note");
}

#[test]
fn part9_lists_and_records() {
    let list = v("(let* [xs (list 0.2 0.4 0.6) k (count xs)] k)");
    let add = || Op::AddItem { node: node(&["xs"]) };
    same("list continues a float step", &list, add(), &v("(let* [xs (list 0.2 0.4 0.6 0.8) k (count xs)] k)"));
    same("list continues an int step", &v("(let* [xs (list 1 2 3) k (count xs)] k)"), add(), &v("(let* [xs (list 1 2 3 4) k (count xs)] k)"));
    same("list of one repeats", &v("(let* [xs (list 5) k (count xs)] k)"), add(), &v("(let* [xs (list 5 5) k (count xs)] k)"));
    same(
        "text adds a part",
        &v("(let* [s (str \"a\" n) k (count (list 1))] k)"),
        Op::AddItem { node: node(&["s"]) },
        &v("(let* [s (str \"a\" n \" \") k (count (list 1))] k)"),
    );
    same("move up", &list, Op::MoveItem { node: node(&["xs"]), pos: 2 }, &v("(let* [xs (list 0.2 0.6 0.4) k (count xs)] k)"));
    refused("", "move the first", &list, Op::MoveItem { node: node(&["xs"]), pos: 0 });
    let record = g("(let* [r {:size 0.5 :count 6} a (sop/uv_sphere :radius r.size)] a)");
    same(
        "record field",
        &record,
        Op::AddField { node: node(&["r"]), name: "depth".into(), value: num("2") },
        &g("(let* [r {:size 0.5 :count 6 :depth 2} a (sop/uv_sphere :radius r.size)] a)"),
    );
    refused("E_EDIT", "an existing field", &record, Op::AddField { node: node(&["r"]), name: "size".into(), value: num("2") });
    same(
        "a new output of values",
        &v("(let* [r (values :a 1)] r.a)"),
        Op::AddField { node: node(&["r"]), name: "b".into(), value: num("0") },
        &v("(let* [r (values :a 1 :b 0)] r.a)"),
    );
    same(
        "keyword field",
        &base(),
        Op::AddField { node: node(&["b"]), name: "scale".into(), value: num("2") },
        &g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3] :scale 2) c (sop/subdivide b :iterations 1)] c)"),
    );
    same(
        "record field scrub",
        &record,
        set_arg(node(&["r"]), ArgKey::Field("size".into()), vec![], num("0.9")),
        &g("(let* [r {:size 0.9 :count 6} a (sop/uv_sphere :radius r.size)] a)"),
    );
}

#[test]
fn part9_items_follow_keywords_in_a_scene_merge() {
    // the items are the positional arguments wherever the keywords are written, and :skip follows them
    let merged = "(workspace w\n  (graph g :context sop (sop/box))\n  (graph scene :context scene\n    (let* [a (scene/geometry (ref g)) b (scene/light) c (scene/light) all (scene/merge :skip [0] a b c)] all)))";
    let moved = apply(merged, Op::MoveItem { node: path(&["scene", "all"]), pos: 1 });
    assert!(has(&moved, "(scene/merge :skip [1] b a c)"), "move up after a keyword\n{moved}");
    refused("", "move past the items", merged, Op::MoveItem { node: path(&["scene", "all"]), pos: 3 });
}

#[test]
fn part10_add_and_delete() {
    same(
        "add node",
        &base(),
        Op::AddNode { scope: path(&["g"]), name: "extra".into(), expr: call("sop/uv_sphere") },
        &g("(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1) extra (sop/uv_sphere)] c)"),
    );
    same(
        "add node to a bare body",
        &g("(sop/uv_sphere)"),
        Op::AddNode { scope: path(&["g"]), name: "x".into(), expr: num("1") },
        &g("(let* [x 1] (sop/uv_sphere))"),
    );
    refused("", "add a used name", &base(), Op::AddNode { scope: path(&["g"]), name: "a".into(), expr: num("1") });
    same(
        "delete an unused node",
        &g("(let* [a (sop/uv_sphere) u (sop/uv_sphere :radius 2) b (sop/transform a)] b)"),
        Op::DeleteNodes { nodes: vec![node(&["u"])] },
        &g("(let* [a (sop/uv_sphere) b (sop/transform a)] b)"),
    );
    same(
        "delete a used node dissolves it into its reader",
        &base(),
        Op::DeleteNodes { nodes: vec![node(&["a"])] },
        &g("(let* [b (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)"),
    );
    // a duplicated loop: its inner names are bound in both copies, and each copy's unused node can go
    let twins = g("(let* [r1 (for [i (range 2)] (let* [u (sop/uv_sphere) v (sop/uv_sphere)] v)) r2 (for [i (range 2)] (let* [u (sop/uv_sphere) v (sop/uv_sphere)] v)) m (sop/merge r1 r2)] m)");
    assert!(
        !has(&apply(&twins, Op::DeleteNodes { nodes: vec![path(&["g", "r1", "u"])] }), "r1 (for [i (range 2)] (let* [u"),
        "an unused node of a duplicated loop was not deleted"
    );
    // a node its own scope reads dissolves into that scope only; the twin keeps its own
    let gone = apply(&twins, Op::DeleteNodes { nodes: vec![path(&["g", "r2", "v"])] });
    assert!(
        has(&gone, "r2 (for [i (range 2)] (let* [u (sop/uv_sphere)] (sop/uv_sphere)))") && has(&gone, "v (sop/uv_sphere)] v)"),
        "a node its own scope reads did not dissolve into that scope only: {gone}"
    );
    same(
        "delete a chain",
        &g("(let* [a (sop/uv_sphere) u (sop/transform a) v (sop/transform u) b (sop/transform a)] b)"),
        Op::DeleteNodes { nodes: vec![node(&["u"]), node(&["v"])] },
        &g("(let* [a (sop/uv_sphere) b (sop/transform a)] b)"),
    );
}

#[test]
fn part10b_duplicate() {
    let three = g("(let* [a (sop/uv_sphere) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b)] c)");
    same(
        "duplicate one",
        &three,
        Op::Duplicate { nodes: vec![node(&["b"])] },
        &g("(let* [a (sop/uv_sphere) b (sop/transform a :translate [1 2 3]) b_2 (sop/transform a :translate [1 2 3]) c (sop/subdivide b)] c)"),
    );
    same(
        "duplicate a chain: the copies read each other",
        &three,
        Op::Duplicate { nodes: vec![node(&["b"]), node(&["c"])] },
        &g("(let* [a (sop/uv_sphere) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b) b_2 (sop/transform a :translate [1 2 3]) c_2 (sop/subdivide b_2)] c)"),
    );
    same(
        "duplicate twice takes fresh names",
        &g("(let* [a (sop/uv_sphere) a_2 (sop/uv_sphere) b (sop/transform a)] b)"),
        Op::Duplicate { nodes: vec![node(&["a"])] },
        &g("(let* [a (sop/uv_sphere) a_3 (sop/uv_sphere) a_2 (sop/uv_sphere) b (sop/transform a)] b)"),
    );
    refused("", "duplicate the result", &three, Op::Duplicate { nodes: vec![node(&["@result"])] });
    refused(
        "",
        "duplicate across scopes",
        &g("(let* [ring (for [i (range n)] (let* [u (sop/uv_sphere)] u)) m (sop/merge ring)] m)"),
        Op::Duplicate { nodes: vec![node(&["ring"]), path(&["g", "ring", "u"])] },
    );
    assert!(
        e::duplicated(&parse(&three), &[node(&["b"]), node(&["c"])]) == vec![node(&["b_2"]), node(&["c_2"])] && e::duplicated(&parse(&three), &[node(&["@result"])]).is_empty(),
        "the copies' paths"
    );
    assert_eq!(e::label(&Op::Duplicate { nodes: vec![] }), "Duplicate", "duplicate label");
    // names for a batch: each joins the used ones, so the next is distinct
    let mut used = vec!["box".to_owned()];
    let first = e::fresh_among(&mut used, "Box");
    assert!(first == "box_2" && e::fresh_among(&mut used, "sop/box") == "box_3" && used.contains(&"box_3".to_owned()), "names of one batch are not distinct");
}

#[test]
fn part10b_scene_and_world_graphs() {
    // a keyword before the positional argument is still an argument of the call
    let mixed = "(workspace w\n  (graph world :context world\n    (world/sun :name \"w\" (world/sky :name \"s\"))))";
    assert!(
        has(&apply(mixed, set_arg(path(&["world", "@result"]), ArgKey::Pos(0), vec![], call("world/sky"))), "(world/sky)"),
        "a positional argument after a keyword pair is found"
    );
    // a scene object joins the merge, a World layer goes on top of the stack, deleting takes them out
    let scene = "(workspace w\n  (graph g :context sop (sop/box))\n  (graph scene :context scene\n    (let* [a (scene/geometry (ref g)) all (scene/merge a)] all)))";
    let added = apply(scene, Op::AddNode { scope: path(&["scene"]), name: "lamp".into(), expr: call("scene/light") });
    assert!(has(&added, "(scene/merge a lamp)") && has(&added, "lamp (scene/light)"), "a new object did not join the merge\n{added}");
    let removed = apply(&added, Op::DeleteNodes { nodes: vec![path(&["scene", "lamp"])] });
    assert!(has(&removed, "(scene/merge a)") && !has(&removed, "lamp"), "a deleted object stayed in the merge");
    let single = "(workspace w\n  (graph g :context sop (sop/box))\n  (graph scene :context scene (scene/geometry (ref g))))";
    assert!(
        has(&apply(single, Op::AddNode { scope: path(&["scene"]), name: "lamp".into(), expr: call("scene/light") }), "(scene/merge (scene/geometry (ref g)) lamp)"),
        "a scene that is one object becomes a merge"
    );
    let world = "(workspace w\n  (graph world :context world\n    (let* [sky (world/sky)] sky)))";
    let layered = apply(world, Op::AddNode { scope: path(&["world"]), name: "sun".into(), expr: call("world/sun") });
    assert!(has(&layered, "sun (world/sun sky)") && has(&layered, "sun)))"), "a new layer is not on top\n{layered}");
    let closed = apply(&layered, Op::DeleteNodes { nodes: vec![path(&["world", "sun"])] });
    assert!(has(&closed, "sky)))") && !has(&closed, "world/sun"), "the stack did not close over a deleted layer\n{closed}");
    let graph = |name: &str| {
        let mut b = flow_syntax::arena::SyntaxBuilder::new();
        let span = rays_diagnostic::Span { start: 0, finish: 0 };
        let mut children = Vec::new();
        for (text, keyword) in [("graph", false), (name, false), ("context", true), ("sop", false)] {
            let name = b.intern(text);
            children.push(b.push(0, if keyword { Node::Kw(name) } else { Node::Sym(name) }, span.clone(), None));
        }
        let name = b.intern("sop/box");
        let head = b.push(0, Node::Sym(name), span.clone(), None);
        let body = b.range(&[head]);
        children.push(b.push(0, Node::List(body), span.clone(), None));
        let children = b.range(&children);
        let root = b.push(0, Node::List(children), span, None);
        b.freeze().handle(root)
    };
    assert!(
        has(&apply(scene, Op::SetGraph { name: "g".into(), form: graph("g") }), "(graph g :context sop")
            && has(&apply("(workspace w (graph g :context sop (sop/box)))", Op::SetGraph { name: "extra".into(), form: graph("extra") }), "(graph extra"),
        "Set_graph replaces a graph or appends one"
    );
    refused("", "Set_graph with another name", scene, Op::SetGraph { name: "other".into(), form: graph("g") });
}

#[test]
fn part11_atomic_refusals_and_labels() {
    // a refused edit changes nothing
    let before = norm(&base());
    assert!(e::apply(&catalog(), &parse(&base()), &rename(node(&["b"]), "a")).is_err(), "expected a refusal");
    assert_eq!(canon(&parse(&base())), before, "source unchanged after a refusal");
    use crate::layout_edit as layout;
    for active in ["5","-1"] {
        let text=format!("(workspace w (graph editor :context editor (let* [a (ui/graph) b (ui/lisp)] (ui/workspace (ui/switch a b :active {active})))))");
        let forms=parse(&text);
        for op in [
            layout::Op::LayoutRemove{graph:"editor".into()},
            layout::Op::LayoutNew{graph:"editor".into()},
            layout::Op::LayoutWindow{graph:"editor".into(),panel:parse("(ui/inspector)").remove(0)}
        ] {
            let diagnostic=layout::apply_checked(&[],false,None,&catalog(),&forms,&op).err().expect("invalid active layout accepted");
            assert!(diagnostic.message.contains("not one of the switch's layouts"),"{}",diagnostic.message);
        }
    }
    for (op, label) in [
        (Op::Wrap { nodes: vec![], loop_: Loop::For }, "Repeat"),
        (Op::Wrap { nodes: vec![], loop_: Loop::Fold }, "Iterate"),
        (Op::Unfold { node: vec![], key: ArgKey::Whole, sub: vec![] }, "Unfold"),
        (Op::MakeMacro { nodes: vec![], name: String::new(), holes: vec![] }, "Make macro"),
        (rename(vec![], ""), "Rename"),
        (Op::Hoist { node: vec![] }, "Move out"),
        (Op::ToggleBypass { node: vec![] }, "Bypass"),
        (Op::FoldInto { node: vec![] }, "Fold"),
    ] {
        assert_eq!(e::label(&op), label, "label {label}");
    }
}

#[test]
fn part12_the_fixtures_notes_survive_edits_round_trip() {
    let names = ["bloom", "facade", "garland", "kit", "orrery", "rosette", "sunflower", "tiles", "tree", "tunnel", "variations", "wave"];
    for name in names {
        let text = case_text(name);
        let forms = parse(&text);
        let [workspace] = &forms[..] else { panic!("{name}: shape") };
        assert!(matches!(workspace.node, Node::List(_)), "{name}: shape");
        let graph = workspace.children().iter().skip(2).find_map(|&id| {
            let form = workspace.child(id);
            if s::head(&form) != Some("graph") { return None; }
            let name = *form.children().get(1)?;
            match form.arena().row(name).node {
                Node::Sym(name) => Some(form.name(name).to_owned()),
                _ => None,
            }
        }).expect("a graph");
        // a note on the graph and back leaves the text unchanged
        let noted = e::apply(&catalog(), &forms, &Op::SetNote { node: vec![graph.clone()], text: "edited".into() }).unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
        assert!(canon(&noted) != canon(&forms), "{name}: note prints");
        let back = e::apply(&catalog(), &noted, &Op::SetNote { node: vec![graph], text: String::new() }).unwrap_or_else(|d| panic!("{name}: {}", rays_diagnostic::to_string(&d)));
        assert!(canon(&back) == canon(&forms), "{name}: note round trip");
    }
    // bloom: rename a graph binding and back; the comments survive
    let bloom = parse(&case_text("bloom"));
    let renamed = e::apply(&catalog(), &bloom, &rename(path(&["flower", "bloom"]), "blossom")).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    assert!(canon(&renamed) != canon(&bloom), "bloom renamed");
    let back = e::apply(&catalog(), &renamed, &rename(path(&["flower", "blossom"]), "bloom")).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    assert!(canon(&back) == canon(&bloom), "bloom rename round trip");
}

#[test]
fn part13_move_binding_wrap_in_map_filter_inline_defn() {
    let order = v("(let* [a 1 b 2 c (+ a b)] c)");
    same("move down", &order, Op::MoveBinding { node: node(&["a"]), by: 1 }, &v("(let* [b 2 a 1 c (+ a b)] c)"));
    same("move up", &order, Op::MoveBinding { node: node(&["b"]), by: -1 }, &v("(let* [b 2 a 1 c (+ a b)] c)"));
    refused("", "move past a reader", &order, Op::MoveBinding { node: node(&["c"]), by: -1 });
    refused("", "move off the end", &order, Op::MoveBinding { node: node(&["a"]), by: -1 });
    same(
        "wrap in map",
        &v("(let* [a (+ n 1) r (* 0.5 (count a))] r)"),
        Op::Wrap { nodes: vec![node(&["a"])], loop_: Loop::Map },
        &v("(let* [items (range 6) a (map (fn [i] (+ n 1)) items) r (* 0.5 (count a))] r)"),
    );
    same(
        "wrap in filter",
        &v("(let* [a (< n 1) r (* 0.5 (count a))] r)"),
        Op::Wrap { nodes: vec![node(&["a"])], loop_: Loop::Filter },
        &v("(let* [items (range 6) a (filter (fn [i] (< n 1)) items) r (* 0.5 (count a))] r)"),
    );
    same(
        "wrap in map over a selected source",
        &v("(let* [xs (range 3) a (+ xs 1) r (count a)] r)"),
        Op::Wrap { nodes: vec![node(&["xs"]), node(&["a"])], loop_: Loop::Map },
        &v("(let* [xs (range 3) a (map (fn [i] (+ i 1)) xs) r (count a)] r)"),
    );
    let twice = "(workspace w\n  (defn twice :context value [(x : int) (y : int)] (- x (* y 2)))\n  (graph g :context value [(n : int 4)] (let* [y 3 a (twice y n)] a)))";
    same(
        "inline defn",
        twice,
        Op::InlineDefn { node: node(&["a"]) },
        "(workspace w\n  (defn twice :context value [(x : int) (y : int)] (- x (* y 2)))\n  (graph g :context value [(n : int 4)] (let* [y 3 a (- y (* n 2))] a)))",
    );
    refused("", "inline a non-function", twice, Op::InlineDefn { node: node(&["y"]) });
    let _ = (p::NODE_WIDTH, manifest::VERSION);
}

#[test]
fn a_raising_rewrite_is_a_refusal() {
    // an exception inside a rewrite (here an index on a [for] with no body) is a refusal too
    let broken = parse(&g("(let* [a (for)] a)"));
    match e::apply(&catalog(), &broken, &set_arg(node(&["a"]), kw("skip"), vec![], num("1"))) {
        Err(d) => assert!(d.code == "E_EDIT" && has(&d.message, "The edit failed"), "a raising rewrite: {}", d.message),
        Ok(_) => panic!("a raising rewrite was accepted"),
    }
    assert_eq!(canon(&broken), norm(&g("(let* [a (for)] a)")), "source unchanged after a raising rewrite");
}
