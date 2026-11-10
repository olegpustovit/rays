use crate::flow_edit::{apply_checked, apply_checked_with, ArgKey, Loop, Op};
use crate::projection as p;
use crate::test_support::*;
use flow_syntax::syntax::Syntax;
use std::sync::Arc;

#[test]
fn same_graph_keys_on_the_shared_ops() {
    let src = parse("(workspace w (graph g :context host (let* [a 1.0] a)) (graph other :context host (let* [b 2.0] b)))");
    let rename = Op::Rename { node: path(&["other", "b"]), to_: "moved".into() };
    // Canonical empty operator sets retain graph identity.
    assert!(p::same_graph(&check(&src), &check(&src), "g"));
    // the editor's ops: one list, checked into the base workspace
    let mut user = flow_check::op::find(&[], "array/float", &flow_syntax::context::value()).unwrap().clone();
    user.name = "test/float".into();
    let ops = Arc::new(vec![user]);
    let base = match flow_check::workspace::check(&ops, false, false, None, &catalog(), &src) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show(&ds)),
    };
    // an edit that leaves the ops alone keeps their Arc (the editor's pipeline): `g` is unchanged, `other` is not
    let (_, mut edited) = apply_checked(&catalog(), &src, &rename).unwrap();
    edited.ops = base.ops.clone();
    assert!(p::same_graph(&base, &edited, "g"));
    assert!(!p::same_graph(&base, &edited, "other"));
    // Separately checked operator owners are distinct even when their contents are equal.
    let (_, fresh) = apply_checked_with(&Arc::new((*ops).clone()), false, None, &catalog(), &src, &rename).unwrap();
    assert!(!p::same_graph(&base, &fresh, "g"));
}

fn source(body: &str) -> Vec<Syntax> {
    parse(&format!("(workspace w (graph g :context host (let* {})))", body))
}

fn edit(src: &[Syntax], op: Op) -> Vec<Syntax> {
    apply_checked(&catalog(), src, &op).unwrap().0
}

fn connect(src: &[Syntax], node: &str, name: &str) -> Vec<Syntax> {
    edit(src, Op::Connect { node: path(&["g", node]), key: ArgKey::Pos(0), src: name.into(), iter: false })
}

#[test]
fn unnamed_math_nodes_fold_into_their_reader() {
    let src = source("[sum (+ 1.0 2.0) x (max 0.0 0.0) y (max 0.0 0.0)] x");
    // an unnamed math node with one reader is written in it
    let one = connect(&src, "x", "sum");
    assert!(has(&text_of(&one), "(max (+ 1.0 2.0) 0.0)") && !has(&text_of(&one), "sum"));
    // a second reader makes it a binding again
    let two = connect(&one, "y", "x#0");
    assert!(has(&text_of(&two), "sum_2 (+ 1.0 2.0)") && has(&text_of(&two), "(max sum_2 0.0)"));
    // a node the user named stays a binding
    let named = connect(&source("[half (+ 1.0 2.0) x (max 0.0 0.0)] x"), "x", "half");
    assert!(has(&text_of(&named), "half (+ 1.0 2.0)") && has(&text_of(&named), "(max half 0.0)"));
    // disconnecting keeps the expression as a binding
    let off = edit(&one, Op::Disconnect { node: path(&["g", "x"]), key: ArgKey::Pos(0), fallback: Some(parse("0.5").remove(0)) });
    assert!(has(&text_of(&off), "sum_2 (+ 1.0 2.0)") && has(&text_of(&off), "(max 0.5 0.0)"));
    // deleting the second reader leaves one: it folds
    let del = edit(&two, Op::DeleteNodes { nodes: vec![path(&["g", "y"])] });
    assert!(has(&text_of(&del), "(max (+ 1.0 2.0) 0.0)") && !has(&text_of(&del), "sum"));
    // the projection draws the open expression as cards, the folded one as a chip
    let w = check(&one);
    let card = |s: &p::Scope| p::find(s, &path(&["g", "x#0"]));
    assert!(card(&p::of_graph(&catalog(), &w, "g").unwrap()).is_none());
    let n = card(&p::of_graph_with(&|_| false, &catalog(), &w, "g").unwrap()).unwrap();
    assert!(n.expr && n.head == "+");
}

#[test]
fn a_deleted_node_dissolves_into_its_readers() {
    // a deleted node with readers dissolves into them; it is never refused
    let src = source("[a (+ 1.0 2.0) b (max a 0.0) c (max a 1.0)] (max b c)");
    let del = edit(&src, Op::DeleteNodes { nodes: vec![path(&["g", "a"])] });
    let t = text_of(&del);
    assert!(has(&t, "b (max (+ 1.0 2.0) 0.0)") && has(&t, "c (max (+ 1.0 2.0) 1.0)") && !has(&t, " a "));
}

#[test]
fn local_fn_refusals() {
    // l: a function of a node that reads nothing, or of a local function call, is refused
    let refused = |src: &[Syntax], nodes: Vec<flow_check::workspace::Path>| apply_checked(&catalog(), src, &Op::MakeLocalFn { nodes }).is_err();
    assert!(refused(&source("[a (+ 1.0 2.0)] a"), vec![path(&["g", "a"])]));
    let made = edit(&source("[a 1.0 b (+ a 2.0)] b"), Op::MakeLocalFn { nodes: vec![path(&["g", "b"])] });
    assert!(has(&text_of(&made), "b_fn (fn [in_a]") && has(&text_of(&made), "b (b_fn a)"));
    assert!(refused(&made, vec![path(&["g", "b"])]));
}

#[test]
fn a_local_fn_call_is_wired_from_it() {
    // the call of a local fn is wired from it
    let made = edit(&source("[a 1.0 b (+ a 2.0)] b"), Op::MakeLocalFn { nodes: vec![path(&["g", "b"])] });
    let w = check(&made);
    let n = p::find(&p::of_graph(&catalog(), &w, "g").unwrap(), &path(&["g", "b"])).unwrap();
    assert!(n.rows.iter().any(|r| r.label == "fn" && p::sources(r) == vec!["b_fn".to_owned()]));
    // and its inputs are named by the fn's parameters
    assert!(n.rows.iter().any(|r| r.label == "in_a" && p::sources(r) == vec!["a".to_owned()]));
}

#[test]
fn repeat_in_a_drawing_graph_merges() {
    // Repeat in a drawing graph collects the drawings and merges them
    let src = parse("(workspace w (graph g :context draw (let* [c (draw/circle [0 0 0] 5)] c)))");
    let r = edit(&src, Op::Wrap { nodes: vec![path(&["g", "c"])], loop_: Loop::For });
    assert!(has(&text_of(&r), "c (draw/merge c_each)"));
}

#[test]
fn chain_steps_trade_places() {
    // a chain step trades places along the chain input; deleting a step hands its place to that input
    let src = parse("(workspace w (graph g :context draw (draw/translate [1 2 0] (draw/rotate 0.3 (draw/circle [0 0 0] 5)))))");
    let ed = |op: Op| edit(&src, op);
    let flat = |t: String| t.replace('\n', " ").split(' ').filter(|x| !x.is_empty()).collect::<Vec<_>>().join(" ");
    let swapped = flat(text_of(&ed(Op::MoveStep { node: path(&["g", "@result#1"]), earlier: false })));
    assert!(has(&swapped, "(draw/rotate 0.3 (draw/translate [1 2 0] (draw/circle [0 0 0] 5)))"));
    assert_eq!(flat(text_of(&ed(Op::MoveStep { node: path(&["g", "@result"]), earlier: true }))), swapped);
    assert!(apply_checked(&catalog(), &src, &Op::MoveStep { node: path(&["g", "@result#1#1"]), earlier: true }).is_err());
    let gone = flat(text_of(&ed(Op::DeleteNodes { nodes: vec![path(&["g", "@result#1"])] })));
    assert!(has(&gone, "(draw/translate [1 2 0] (draw/circle [0 0 0] 5))"));
}

#[test]
fn the_context_symbol_is_a_card() {
    // the context symbol [t] written in an input is a card of its own, wired to the row; the
    // text is unchanged and disconnecting the wire writes the fallback literal
    let src = parse("(workspace w (graph g :context host (let* [s (sin (* t 0.5))] s)))");
    let w = check(&src);
    let scope = p::of_graph_with(&|_| false, &catalog(), &w, "g").unwrap();
    let t = p::find(&scope, &path(&["g", "s#0#0"])).unwrap();
    assert!(t.head == "t" && t.expr && t.rows.is_empty());
    let mul = p::find(&scope, &path(&["g", "s#0"])).unwrap();
    let a = mul.rows[0].clone();
    assert!(a.chip == p::Chip::Name("s#0#0".into()) && p::sources(&a) == vec!["s#0#0".to_owned()]);
    assert!(has(&text_of(&src), "(sin (* t 0.5))"));
    let off = edit(&src, Op::Disconnect { node: path(&["g", "s#0"]), key: ArgKey::Pos(0), fallback: Some(parse("0.5").remove(0)) });
    assert!(has(&text_of(&off), "(sin (* 0.5 0.5))"));
}

#[test]
fn a_call_in_a_vector_cell_is_a_node() {
    // a call written in a cell of a vector literal is a node of its own, wired to the row
    let src = source("[a (+ [0.0 (* 2.0 3.0)] [1.0 1.0])] a");
    let w = check(&src);
    let scope = p::of_graph(&catalog(), &w, "g").unwrap();
    let a = p::find(&scope, &path(&["g", "a"])).unwrap();
    let row = a.rows[0].clone();
    assert!(row.cells == vec![(1, "a#0#c1".to_owned())] && p::sources(&row) == vec!["a#0#c1".to_owned()]);
    let mul = p::find(&scope, &path(&["g", "a#0#c1"])).unwrap();
    assert!(mul.head == "*" && mul.expr);
    // the call's own inputs edit in place
    let set = edit(&src, Op::SetArg { node: path(&["g", "a#0#c1"]), key: ArgKey::Pos(0), sub: vec![], value: parse("7.0").remove(0) });
    assert!(has(&text_of(&set), "[0.0 (* 7.0 3.0)]"));
    // taking the wire off sets the cell and keeps the call as a binding
    let off = edit(&src, Op::Disconnect { node: path(&["g", "a#0"]), key: ArgKey::Cell(1), fallback: Some(parse("0.5").remove(0)) });
    assert!(has(&text_of(&off), "[0.0 0.5]") && has(&text_of(&off), "(* 2.0 3.0)"));
    // naming it binds it where the vector read it
    let named = edit(&src, Op::Rename { node: path(&["g", "a#0#c1"]), to_: "twice".into() });
    assert!(has(&text_of(&named), "twice (* 2.0 3.0)") && has(&text_of(&named), "[0.0 twice]"));
}

// Gestures are applied, rechecked and written back through the public graph-edit path.
#[test]
fn gesture_smoke() {
    let src = source("[a 1.0 b (+ a 2.0) c (* b 3.0) l (list 1 2 3)] c");
    let tx = |op: Op| text_of(&edit(&src, op));
    let t = tx(Op::Rename { node: path(&["g", "b"]), to_: "bee".into() });
    assert!(has(&t, "bee (+ a 2.0)") && has(&t, "c (* bee 3.0)"));
    let t = tx(Op::SetNote { node: path(&["g", "b"]), text: "hello\nworld".into() });
    assert!(has(&t, "; hello\n           ; world\n           b (+ a 2.0)"));
    assert!(has(&tx(Op::ToggleBypass { node: path(&["g", "b"]) }), "b ^:bypass (+ a 2.0)"));
    assert!(has(&tx(Op::Duplicate { nodes: vec![path(&["g", "b"])] }), "b_2 (+ a 2.0)"));
    assert!(has(&tx(Op::MoveBinding { node: path(&["g", "l"]), by: -1 }), "l (list 1 2 3)\n           c (* b 3.0)"));
    assert!(has(&tx(Op::AddItem { node: path(&["g", "l"]) }), "(list 1 2 3 4)"));
    assert!(has(&tx(Op::MoveItem { node: path(&["g", "l"]), pos: 2 }), "(list 1 3 2)"));
    assert!(has(&tx(Op::Wrap { nodes: vec![path(&["g", "b"])], loop_: Loop::For }), "b (sum [i (range 6)] (+ a 2.0))"));
    assert!(has(&tx(Op::Wrap { nodes: vec![path(&["g", "b"])], loop_: Loop::Fold }), "b (fold [prev a] [i (range 4)] (+ prev 2.0))"));
    let make_macro = Op::MakeMacro { nodes: vec![path(&["g", "b"])], name: "twice".into(), holes: vec![(vec![2], "k".into())] };
    let t = tx(make_macro.clone());
    assert!(has(&t, "(defmacro twice [k a]\n    `(+ ~a ~k))") && has(&t, "b (twice 2.0 a)"));
    let make_defn = Op::MakeDefn { nodes: vec![path(&["g", "b"])], name: "plus2".into(), context: "host".into(), params: vec![("a".into(), "float".into())] };
    let t = tx(make_defn.clone());
    assert!(has(&t, "(defn plus2 :context host [(a : float)]\n    (+ a 2.0))") && has(&t, "b (plus2 a)"));
    let u = source("[a 1.0 c (* (+ a 1.0) 3.0)] c");
    let t = text_of(&edit(&u, Op::Unfold { node: path(&["g", "c"]), key: ArgKey::Pos(0), sub: vec![] }));
    assert!(has(&t, "sum_2 (+ a 1.0)") && has(&t, "c (* sum_2 3.0)"));
    let m = edit(&src, make_macro);
    assert!(has(&text_of(&edit(&m, Op::InlineMacro { node: path(&["g", "b"]) })), "b (+ a 2.0)"));
    let d = edit(&src, make_defn);
    assert!(has(&text_of(&edit(&d, Op::InlineDefn { node: path(&["g", "b"]) })), "b (+ a 2.0)"));
    assert!(has(&tx(Op::FoldInto { node: path(&["g", "b"]) }), "c (* (+ a 2.0) 3.0)"));
}

#[test]
fn inferred_context_and_let_alias_keep_projection_and_direct_edits() {
    let src = parse("(workspace demo (graph g (let [radius 12.5] (draw/circle [20 30] radius :fill \"#5aaaf0\"))))");
    let ws = check(&src);
    assert_eq!(ws.graphs[0].context, flow_syntax::context::draw());
    let scope = p::of_graph(&catalog(), &ws, "g").unwrap();
    assert!(find_head("draw/circle", &scope).is_some());
    let (changed, checked) = apply_checked(&catalog(), &src, &Op::Rename { node: path(&["g", "radius"]), to_: "size".into() }).unwrap();
    let text = text_of(&changed);
    assert!(text.contains("(let [size 12.5]") && !text.contains(":context"), "{text}");
    assert_eq!(checked.graphs[0].context, flow_syntax::context::draw());
    assert!(p::of_graph(&catalog(), &checked, "g").is_ok());
    for source in [
        "(workspace demo (graph g (circle 12.5)) (defn circle [(radius : float)] (draw/circle [20 30] radius)))",
        "(workspace demo (graph g (ref picture)) (graph picture (draw/merge)))",
    ] {
        let forms = parse(source);
        let workspace = check(&forms);
        assert_eq!(workspace.graphs[0].context, flow_syntax::context::draw());
        assert!(p::of_graph(&catalog(), &workspace, "g").is_ok());
    }
}
