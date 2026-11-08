// Port of lib/flow_graph/test_domain.ml: each OCaml `let () = ...` block is one #[test].
use crate::edit_base::ArgKey;
use crate::flow_edit::{apply_checked, default_for, remap, Op};
use crate::probe::{self, Options, Summary};
use crate::projection as p;
use crate::test_support::*;
use flow_check::op;
use flow_eval as e;
use flow_eval::Value;
use flow_syntax::context;
use flow_syntax::syntax::{self as s, Node};
use flow_syntax::ty::{self, FnSignature, Ty};
use flow_syntax::lisp;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

const MAX_ARRAY_LENGTH: i64 = (1i64 << 54) - 1;

fn num(n: &str) -> s::Syntax {
    s::make(s::Draft::Num(n.into()))
}
fn set_arg(node: &[String], key: ArgKey, v: s::Syntax) -> Op {
    Op::SetArg { node: node.to_vec(), key, sub: vec![], value: v }
}
fn array_of(t: Ty) -> Ty {
    Ty::Array(Box::new(t))
}
fn lookup(scope: &p::Scope) -> Option<p::Node> {
    find_head("array/nth", scope)
}
fn int_array(v: &[i64]) -> Value {
    Value::Int_array(Arc::new(v.to_vec()))
}

#[test]
fn lookup_keeps_big_integers() {
    let source = parse("(workspace lookup (graph g :context value (let* [table (array/int 3 9007199254740993) picked (array/nth table 1)] picked)))");
    let workspace = check(&source);
    let card = card(&workspace, &["g", "picked"]);
    assert!(card.head == "array/nth" && card.ty == Ty::Int);
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(1), num("2"))).unwrap();
    assert_eq!(result(&edited, 0.0), Value::Int(9007199254740993));
}

#[test]
fn lookup_in_a_map() {
    let source = parse("(workspace lookup (graph g :context host (let* [table (array/range 3 :type int) picked (map (fn [i] (array/nth table i)) (array/range 3 :type int))] picked)))");
    let workspace = check(&source);
    let card = lookup(&pane(&workspace)).unwrap();
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(1), num("2"))).unwrap();
    assert_eq!(result(&edited, 0.0), int_array(&[2, 2, 2]));
}

#[test]
fn lookup_of_text() {
    let source = parse("(workspace lookup (graph g :context host (let* [table (array/text 3 \"é日本😀\") picked (map (fn [(i : int)] (array/nth table i)) (array/range 3 :type int))] picked)))");
    let workspace = check(&source);
    let card = lookup(&pane(&workspace)).unwrap();
    assert_eq!(card.ty, Ty::Text);
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(1), num("2"))).unwrap();
    let t = "é日本😀".as_bytes().to_vec();
    assert_eq!(result(&edited, 0.0), Value::Text_array(Arc::new(vec![t.clone(), t.clone(), t].into())));
}

#[test]
fn checked_arithmetic_records() {
    for (name, args, index) in [("float/mul_checked", "t 2.0", 1), ("float/fma_checked", "t 2.0 1.0", 2)] {
        let source = parse(&format!("(workspace checked (graph g :context value (let* [status ({} {})] status.value)))", name, args));
        let workspace = check(&source);
        let card = card(&workspace, &["g", "status"]);
        assert_eq!(card.ty, Ty::Record(vec![("value".into(), Ty::Float), ("valid".into(), Ty::Bool)]));
        let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(index), num("4.0"))).unwrap();
        assert_eq!(result(&edited, 2.0), Value::Float(8.0));
    }
}

#[test]
fn footer_of_an_int_array() {
    let source = parse("(workspace integers (graph g :context host (let* [v (array/int 1)] v)))");
    let workspace = check(&source);
    let card = card(&workspace, &["g", "v"]);
    let mut probe = probe::make(&recorded(&workspace), Options::default());
    assert_eq!(probe.footer(&card, &[]).value, "1 × int");
}

#[test]
fn footer_of_a_long_text_array() {
    let source = parse("(workspace texts (graph g :context host (let* [v (array/text 5001 \"猫\")] v)))");
    let workspace = check(&source);
    let card = card(&workspace, &["g", "v"]);
    assert_eq!(card.ty, array_of(Ty::Text));
    let default = default_for(&array_of(Ty::Text), "texts", None).unwrap();
    assert_eq!(s::head(&default), Some("array/text"));
    let mut probe = probe::make(&recorded(&workspace), Options::default());
    assert_eq!(probe.footer(&card, &[]).value, "5001 × text");
}

#[test]
fn vector_literals_edit_a_component() {
    for (literal, ty, width, expected) in [("[1 2]", Ty::Vec2, 2usize, "[1 9.5]"), ("[1 2 3 4]", Ty::Vec4, 4usize, "[1 2 3 9.5]")] {
        let source = parse(&format!("(workspace vectors (graph g :context host (let* [v {}] v)))", literal));
        let workspace = check(&source);
        let card = card(&workspace, &["g", "v"]);
        assert!(card.ty == ty && card.rows[0].ty == Some(ty.clone()));
        let default = default_for(&ty, "v", None).unwrap();
        match &default.node {
            Node::Vec(cs) => assert_eq!(default.range(*cs).len(), width),
            _ => panic!("default is not a vector"),
        }
        let (_, edited) = apply_checked(
            &catalog(),
            &source,
            &Op::SetArg { node: card.path.clone(), key: ArgKey::Whole, sub: vec![width as i64 - 1], value: num("9.5") },
        )
        .unwrap();
        let mut probe = probe::make(&recorded(&edited), Options::default());
        assert_eq!(probe.footer(&card, &[]).value, expected);
    }
}

#[test]
fn live_branches_and_wrapping() {
    let text = "(workspace live (graph g :context value (let* [x (if (< t 1) 1 2) y (cond (< t 0) 1 (< t 1) 2 :else 3) z (case (< t 1) 0 4 true 5 :else 6)] (+ x (+ y z)))))";
    let source = parse(text);
    let workspace = check(&source);
    let scope = p::of_graph(&catalog(), &workspace, "g").unwrap();
    let evaluated = recorded(&workspace);
    for (time, expected) in [(0.0, [0i64, 1, 1]), (2.0, [1, 2, 0])] {
        let mut probe = probe::make(&evaluated, Options { time: Some(time), ..Default::default() });
        for (name, expected) in ["x", "y", "z"].iter().zip(expected.iter()) {
            let node = p::find(&scope, &path(&["g", name])).unwrap();
            let actual = probe.taken_arm(&node, &[]);
            assert_eq!(actual, Some(*expected), "taken {} at {}", name, time);
        }
    }
    for (expression, result_text) in [
        ("{:weight 2 :name \"point\"}", "x.weight"),
        ("(array/float 6 3.0)", "(array/count x)"),
        ("[1 2 3]", "x.x"),
        ("(list 1 2)", "(count x)"),
    ] {
        let source = parse(&format!("(workspace wrap (graph g :context value (let* [x {}] {})))", expression, result_text));
        let before = run(&check_lenient(&source), 0.0);
        let (_, workspace) = apply_checked(&catalog(), &source, &Op::Wrap { nodes: vec![path(&["g", "x"])], loop_: crate::flow_edit::Loop::If }).unwrap();
        assert_eq!(run(&workspace, 0.0).results, before.results, "{}", expression);
    }
    let source = parse("(workspace bypass (graph g :context value (let* [x (if false 3 9)] x)))");
    let (_, workspace) = apply_checked(&catalog(), &source, &Op::ToggleBypass { node: path(&["g", "x"]) }).unwrap();
    assert_eq!(result(&workspace, 0.0), Value::Int(3));
}

#[test]
fn tuples_and_lazy_geometry_records() {
    let source = parse("(workspace tuples (graph g :context value (sum [i (range 2)] (if (< i 1) 1 2))))");
    let workspace = check(&source);
    let scope = p::of_graph(&catalog(), &workspace, "g").unwrap();
    let node = p::find(&scope, &path(&["g", "@result", "@result"])).unwrap();
    let mut probe = probe::make(&recorded(&workspace), Options::default());
    for (tuple, arm) in [(vec![0i64], 0i64), (vec![1], 1), (vec![0], 0), (vec![1], 1)] {
        assert_eq!(probe.taken_arm(&node, &tuple), Some(arm));
    }
    let mut empty = probe::make(&unrecorded(&workspace), Options::default());
    assert_eq!(empty.taken_arm(&node, &[0]), None);
    assert_eq!(empty.taken_arm(&node, &[0]), None);
    let pick = path(&["g", "pick"]);
    let mut evaluated = unrecorded(&workspace);
    evaluated.records = vec![(
        pick.clone(),
        vec![(vec![0], Value::Deferred(ty::geometry(), 1)), (vec![1], Value::Deferred(ty::geometry(), 2))],
    )];
    let forced: Rc<RefCell<Vec<i64>>> = Rc::new(RefCell::new(Vec::new()));
    let f = forced.clone();
    let mut probe = probe::make(
        &evaluated,
        Options {
            geometry: Some(Rc::new(move |id| {
                f.borrow_mut().insert(0, id);
                None
            })),
            ..Default::default()
        },
    );
    let at = |probe: &mut probe::Probe, tuple: &[i64], id: i64| {
        assert_eq!(probe.at(&pick, tuple), Some(Summary::Value(Value::Deferred(ty::geometry(), id))));
    };
    at(&mut probe, &[0], 1);
    at(&mut probe, &[0], 1);
    assert_eq!(*forced.borrow(), vec![1]);
    let mut geometry = node.clone();
    geometry.path = pick.clone();
    geometry.head = "geometry".into();
    geometry.ty = ty::geometry();
    geometry.invariant = false;
    let footer = probe.footer(&geometry, &[0]);
    assert!(footer.runs == Some(2) && footer.spark.is_none() && *forced.borrow() == vec![1]);
    assert!(probe.footer(&geometry, &[2]).value == "not run here" && *forced.borrow() == vec![1]);
    at(&mut probe, &[1], 2);
    assert_eq!(*forced.borrow(), vec![2, 1]);
    assert_eq!(probe.records(&pick).len(), 2);
}

#[test]
fn conditional_arms_are_inputs() {
    let source = parse("(workspace branches (graph g :context value (let* [x (if true (+ 1 2) (* 3 4)) y (cond false 1 true 2 :else 3) z (case 1 0 4 1 5 :else 6)] (+ x (+ y z)))))");
    let workspace = check(&source);
    let scope = p::of_graph_with(&|_| false, &catalog(), &workspace, "g").unwrap();
    for path in [path(&["g", "x#then"]), path(&["g", "x#else"])] {
        // an arm is an input of its conditional: a call written there is a card, not a zone
        assert!(p::find(&scope, &path).unwrap().zone.is_none());
    }
    let (source, workspace) = apply_checked(&catalog(), &source, &set_arg(&path(&["g", "x#then"]), ArgKey::Pos(0), num("8"))).unwrap();
    let mut probe = probe::make(&recorded(&workspace), Options::default());
    let scope = p::of_graph_with(&|_| false, &catalog(), &workspace, "g").unwrap();
    let node = p::find(&scope, &path(&["g", "x#then"])).unwrap();
    assert_eq!(probe.footer(&node, &[]).value, "10");
    let (source, _) = apply_checked(&catalog(), &source, &Op::AddArm { node: path(&["g", "y"]), after: 0 }).unwrap();
    assert_eq!(remap(&Op::AddArm { node: path(&["g", "y"]), after: 0 }, &path(&["g", "y#then~2", "@result"])), Some(path(&["g", "y#then~3", "@result"])));
    let _ = apply_checked(&catalog(), &source, &Op::DeleteArm { node: path(&["g", "y"]), index: 0 }).unwrap();
    assert_eq!(remap(&Op::DeleteArm { node: path(&["g", "y"]), index: 0 }, &path(&["g", "y#then", "@result"])), None);
}

#[test]
fn state_zone_and_forced_records() {
    let source = parse("(workspace toy (graph g :context draw (let* [elapsed (state [s 0.0] (+ s (frame/dt))) dot (draw/circle [12 20 0] (+ 1 elapsed) :fill \"#00ffff\")] (draw/merge dot))))");
    let (_, workspace) = apply_checked(&catalog(), &source, &set_arg(&path(&["g", "elapsed"]), ArgKey::Bv(1, 1), num("2.0"))).unwrap();
    let scope = p::of_graph(&catalog(), &workspace, "g").unwrap();
    let state = p::find(&scope, &path(&["g", "elapsed"])).unwrap();
    assert_eq!(state.zone.unwrap().kind, p::ZoneKind::State);
    let dot = p::find(&scope, &path(&["g", "dot"])).unwrap();
    assert!(dot.ty == ty::drawing() && dot.rows.len() == 4);
    let evaluated = recorded(&workspace);
    assert_eq!(evaluated.plan.nodes.len(), 2);
    let session = e::create_state();
    let mut live = input::at_time(0.0);
    live.dt = 0.25;
    let mut probes = probe::make(&evaluated, Options { state: Some(session.clone()), live: Some(live.clone()), ..Default::default() });
    let _ = probes.records(&path(&["g", "elapsed"]));
    let record = evaluated.records.iter().find(|(p, _)| *p == path(&["g", "elapsed"])).unwrap().1[0].1.clone();
    assert_eq!(e::force(&record, &live, Some(&session)).unwrap(), Value::Float(2.25));
}

#[test]
fn exact_reads_back() {
    let source = parse("(workspace readback (graph g :context value (let* [x (exact (sin t))] x)))");
    let workspace = check(&source);
    let card = card(&workspace, &["g", "x"]);
    assert!(card.ty == Ty::Float && card.rows.len() == 1);
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&path(&["g", "x"]), ArgKey::Pos(0), num("2.5"))).unwrap();
    assert_eq!(result(&edited, 7.0), Value::Float(2.5));
}

#[test]
fn nested_exact() {
    let source = parse("(workspace nested_exact (graph g :context draw\n    (let* [output (draw/circles (exact (map (fn [x] [(+ x t) 0 0]) (array/float 4))))] output)))");
    let workspace = check(&source);
    let scope = pane(&workspace);
    let exact = p::find(&scope, &path(&["g", "output#0"])).unwrap();
    assert_eq!(exact.head, "exact");
    assert!(p::find(&scope, &path(&["g", "output#0#0#0"])).is_some());
    let replacement = parse("(array/vec3 4)").remove(0);
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&exact.path, ArgKey::Pos(0), replacement)).unwrap();
    assert!(p::find(&pane(&edited), &path(&["g", "output#0#0#0"])).is_none());
}

#[test]
fn exact_freezes_an_image() {
    let source = parse("(workspace frozen (graph g :context image\n    (let* [img (image/noise :width 3 :height 2) snapshot (exact img)] snapshot)))");
    let workspace = check(&source);
    let card = p::find(&pane(&workspace), &path(&["g", "snapshot"])).unwrap();
    assert!(card.head == "exact" && card.ty == ty::image() && card.rows.len() == 1);
    let evaluation = unrecorded(&workspace);
    let node = evaluation.plan.nodes.iter().find(|n| n.kind == "exact").unwrap();
    assert!(node.ty == ty::image() && node.args.iter().any(|(n, _)| n == "value"));
    assert_eq!(evaluation.results.iter().find(|(n, _)| n == "g").unwrap().1, Value::Deferred(ty::image(), node.id));
    let replacement = parse("(image/noise :width 5 :height 4)").remove(0);
    let (source, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(0), replacement)).unwrap();
    let nested = p::find(&pane(&edited), &path(&["g", "snapshot#0"])).unwrap();
    assert_eq!(nested.head, "image/noise");
    let text = lisp::print(&source).unwrap().0;
    let roundtrip = check(&parse(&text));
    let result = unrecorded(&roundtrip);
    let node = result.plan.nodes.iter().find(|n| n.kind == "exact").unwrap();
    let child = match &node.args.iter().find(|(n, _)| n == "value").unwrap().1 {
        Value::Deferred(_, id) => &result.plan.nodes[*id as usize],
        _ => panic!("exact argument is not deferred"),
    };
    assert_eq!(child.args.iter().find(|(n, _)| n == "width").unwrap().1, Value::Int(5));
    assert_eq!(child.args.iter().find(|(n, _)| n == "height").unwrap().1, Value::Int(4));
}

#[test]
fn vector_norm_input_is_typed() {
    let source = parse("(workspace norm (graph g :context value\n    (let* [radius (length [3 4 0])] radius)))");
    let checked = check(&source);
    let card = p::find(&pane(&checked), &path(&["g", "radius"])).unwrap();
    assert!(card.ty == Ty::Float && card.rows[0].ty == Some(Ty::Vec3));
    let vector = parse("[0 0 12]").remove(0);
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(0), vector)).unwrap();
    assert_eq!(result(&edited, 0.0), Value::Float(12.0));
}

#[test]
fn maps_probe_one_call() {
    let source = parse("(workspace maps (graph g :context value (let* [amp 2.0 out (map (fn [(x : float)] (let* [shift (+ x amp)] (* shift 3.0))) (array/float 10003 0.5))] (array/count out))))");
    let workspace = check(&source);
    let scope = pane(&workspace);
    let fnc = p::find(&scope, &path(&["g", "out#0"])).unwrap();
    assert!(fnc.ty == Ty::Fn(Some(Box::new(FnSignature { params: vec![Ty::Float], result: Ty::Float }))) && fnc.zone.as_ref().unwrap().kind == p::ZoneKind::Fn);
    assert!(fnc.zone.as_ref().unwrap().rail.iter().any(|rail| rail.name == "x" && rail.ty == Some(Ty::Float)));
    let card = p::find(&scope, &path(&["g", "out"])).unwrap();
    assert_eq!(card.rows[0].chip, p::Chip::Name("out#0".into()));
    let chain = probe::chains(&scope);
    assert_eq!(chain[&path(&["g", "out#0", "shift"])], vec![path(&["g", "out#0"])]);
    let mut probe = probe::make(&recorded(&workspace), Options::default());
    let counts = probe.counts(&scope, &|_| 0);
    assert_eq!(counts.iter().find(|(p, _)| *p == path(&["g", "out#0"])).unwrap().1, 10003);
    let shift = p::find(&scope, &path(&["g", "out#0", "shift"])).unwrap();
    assert_eq!(probe.footer(&shift, &[9999]).value, "2.5");
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&shift.path, ArgKey::Pos(1), num("4.0"))).unwrap();
    let scope = pane(&edited);
    let shift = p::find(&scope, &shift.path).unwrap();
    let mut probe = probe::make(&recorded(&edited), Options::default());
    assert_eq!(probe.footer(&shift, &[9999]).value, "4.5");
    let source = parse("(workspace nested (graph g :context value (array/count (map (fn [(x : float)] (+ x 2.0)) (array/float 3 1.0)))))");
    let workspace = check(&source);
    let scope = pane(&workspace);
    let fnc = p::find(&scope, &path(&["g", "@result#0#0"])).unwrap();
    let mut body_path = fnc.path.clone();
    body_path.push("@result".into());
    let body = p::find(&scope, &body_path).unwrap();
    let (_, edited) = apply_checked(&catalog(), &source, &set_arg(&body.path, ArgKey::Pos(1), num("5.0"))).unwrap();
    let mut probe = probe::make(&recorded(&edited), Options::default());
    assert_eq!(probe.footer(&body, &[2]).value, "6");
}

fn is_error(source: &[s::Syntax], op: Op) -> bool {
    apply_checked(&catalog(), source, &op).is_err()
}

#[test]
fn scatter_add_is_typed() {
    let source = parse("(workspace scatter (graph g :context host (let* [added (array/scatter_add (array/int 3 0) (array/vec3 3 [1 2 3]) 2)] added)))");
    let workspace = check(&source);
    let card = card(&workspace, &["g", "added"]);
    assert!(card.head == "array/scatter_add" && card.ty == array_of(Ty::Vec3));
    let o = op::find(&[], &card.head, &context::value()).unwrap();
    assert_eq!(
        o.signature.pos,
        vec![("indices".to_owned(), array_of(Ty::Int)), ("values".to_owned(), array_of(Ty::Any)), ("size".to_owned(), Ty::Int)]
    );
    let (edited_source, edited) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(2), num("3"))).unwrap();
    assert_eq!(result(&edited, 0.0), Value::Vec3_array(Arc::new(vec![3., 6., 9., 0., 0., 0., 0., 0., 0.])));
    let saved = lisp::print(&edited_source).unwrap().0;
    let restored = parse(&saved);
    assert!(matches!(flow_check::workspace::check(&[], false, false, None, &catalog(), &restored), (Some(_), ds) if ds.is_empty()));
    for (index, replacement) in [(0, "(array/float 3)"), (1, "(array/text 3)"), (2, "\"bad\""), (2, "2.5")] {
        assert!(is_error(&source, set_arg(&card.path, ArgKey::Pos(index), parse(replacement).remove(0))), "{} {}", index, replacement);
    }
}

fn checks_clean(src: &[s::Syntax]) -> bool {
    matches!(flow_check::workspace::check(&[], false, false, None, &catalog(), src), (Some(_), ds) if ds.is_empty())
}

#[test]
fn integer_arrays() {
    let cases: [(&str, &str, Vec<i64>, &str, Vec<i64>); 2] = [
        ("array/compact", "(array/int 3 1)", vec![0, 1, 2], "(array/int 5 0)", vec![]),
        ("array/scan_sum", "(array/int 3 2)", vec![0, 2, 4, 6], "(array/int 5 -1)", vec![0, -1, -2, -3, -4, -5]),
    ];
    for (name, argument, expected, edited_argument, edited_expected) in cases {
        let source = parse(&format!("(workspace integer_arrays (graph g :context host (let* [tested ({} {})] tested)))", name, argument));
        let workspace = check(&source);
        let card = card(&workspace, &["g", "tested"]);
        assert!(card.head == name && card.ty == array_of(Ty::Int));
        let signature = op::find(&[], name, &context::value()).unwrap().signature;
        assert_eq!(signature.pos, vec![(if name == "array/compact" { "mask" } else { "array" }.to_owned(), array_of(Ty::Int))]);
        assert_eq!(result(&workspace, 0.0), int_array(&expected));
        let (changed, checked) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(0), parse(edited_argument).remove(0))).unwrap();
        assert_eq!(result(&checked, 0.0), int_array(&edited_expected));
        let saved = lisp::print(&changed).unwrap().0;
        assert!(checks_clean(&parse(&saved)));
        for value in ["(array/float 3)", "(array/text 3)", "3"] {
            assert!(is_error(&source, set_arg(&card.path, ArgKey::Pos(0), parse(value).remove(0))), "{} {}", name, value);
        }
    }
}

#[test]
fn ordering_arrays() {
    let cases = [("array/grade", "(array/int 3 2)", vec![0i64, 1, 2]), ("array/index_of", "(array/int 3 1) (array/int 4 1)", vec![0, 0, 0, 0])];
    for (name, args, expected) in cases {
        let source = parse(&format!("(workspace ordering (graph g :context host (let* [tested ({} {})] tested)))", name, args));
        let workspace = check(&source);
        let card = card(&workspace, &["g", "tested"]);
        assert!(card.head == name && card.ty == array_of(Ty::Int));
        assert!(card.rows[0].ty == Some(array_of(Ty::Any)) && card.rows[0].socket);
        if name == "array/grade" {
            assert!(card.rows.iter().any(|row| row.key == ArgKey::Kw("descending".into()) && row.ty == Some(Ty::Bool)));
        }
        let signature = op::find(&[], name, &context::value()).unwrap().signature;
        let want = if name == "array/grade" {
            vec![("keys".to_owned(), array_of(Ty::Any))]
        } else {
            vec![("keys".to_owned(), array_of(Ty::Any)), ("queries".to_owned(), array_of(Ty::Any))]
        };
        assert_eq!(signature.pos, want);
        assert_eq!(result(&workspace, 0.0), int_array(&expected));
        let (key, value, expected) =
            if name == "array/grade" { (ArgKey::Kw("descending".into()), "true", vec![0i64, 1, 2]) } else { (ArgKey::Pos(1), "(array/int 5 2)", vec![-1i64, -1, -1, -1, -1]) };
        let (changed, checked) = apply_checked(&catalog(), &source, &set_arg(&card.path, key, parse(value).remove(0))).unwrap();
        assert_eq!(result(&checked, 0.0), int_array(&expected));
        let saved = lisp::print(&changed).unwrap().0;
        assert!(checks_clean(&parse(&saved)));
        let (_, checked) = apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Pos(0), parse("(array/int 0)").remove(0))).unwrap();
        assert_eq!(result(&checked, 0.0), if name == "array/grade" { int_array(&[]) } else { int_array(&[-1, -1, -1, -1]) });
        let refused: Vec<(ArgKey, &str)> = if name == "array/grade" {
            vec![(ArgKey::Pos(0), "(array/vec3 3)"), (ArgKey::Pos(0), "3"), (ArgKey::Kw("descending".into()), "\"bad\"")]
        } else {
            vec![(ArgKey::Pos(0), "(array/text 3)"), (ArgKey::Pos(1), "(array/float 3)"), (ArgKey::Pos(1), "3")]
        };
        for (key, value) in refused {
            assert!(is_error(&source, set_arg(&card.path, key.clone(), parse(value).remove(0))), "{} {:?} {}", name, key, value);
        }
    }
}

#[test]
fn array_capacities() {
    let source = parse("(workspace capacities (graph g :context host (let* [limit (array/max_length)] limit)))");
    let checked = check(&source);
    let card = card(&checked, &["g", "limit"]);
    assert!(card.ty == Ty::Int && card.head == "array/max_length");
    let selector = card.rows.iter().find(|row| row.key == ArgKey::Kw("type".into())).unwrap();
    assert!(selector.ty == Some(Ty::Text) && selector.control == p::Control::Choice && !selector.socket);
    for (kind, expected) in [
        ("int", MAX_ARRAY_LENGTH),
        ("text", MAX_ARRAY_LENGTH),
        ("float", MAX_ARRAY_LENGTH),
        ("vec2", MAX_ARRAY_LENGTH / 2),
        ("vec3", MAX_ARRAY_LENGTH / 3),
        ("vec4", MAX_ARRAY_LENGTH / 4),
    ] {
        let (changed, checked) =
            apply_checked(&catalog(), &source, &set_arg(&card.path, ArgKey::Kw("type".into()), s::make(s::Draft::Sym(kind.into())))).unwrap();
        assert_eq!(result(&checked, 0.0), Value::Int(expected));
        let printed = lisp::print(&changed).unwrap().0;
        let saved = check(&parse(&printed));
        assert_eq!(result(&saved, 0.0), Value::Int(expected));
    }
    for text in ["\"unknown\"", "1"] {
        assert!(is_error(&source, set_arg(&card.path, ArgKey::Kw("type".into()), parse(text).remove(0))), "{}", text);
    }
    assert_eq!(result(&checked, 0.0), Value::Int(MAX_ARRAY_LENGTH));
}


#[test]

fn array_capacities_refuse_a_computed_type() {
    let source = parse("(workspace capacities (graph g :context host (let* [limit (array/max_length)] limit)))");
    let checked = check(&source);
    let card = card(&checked, &["g", "limit"]);
    assert!(is_error(&source, set_arg(&card.path, ArgKey::Kw("type".into()), parse("(if (> t 0.) \"int\" \"float\")").remove(0))));
}
