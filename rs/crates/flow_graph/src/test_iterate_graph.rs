// Port of lib/flow_graph/test_iterate_graph.ml.
use crate::flow_edit::{apply_checked, ArgKey, Op};
use crate::projection as p;
use crate::test_support::*;
use flow_eval::Value;
use flow_syntax::lisp;
use flow_syntax::ty::Ty;

fn first(v: &Value) -> f64 {
    match v {
        Value::Float_array(a) => a[0],
        // flow_eval sometimes (run to run) hands back the same numbers as a List; the OCaml always packs them
        Value::List(l) => match l.first() {
            Some(Value::Float(f)) => *f,
            _ => panic!("not a float array: {:?}", v),
        },
        _ => panic!("not a float array: {:?}", v),
    }
}

#[test]
fn iterate_graph() {
    let source = parse(
        "(workspace passes (graph g :context host (let* [count 3 seed (array/float 4 1.0) tested (iterate count seed (fn [p] (map (fn [x] (* x 0.5)) p)))] tested)))",
    );
    let checked = check(&source);
    let pane = p::of_graph(&catalog(), &checked, "g").unwrap();
    let card = p::find(&pane, &path(&["g", "tested"])).unwrap();
    // a card like a fold's: typed rows for the count, the start, the function and the optional stop
    assert!(card.head == "iterate" && card.ty == Ty::Array(Box::new(Ty::Float)));
    assert!(row(&card, &ArgKey::Pos(0)).ty == Some(Ty::Int) && row(&card, &ArgKey::Pos(0)).chip == p::Chip::Name("count".into()));
    assert!(row(&card, &ArgKey::Pos(1)).ty == Some(Ty::Array(Box::new(Ty::Float))) && row(&card, &ArgKey::Pos(1)).chip == p::Chip::Name("seed".into()));
    assert!(row(&card, &ArgKey::Pos(2)).ty == Some(Ty::Fn(None)));
    let until = row(&card, &ArgKey::Kw("until".into()));
    assert!(until.expr.is_none() && until.socket);
    // the function is a nested fn zone whose parameter has the start's type
    let fnc = find_head("fn", &pane).unwrap();
    let zone = fnc.zone.unwrap();
    assert!(zone.kind == p::ZoneKind::Fn && zone.rail[0].ty == Some(Ty::Array(Box::new(Ty::Float))));
    assert_eq!(first(&result(&checked, 0.0)), 0.125);
    let edit = |op: Op| apply_checked(&catalog(), &source, &op).unwrap();
    let set = |key: ArgKey, v: &str| Op::SetArg { node: card.path.clone(), key, sub: vec![], value: form(v) };
    // the same gestures as any call: set a literal, wire a name, add and remove :until
    let (_, literal) = edit(set(ArgKey::Pos(0), "5"));
    assert_eq!(first(&result(&literal, 0.0)), 1.0 / 32.0);
    let (_, zero) = edit(set(ArgKey::Pos(0), "0"));
    assert_eq!(first(&result(&zero, 0.0)), 1.0);
    let (_, wired) = edit(Op::Connect { node: card.path.clone(), key: ArgKey::Pos(0), src: "count".into(), iter: false });
    assert_eq!(first(&result(&wired, 0.0)), 0.125);
    let (_, reseeded) = edit(set(ArgKey::Pos(1), "(array/float 4 8.0)"));
    assert_eq!(first(&result(&reseeded, 0.0)), 1.0);
    let (stopped, until_ws) = edit(set(ArgKey::Kw("until".into()), "(fn [a b] true)"));
    assert_eq!(first(&result(&until_ws, 0.0)), 0.5);
    let saved = lisp::print(&stopped).0;
    let loaded = check(&parse(&saved));
    assert_eq!(first(&result(&loaded, 0.0)), 0.5);
    let with_until = p::of_graph(&catalog(), &until_ws, "g").unwrap();
    let card_until = p::find(&with_until, &path(&["g", "tested"])).unwrap();
    assert!(row(&card_until, &ArgKey::Kw("until".into())).expr.is_some());
    let (_, removed) =
        apply_checked(&catalog(), &stopped, &Op::Disconnect { node: card.path.clone(), key: ArgKey::Kw("until".into()), fallback: None }).unwrap();
    assert_eq!(first(&result(&removed, 0.0)), 0.125);
    assert!(apply_checked(&catalog(), &source, &set(ArgKey::Pos(0), "\"bad\"")).is_err());
    assert!(apply_checked(&catalog(), &source, &set(ArgKey::Pos(2), "3")).is_err());
    // a new iterate node from the menu
    let (_, added) = apply_checked(
        &catalog(),
        &source,
        &Op::AddNode { scope: path(&["g"]), name: "again".into(), expr: form("(iterate 2 tested (fn [q] (map (fn [y] (+ y 1.0)) q)))") },
    )
    .unwrap();
    assert_eq!(p::find(&p::of_graph(&catalog(), &added, "g").unwrap(), &path(&["g", "again"])).unwrap().ty, Ty::Array(Box::new(Ty::Float)));
}

#[test]
fn debug_iterate() {
    let source = parse("(workspace passes (graph g :context host (let* [count 3 seed (array/float 4 1.0) tested (iterate count seed (fn [p] (map (fn [x] (* x 0.5)) p)))] tested)))");
    let checked = check(&source);
    let pane = p::of_graph(&catalog(), &checked, "g").unwrap();
    let fnc = find_head("fn", &pane).unwrap();
    eprintln!("PATH {:?} TY {:?} RAIL {:?}", fnc.path, fnc.ty, fnc.zone.as_ref().unwrap().rail);
}
