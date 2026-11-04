// Port of lib/flow_graph/test_array_composition_graph.ml.
use crate::flow_edit::{apply_checked, default_for, ArgKey, Op};
use crate::projection as p;
use crate::test_support::*;
use flow_syntax::syntax::{self as s, Node};
use flow_syntax::ty::Ty;
use flow_syntax::{lisp, value};

fn array(ty: &Ty, count: &str) -> String {
    match ty {
        Ty::Int => format!("(array/int {} 9007199254740993)", count),
        Ty::Float => format!("(array/float {} -0.)", count),
        Ty::Vec2 => format!("(map (fn [x] [x -0.]) (array/float {}))", count),
        Ty::Vec3 => format!("(array/vec3 {} [1 -0. 2])", count),
        Ty::Vec4 => format!("(array/vec4 {} [1 -0. 2 3])", count),
        Ty::Text => format!("(array/text {} \"é日本😀\")", count),
        _ => panic!("no array of this type"),
    }
}

#[test]
fn typed_composition() {
    let types = [Ty::Int, Ty::Float, Ty::Vec2, Ty::Vec3, Ty::Vec4, Ty::Text];
    for ty in &types {
        let cases = [
            ("array/concat", format!("{} {}", array(ty, "3"), array(ty, "2")), ArgKey::Pos(1), array(ty, "4"), 7usize),
            ("array/slice", format!("{} 0 3", array(ty, "3")), ArgKey::Pos(2), "1".to_owned(), 1usize),
        ];
        for (name, args, key, replacement, expected_count) in cases {
            let source = parse(&format!("(workspace composition (graph g :context host (let* [tested ({} {})] tested)))", name, args));
            let checked = check(&source);
            let card = card(&checked, &["g", "tested"]);
            assert!(card.head == name && card.ty == Ty::Array(Box::new(ty.clone())));
            assert!(card.rows[0].ty == Some(Ty::Array(Box::new(Ty::Any))) && card.rows[0].socket);
            if name == "array/slice" {
                for key in [ArgKey::Pos(1), ArgKey::Pos(2)] {
                    assert!(card.rows.iter().any(|r| r.key == key && r.ty == Some(Ty::Int)));
                }
            }
            let (changed, checked) =
                apply_checked(&catalog(), &source, &Op::SetArg { node: card.path.clone(), key, sub: vec![], value: form(&replacement) }).unwrap();
            let r = result(&checked, 0.0);
            assert!(value::ty_of(&r) == Ty::Array(Box::new(ty.clone())) && value::array_length(&r).unwrap() == expected_count);
            let saved = lisp::print(&changed).0;
            let loaded = check(&parse(&saved));
            assert_eq!(format!("{:?}", result(&loaded, 0.0)), format!("{:?}", r));
            assert!(apply_checked(&catalog(), &source, &Op::SetArg { node: card.path.clone(), key: ArgKey::Pos(0), sub: vec![], value: form("3") }).is_err());
            if name == "array/slice" {
                assert!(apply_checked(&catalog(), &source, &Op::SetArg { node: card.path.clone(), key: ArgKey::Pos(1), sub: vec![], value: form("\"bad\"") }).is_err());
            }
        }
        let default = default_for(&Ty::Array(Box::new(ty.clone())), "source", None).unwrap();
        let expression = s::make(Node::List(vec![s::make(Node::Sym("array/concat".into())), default.clone(), default]));
        let source = parse("(workspace menu (graph g :context host (array/int 0)))");
        let (_, added) = apply_checked(&catalog(), &source, &Op::AddNode { scope: path(&["g"]), name: "joined".into(), expr: expression }).unwrap();
        assert_eq!(card(&added, &["g", "joined"]).ty, Ty::Array(Box::new(ty.clone())));
    }
    // Inline composition is an ordinary nested card in the map's Fn zone and its
    // bounds can be edited directly; it is not hidden in a host callback.
    let source = parse(
        "(workspace inline (graph g :context host (let* [table (array/int 13) tested (map (fn [(i : int)] (array/nth (array/slice (array/concat table table) 0 13) i)) (array/range 13 :type int))] tested)))",
    );
    let checked = check(&source);
    let pane = p::of_graph(&catalog(), &checked, "g").unwrap();
    let slice = find_head("array/slice", &pane).unwrap();
    let concat = find_head("array/concat", &pane).unwrap();
    assert!(slice.ty == Ty::Array(Box::new(Ty::Int)) && concat.ty == Ty::Array(Box::new(Ty::Int)));
    let (_, edited) = apply_checked(&catalog(), &source, &Op::SetArg { node: slice.path.clone(), key: ArgKey::Pos(1), sub: vec![], value: form("13") }).unwrap();
    assert_eq!(value::array_length(&result(&edited, 0.0)).unwrap(), 13);
}
