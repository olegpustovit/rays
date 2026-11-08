//! Port of lib/editor_core/test_panels.ml.
use crate::panels::{each_leaf, label, labels, leaves, set_size, Panels, to_string, valid, Axis, Panel, Size};

fn v() -> Panels {
    Panels::Leaf(Panel::View("main".into()))
}

fn g() -> Panels {
    Panels::Leaf(Panel::Graph)
}

fn h_ratio(ratio: f64, a: Panels, b: Panels) -> Panels {
    Panels::Split { axis: Axis::H, size: Size::Ratio(ratio), a: Box::new(a), b: Box::new(b) }
}

fn h(a: Panels, b: Panels) -> Panels {
    h_ratio(0.5, a, b)
}

fn s_ratio(ratio: f64, a: Panels, b: Panels) -> Panels {
    Panels::Split { axis: Axis::V, size: Size::Ratio(ratio), a: Box::new(a), b: Box::new(b) }
}

fn s(a: Panels, b: Panels) -> Panels {
    s_ratio(0.5, a, b)
}

fn fixed(size: Size) -> Panels {
    Panels::Split { axis: Axis::H, size, a: Box::new(v()), b: Box::new(g()) }
}

fn check(tree: &Panels, expected: &str) {
    let actual = label(tree);
    assert!(actual == expected, "{}: {:?}, expected {:?}", to_string(tree), actual, expected);
}

// a split is sized by a ratio or by one fixed side; both are clamped and validated
#[test]
fn split_size_clamp_and_validate() {
    assert!(set_size(&[], Size::Ratio(2.), h(v(), g())) == h_ratio(0.9, v(), g()));
    assert!(set_size(&[], Size::First(0), h(v(), g())) == fixed(Size::First(1)));
    assert!(
        set_size(&[1], Size::Second(320), h(v(), h(v(), g())))
            == h(v(), fixed(Size::Second(320)))
    );
    assert!(valid(&fixed(Size::First(216))) == Ok(()) && valid(&fixed(Size::Second(0))).is_err());
    assert!(label(&fixed(Size::First(216))) == "View | Graph");
}

#[test]
fn layout_labels() {
    check(&v(), "View");
    check(&s(h(v(), g()), Panels::Leaf(Panel::Timeline)), "(View | Graph) / Timeline");
    check(&h(v(), s(g(), Panels::Leaf(Panel::Lisp))), "View | Graph / Lisp");
    check(&h(h(v(), g()), Panels::Leaf(Panel::Inspector)), "View | Graph | Inspector");
    check(&Panels::Tile(vec![v(), v(), v(), v()]), "View \u{d7}4");
    check(&h(v(), h(v(), g())), "View \u{d7}2 | Graph");
    check(&h(g(), Panels::Float(Box::new(Panels::Leaf(Panel::Lisp)))), "Graph + Lisp");
    check(
        &h(Panels::Leaf(Panel::Outline), h(v(), h(g(), Panels::Leaf(Panel::Inspector)))),
        "Outline | View | Graph | Inspector",
    );
    check(&h(v(), Panels::Float(Box::new(Panels::Leaf(Panel::Inspector)))), "View + Inspector");
    assert!(
        labels(&[h_ratio(0.3, v(), g()), h_ratio(0.7, v(), g())])
            == vec![
                "View | Graph \u{b7} Graph 70%".to_string(),
                "View | Graph \u{b7} View 70%".to_string()
            ]
    );
    assert!(
        labels(&[v(), v(), g()])
            == vec![
                "View \u{b7} View 100%".to_string(),
                "View \u{b7} View 100% (2)".to_string(),
                "Graph".to_string()
            ]
    );
}

#[test]
fn leaves_put_floats_last_and_a_floats_own_before_its_floats() {
    let float = |x: Panels| Panels::Float(Box::new(x));
    let tree = h(float(h(Panels::Leaf(Panel::Lisp), float(Panels::Leaf(Panel::Inspector)))), h(g(), float(Panels::Leaf(Panel::Outline))));
    let all = leaves(&tree);
    let expected: Vec<(Vec<usize>, Panel)> =
        vec![(vec![1, 0], Panel::Graph), (vec![0, 0, 0], Panel::Lisp), (vec![0, 0, 1, 0], Panel::Inspector), (vec![1, 1, 0], Panel::Outline)];
    assert_eq!(all, expected);
    let mut seen = 0;
    assert!(each_leaf(&tree, &mut |path, p| { seen += 1; *p == Panel::Inspector && path == [0, 0, 1, 0] }));
    assert_eq!(seen, 3, "each_leaf stops at the first true");
}
