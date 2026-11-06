//! Port of lib/editor_core/test_panels.ml.
use crate::panels::{label, labels, set_size, t, to_string, valid, Axis, Panel, Size};

fn v() -> t {
    t::Leaf(Panel::View("main".into()))
}

fn g() -> t {
    t::Leaf(Panel::Graph)
}

fn h_ratio(ratio: f64, a: t, b: t) -> t {
    t::Split { axis: Axis::H, size: Size::Ratio(ratio), a: Box::new(a), b: Box::new(b) }
}

fn h(a: t, b: t) -> t {
    h_ratio(0.5, a, b)
}

fn s_ratio(ratio: f64, a: t, b: t) -> t {
    t::Split { axis: Axis::V, size: Size::Ratio(ratio), a: Box::new(a), b: Box::new(b) }
}

fn s(a: t, b: t) -> t {
    s_ratio(0.5, a, b)
}

fn fixed(size: Size) -> t {
    t::Split { axis: Axis::H, size, a: Box::new(v()), b: Box::new(g()) }
}

fn check(tree: &t, expected: &str) {
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
    check(&s(h(v(), g()), t::Leaf(Panel::Timeline)), "(View | Graph) / Timeline");
    check(&h(v(), s(g(), t::Leaf(Panel::Lisp))), "View | Graph / Lisp");
    check(&h(h(v(), g()), t::Leaf(Panel::Inspector)), "View | Graph | Inspector");
    check(&t::Tile(vec![v(), v(), v(), v()]), "View \u{d7}4");
    check(&h(v(), h(v(), g())), "View \u{d7}2 | Graph");
    check(&h(g(), t::Float(Box::new(t::Leaf(Panel::Lisp)))), "Graph + Lisp");
    check(
        &h(t::Leaf(Panel::Outline), h(v(), h(g(), t::Leaf(Panel::Inspector)))),
        "Outline | View | Graph | Inspector",
    );
    check(&h(v(), t::Float(Box::new(t::Leaf(Panel::Inspector)))), "View + Inspector");
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
