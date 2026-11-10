//! `Bars.tree_text`: a panel tree as an editor graph.
use crate::bars;
use editor_core::panels::{self, Axis, Panel, Size};

#[test]
fn tree_text_binds_every_leaf_and_split() {
    let tree = panels::Panels::Split {
        axis: Axis::H,
        size: Size::Ratio(0.45),
        a: Box::new(panels::Panels::Leaf(panels::main())),
        b: Box::new(panels::Panels::Split {
            axis: Axis::V,
            size: Size::First(120),
            a: Box::new(panels::Panels::Leaf(Panel::Graph)),
            b: Box::new(panels::Panels::Leaf(Panel::Graph)),
        }),
    };
    let (text, names) = bars::tree_text("editor", "scene", &tree);
    assert_eq!(
        text,
        "(graph editor :context editor\n  (let* [preview (ui/viewport (ref scene))\n         network (ui/graph)\n         network_2 (ui/graph)\n         split (ui/split \"vertical\" network network_2 :first_size 120)\n         split_2 (ui/split_at \"horizontal\" 0.45 preview split)]\n    (ui/workspace split_2)))"
    );
    assert_eq!(names(&vec![0]), Some("preview".to_string()));
    assert_eq!(names(&vec![1, 1]), Some("network_2".to_string()));
    assert_eq!(names(&vec![]), Some("split_2".to_string()));
    assert_eq!(names(&vec![2]), None);
}
