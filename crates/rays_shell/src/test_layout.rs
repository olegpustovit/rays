use crate::chrome;
use crate::layout::{self as L, Axis, Panel, Size, Panels};
use editor_core::panels;
use rays::frame;

pub fn frame_of(size: (i64, i64)) -> frame::Frame {
    let mut input = input::at_time(0.0);
    input.size = size;
    frame::Frame { input, pixel_scale: (1.0, 1.0), fps: 0.0, mouse_delta: (0.0, 0.0) }
}

fn leaf(p: Panel) -> Panels {
    Panels::Leaf(p)
}

fn split(axis: Axis, size: Size, a: Panels, b: Panels) -> Panels {
    Panels::Split { axis, size, a: Box::new(a), b: Box::new(b) }
}

fn views(n: usize) -> Panels {
    Panels::Tile((0..n).map(|i| leaf(Panel::View(i.to_string()))).collect())
}

#[test]
fn default_columns_are_45_35_20() {
    let panes = L::panes(&L::geometry(None, None, &L::default(), &frame_of((1000, 300))));
    assert_eq!((panes.view.2, panes.graph.2, panes.inspector.2), (449, 349, 200));
    let hidden = [Panel::Timeline, Panel::Inspector];
    let p = L::panes(&L::geometry(None, Some(&hidden), &L::default(), &frame_of((1000, 300))));
    let hidden_both = [Panel::Timeline, Panel::Inspector, Panel::Graph];
    let v = L::panes(&L::geometry(None, Some(&hidden_both), &L::default(), &frame_of((1000, 300)))).view;
    assert!(p.inspector.2 == 0 && v.2 == 1000, "collapsed graph and inspector did not vanish");
}

#[test]
fn default_layout_goldens() {
    // the default three columns match the retired fixed layout at these sizes and states
    let golden = |size: (i64, i64), hidden: &[Panel], want: (L::Bounds, i64, i64, L::Bounds, L::Bounds)| {
        let p = L::panes(&L::geometry(None, Some(hidden), &L::default(), &frame_of(size)));
        assert!(
            p.view == want.0 && p.graph.2 == want.1 && p.inspector.2 == want.2 && p.status == want.3 && p.timeline == want.4,
            "default layout changed at {size:?}: {p:?}"
        );
    };
    // a header is a 4-point margin and a 24-point row: the bodies start at 28; the status strip is
    // a hairline and a 24-point bar, the strip under a tree without a timeline the same
    golden((800, 600), &[Panel::Timeline], ((0, 28, 359, 547), 279, 160, (0, 575, 800, 25), (0, 575, 800, 0)));
    golden((1280, 800), &[Panel::Timeline, Panel::Graph], ((0, 28, 885, 747), 0, 394, (0, 775, 1280, 25), (0, 775, 1280, 0)));
    golden((1920, 1080), &[Panel::View("main".into())], ((0, 28, 24, 1002), 1205, 689, (0, 1055, 1920, 25), (0, 1030, 1920, 25)));
    golden((1000, 300), &[Panel::Inspector], ((0, 28, 561, 222), 438, 0, (0, 275, 1000, 25), (0, 250, 1000, 25)));
    golden((500, 400), &[Panel::Timeline], ((0, 28, 224, 347), 174, 100, (0, 375, 500, 25), (0, 375, 500, 0)));
}

#[test]
fn every_tree_covers_the_area_once() {
    let trees: Vec<(&str, Panels)> = vec![
        ("default", L::default()),
        (
            "sheet",
            split(
                Axis::V,
                Size::Ratio(0.13),
                leaf(Panel::Outline),
                split(
                    Axis::H,
                    Size::Ratio(0.5),
                    split(Axis::V, Size::Ratio(0.58), leaf(Panel::Graph), leaf(Panel::Lisp)),
                    views(4),
                ),
            ),
        ),
        ("tile of five", views(5)),
        (
            "vertical stack",
            split(
                Axis::V,
                Size::Ratio(0.3),
                leaf(Panel::Graph),
                split(Axis::V, Size::Ratio(0.5), leaf(Panel::Inspector), leaf(Panel::List)),
            ),
        ),
        (
            "nested",
            split(
                Axis::H,
                Size::Ratio(0.3),
                split(Axis::V, Size::Ratio(0.4), leaf(Panel::Graph), leaf(Panel::List)),
                split(Axis::H, Size::Ratio(0.5), leaf(Panel::Inspector), leaf(Panel::View("z".into()))),
            ),
        ),
    ];
    for (name, tree) in &trees {
        for (w, h) in [(1000i64, 600i64), (640, 480), (333, 217)] {
            let g = L::geometry(None, None, tree, &frame_of((w, h)));
            let mut seen = vec![0i64; (w * h) as usize];
            let mut cover = |(x, y, cw, ch): L::Bounds| {
                for i in x..x + cw {
                    for j in y..y + ch {
                        if i >= 0 && j >= 0 && i < w && j < h {
                            seen[(i * h + j) as usize] += 1;
                        } else {
                            panic!("{name}: a rectangle leaves the frame");
                        }
                    }
                }
            };
            for l in &g.leaves {
                // the frame is the margin, the header row and the body, once
                let (fx, fy, fw, fh) = l.frame;
                let (hx, hy, hw, hh) = l.header;
                let (bx, by, bw, bh) = l.body;
                assert!(
                    !((hx, hw) != (fx, fw)
                        || (bx, bw) != (fx, fw)
                        || hy < fy
                        || by != hy + hh
                        || by + bh > fy + fh
                        || (!l.floating && hy - fy + hh + bh != fh && bh > 0)),
                    "{name}: a leaf's header and body do not tile its frame"
                );
                cover(l.frame);
            }
            cover(g.status_at);
            for s in &g.splitters {
                cover(s.bounds);
            }
            for n in &seen {
                assert!(*n == 1, "{name} at {w}x{h}: a point is covered {n} times");
            }
        }
    }
}

#[test]
fn floats_overlay_their_parent_and_come_last() {
    let wide = frame_of((1000, 300));
    let floated = split(Axis::H, Size::Ratio(0.5), leaf(Panel::Graph), Panels::Float(Box::new(leaf(Panel::Inspector))));
    let g = L::geometry(None, None, &floated, &wide);
    assert_eq!(g.leaves.iter().map(|l| l.floating).collect::<Vec<_>>(), vec![false, true], "authored floats lost their painting precedence");
    assert_eq!(g.leaves.iter().map(|l| l.panel.clone()).collect::<Vec<_>>(), vec![Panel::Graph, Panel::Inspector]);
    let (bx, by, bw, bh) = g.leaves.last().unwrap().body;
    let (gx, _, gw, _) = L::find(&g, &Panel::Graph).unwrap().body;
    assert!(!(gx != 0 || gw != 1000 || bx <= 0 || bx + bw >= 1000 || by <= 28 || bh <= 0), "a float is not an inset overlay over a full-size sibling");
    // collapsed, an authored float that was never moved is a short tab, not its whole window
    let collapsed = |path: &[usize]| panels::State { collapsed: path == [1usize, 0], window: None };
    let folded = L::geometry(Some(&collapsed), None, &floated, &wide);
    let last = folded.leaves.last().unwrap();
    assert!(last.frame.2 <= 150 && last.frame.3 <= 40 && last.body.3 == 0, "a collapsed float kept its window size");
}

#[test]
fn fixed_sides_keep_their_points() {
    // fixed sides keep their points at every window size: the outline column, the right column,
    // the graph and the lisp panel; the timeline leaf is its 24-point strip with no header; the
    // status strip spans the window
    let target = split(
        Axis::H,
        Size::First(216),
        leaf(Panel::Outline),
        split(
            Axis::H,
            Size::Second(320),
            split(
                Axis::V,
                Size::Second(336),
                split(Axis::V, Size::Ratio(0.5), leaf(Panel::View("main".into())), leaf(Panel::Timeline)),
                leaf(Panel::Graph),
            ),
            split(Axis::V, Size::Second(300), leaf(Panel::Inspector), leaf(Panel::Lisp)),
        ),
    );
    for (w, h) in [(1440i64, 900i64), (1920, 1080), (1100, 700)] {
        let g = L::geometry(None, Some(&[]), &target, &frame_of((w, h)));
        let whole = |panel: Panel| g.leaves.iter().find(|l| l.panel == panel).expect("the target lost a panel").frame;
        let tree = h - 25;
        let expect = |what: &str, got: L::Bounds, want: L::Bounds| assert_eq!(got, want, "target at {w}x{h}: {what}");
        expect("outline", whole(Panel::Outline), (0, 0, 216, tree));
        expect("viewport", whole(Panel::View("main".into())), (217, 0, w - 216 - 320 - 2, tree - 336 - 24 - 2));
        expect("timeline", whole(Panel::Timeline), (217, tree - 336 - 24 - 1, w - 216 - 320 - 2, 24));
        expect("graph", whole(Panel::Graph), (217, tree - 336, w - 216 - 320 - 2, 336));
        expect("inspector", whole(Panel::Inspector), (w - 320, 0, 320, tree - 300 - 1));
        expect("lisp", whole(Panel::Lisp), (w - 320, tree - 300, 320, 300));
        expect("status", g.status_at, (0, tree, w, 25));
        expect("no second timeline strip", g.timeline_at, (0, tree, w, 0));
        let timeline = g.leaves.iter().find(|l| l.panel == Panel::Timeline).unwrap();
        assert_eq!(timeline.header.3, 0, "a docked timeline has a header");
    }
}

#[test]
fn a_fixed_side_yields_when_both_do_not_fit() {
    // too small for both: the other side keeps its minimum, then the fixed side shrinks to a point
    let narrow = split(Axis::H, Size::First(216), leaf(Panel::Outline), leaf(Panel::Graph));
    let widths = |w: i64| -> Vec<i64> { L::geometry(None, None, &narrow, &frame_of((w, 300))).leaves.iter().map(|l| l.header.2).collect() };
    assert!(widths(300) == vec![119, 180] && widths(100) == vec![1, 98], "a fixed side did not yield");
}

#[test]
fn collapsed_stack_and_timeline_strips() {
    let wide = frame_of((1000, 300));
    // a collapsed panel in a stack is its 24-point header, with no margin over it; the next
    // panel's header sits under its own 4-point margin
    let stack = split(Axis::V, Size::Ratio(0.5), leaf(Panel::Inspector), leaf(Panel::Lisp));
    let first = |path: &[usize]| panels::State { collapsed: path == [0usize], window: None };
    let g = L::geometry(Some(&first), None, &stack, &wide);
    assert!(
        g.leaves.len() == 2
            && g.leaves[0].header.1 == 0
            && g.leaves[0].header.3 == 24
            && g.leaves[0].body.3 == 0
            && g.leaves[1].header.1 == 29,
        "a collapsed strip in a stack is not its header"
    );
    // a header is a 24-point row under a 4-point margin; a docked timeline under 90 points is its
    // strip alone, a taller one has a header like any panel
    let tall = split(Axis::V, Size::Second(97), leaf(Panel::View("main".into())), leaf(Panel::Timeline));
    let g = L::geometry(None, Some(&[]), &tall, &wide);
    assert_eq!(g.leaves.len(), 2);
    let l = &g.leaves[1];
    assert!(l.panel == Panel::Timeline && l.frame.3 == 97 && l.header.3 == 24, "a tall timeline has no header row");
    let (fx, fy, fw, _) = l.frame;
    let (hx, hy, hw, _) = l.header;
    let (bx, by, bw, bh) = l.body;
    assert!((hx, hy, hw) == (fx, fy + 4, fw) && (bx, by, bw) == (fx, fy + 28, fw) && bh == 97 - 28, "a tall timeline's header and body are not under the 4-point margin");
    let strip = split(Axis::V, Size::Second(24), leaf(Panel::View("main".into())), leaf(Panel::Timeline));
    let g = L::geometry(None, Some(&[]), &strip, &wide);
    assert!(g.leaves.len() == 2 && g.leaves[1].panel == Panel::Timeline && g.leaves[1].header.3 == 0 && g.leaves[1].body.3 == 24, "a 24-point timeline is not its strip");
}

#[test]
fn nested_splits_take_their_share() {
    let wide = frame_of((1000, 300));
    let g = L::geometry(None, None, &split(Axis::V, Size::Ratio(0.25), leaf(Panel::Graph), leaf(Panel::Lisp)), &wide);
    assert_eq!(g.leaves.len(), 2);
    assert!(g.leaves[0].body.3 + 22 < g.leaves[1].body.3 + 22, "a vertical split ignored its ratio");
}

#[test]
fn gutter_sides_and_resizes() {
    let sized = split(Axis::H, Size::Second(320), leaf(Panel::Graph), leaf(Panel::Inspector));
    let gutter = L::geometry(None, None, &sized, &frame_of((1000, 600))).splitters[0].clone();
    assert!(L::sides(&gutter) == (679, 320) && L::resized(&gutter, L::Way::Second) == Size::Second(320), "a gutter does not report the sides of its split");
}

#[test]
fn header_groups_stand_inside_the_header() {
    // A header's right end: the groups stand outermost first inside the header and never over one
    // another; the title's room shrinks first, then the last group goes.
    let leaf_of = |width: i64| L::Leaf {
        path: vec![0],
        panel: Panel::Graph,
        frame: (100, 0, width, 200),
        header: (100, 4, width, 24),
        body: (100, 28, width, 172),
        floating: false,
    };
    let slots = |width: i64| chrome::header_slots(&leaf_of(width), &[100.0, 150.0]);
    let (placed, limit) = slots(600);
    let (views, tabs) = (placed[0].expect("a wide header dropped a group"), placed[1].expect("a wide header dropped a group"));
    assert_eq!(views, 664.0, "the first group does not end before the collapse button");
    assert!(tabs == views - 108.0 && limit == tabs - 158.0, "header groups are not 8 apart");
    let (placed, limit) = slots(290);
    assert!(placed[0].is_some() && placed[1].is_none(), "a narrow header did not drop its last group alone");
    assert_eq!(limit, placed[0].unwrap() - 108.0, "a dropped group kept its room");
    let (placed, limit) = slots(130);
    assert!(placed == vec![None, None], "a group stands in a header too narrow for it");
    assert_eq!(limit, 194.0, "a header with no groups ends its title at the button");
    // whatever the width, no group starts left of the margin or ends over the button, and the
    // title ends before the groups
    for width in 0..=700 {
        let (placed, limit) = slots(width);
        for (slot, w) in placed.iter().zip([100.0, 150.0]) {
            if let Some(right) = slot {
                assert!(!(right - w < 104.0 || *right > (100 + width - 36) as f32 || limit > right - w), "a header group leaves its header");
            }
        }
    }
}

#[test]
fn layout_geometry_is_cheap() {
    // Smoke-test repeated default-tree layouts against one expected geometry.
    let f = frame_of((1000, 300));
    let tree = L::default();
    let mut sum = 0;
    for _ in 0..2000 {
        sum += L::panes(&L::geometry(None, None, &tree, &f)).view.2;
    }
    assert_eq!(sum, 2000 * 449);
}
