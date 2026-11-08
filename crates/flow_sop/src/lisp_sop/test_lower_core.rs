//! Lowering-level checks that need only sop/line, sop/merge and sop/curve (no grid, no static
//! maps): stable compiled ids across relowering, merge provenance, pending live parameters,
//! volatility, counts/status and the Edit_graph shape of a lowered graph.
use super::support::*;
use crate::lower;
use sop::edit_graph as edit;

fn merge_text() -> String {
    "(workspace w (graph g :context sop (sop/merge (sop/line :points 2) (sop/line :points 3))))".to_owned()
}

fn lowered(text: &str) -> lower::T {
    lower::workspace(None, Vec::new(), lower::Options::default(), &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

#[test]
fn compiled_ids_are_stable_when_passed_back() {
    let first = lowered(&merge_text());
    let again = lower::workspace(
        None,
        Vec::new(),
        lower::Options { compiled_ids: first.compiled_ids.clone(), sites: first.sites.clone(), ..lower::Options::default() },
        &parse(&merge_text()),
    )
    .unwrap();
    assert_eq!(first.graphs[0].root, again.graphs[0].root, "the displayed node keeps its compiled id");
    assert_eq!(first.compiled_ids, again.compiled_ids);
    assert_eq!(first.sites, again.sites);
    // without the previous ids the allocator hands out fresh ones
    let fresh = lowered(&merge_text());
    assert_ne!(first.graphs[0].root, fresh.graphs[0].root);
}

#[test]
fn merge_inputs_carry_provenance_tags() {
    let l = lowered(&merge_text());
    assert_eq!(l.provenance.len(), 2, "a collecting merge tags each of its inputs");
    let root = l.graphs[0].root.unwrap();
    for (tag, origin) in &l.provenance {
        assert_ne!(origin.merge, 0, "the tagged merge node has a compiled id (the inlined body's concat)");
        let _ = root;
        assert_eq!(origin.input, tag - l.provenance.keys().next().unwrap());
        assert_eq!(lower::origin(&l, *tag).as_ref(), Some(origin));
        assert!(lower::tags(&l, &origin.site, &origin.iter).contains(tag));
    }
    assert_eq!(lower::origin(&l, 1_000_000), None);
    assert_eq!(lower::zone_count(&l, &vec!["g".to_owned()]), None);
}

#[test]
fn a_live_curve_is_pending_and_volatile() {
    let l = lowered("(workspace w (graph g :context sop (sop/curve (list [0.0 0.0 (* t 1.0)] [1.0 0.0 0.0]))))");
    assert_eq!(l.pending.len(), 1, "the live points drive is a pending parameter");
    let root = l.graphs[0].root.unwrap();
    assert_eq!(l.pending[0].node, root);
    assert_eq!(l.pending[0].field, "points");
    assert!(lower::is_volatile(&l)(root), "a node with a live argument is volatile");
    let (live, cached) = lower::counts(&l);
    assert_eq!((live, cached), (1, 0));
    assert!(lower::status(&l, 0.0125).unwrap().starts_with("t 1 live · 0 cached · frame cook 12.5 ms"));
    assert!(l.graphs[0].network.drives.values().len() == 1);
    // a static graph has nothing pending and no status
    let s = lowered("(workspace w (graph g :context sop (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0]))))");
    assert!(s.pending.is_empty() && lower::status(&s, 0.0).is_none());
    assert!(!lower::is_volatile(&s)(s.graphs[0].root.unwrap()));
}

#[test]
fn a_lowered_graph_compiles_and_cooks() {
    let (l, node) = lower_text(lower::Options::default(), &merge_text());
    assert_eq!(l.graphs.len(), 1);
    assert!(l.graphs[0].default);
    assert_eq!(lower::objects(&l).len(), 1);
    assert!(edit::find(&l.graphs[0].network.geometry, l.graphs[0].root.unwrap()).is_some());
    let g = cook_once(1, &node);
    assert_eq!(rdk_core::geometry::point_count(&g), 5);
    assert!(rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Primitive, lower::source_attribute(), &g).is_some() || rdk_core::geometry::primitive_count(&g) == 0);
}

#[test]
fn a_graph_without_geometry_is_refused() {
    let r = lower::workspace(None, Vec::new(), lower::Options::default(), &parse("(workspace w (graph g :context sop 1))"));
    assert!(r.is_err());
    let r = lower::workspace(None, Vec::new(), lower::Options::default(), &parse("(workspace w (graph g :context sop (sop/not_a_node)))"));
    assert!(r.is_err());
}

#[test]
fn a_lowering_is_freed_with_its_last_handle() {
    let l = lowered(&merge_text());
    let weak = l.lowering.clone();
    let kept = l.clone();
    assert!(weak.upgrade().is_some(), "the lowering lives while its result does");
    drop(l);
    assert!(weak.upgrade().is_some(), "a clone keeps it");
    drop(kept);
    assert!(weak.upgrade().is_none(), "cook services do not own the lowering's factory cache");
}
