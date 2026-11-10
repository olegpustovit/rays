//! pxui::a11y builder, diff and the AT-SPI mapping tables (pxui's lib tests are off, so they live here). Headless,
//! every target.
use pxui::a11y::{A11yDiff, A11yTree, NO_PARENT, Role, Value, change, diff, node_id, state};
use pxui::geom::Rect;
use pxui_atspi::map;

fn sample(button: &str, checked: bool) -> A11yTree {
    let mut t = A11yTree::new();
    build(&mut t, button, checked, true);
    t
}

fn build(t: &mut A11yTree, button: &str, checked: bool, with_field: bool) {
    t.begin();
    let window = t.push(NO_PARENT, node_id(0, 0), Role::Window, Rect::new(0.0, 0.0, 400.0, 300.0));
    t.set_name(window, "Demo");
    let panel = t.push(window, node_id(1, 0), Role::Panel, Rect::new(0.0, 0.0, 400.0, 300.0));
    let b = t.push(panel, node_id(2, 0), Role::Button, Rect::new(10.0, 10.0, 80.0, 20.0));
    t.set_name(b, button);
    let toggle = t.push(panel, node_id(3, 0), Role::Toggle, Rect::new(10.0, 40.0, 80.0, 20.0));
    t.set_name(toggle, "Snap");
    t.set_states(toggle, state::ENABLED | state::VISIBLE | state::CHECKABLE | if checked { state::CHECKED } else { 0 });
    if with_field {
        let field = t.push(panel, node_id(4, 7), Role::TextField, Rect::new(10.0, 70.0, 200.0, 20.0));
        t.set_value(field, "héllo");
        t.set_selection(field, 1, 3);
    }
    t.end();
}

#[test]
fn builder_links_children_and_interns_strings() {
    let t = sample("Apply", false);
    assert_eq!(t.nodes.len(), 5);
    assert_eq!(t.revision, 1);
    assert_eq!(t.str(t.nodes[2].name), "Apply");
    assert_eq!(t.nodes[1].child_count, 3);
    assert_eq!(t.children(1).collect::<Vec<_>>(), vec![2, 3, 4]);
    assert_eq!(t.children(NO_PARENT).collect::<Vec<_>>(), vec![0]);
    assert_eq!(t.child(1, 2), Some(4));
    assert_eq!(t.child(1, 3), None);
    assert_eq!(t.index_in_parent(3), 1);
    assert_eq!(t.find(node_id(4, 7)), Some(4));
    assert_eq!(t.find(node_id(4, 0)), None, "a new generation is a different node");
    assert_eq!(t.value_text(&t.nodes[4]), "héllo");
    assert_eq!(t.nodes[2].states, state::ENABLED | state::VISIBLE, "default states");
}

#[test]
fn rebuild_is_allocation_stable() {
    let mut t = sample("Apply", false);
    let (nodes, strings) = (t.nodes.capacity(), t.strings.capacity());
    let (np, sp) = (t.nodes.as_ptr(), t.strings.as_ptr());
    build(&mut t, "Apply", true, true);
    assert_eq!((t.nodes.capacity(), t.strings.capacity()), (nodes, strings));
    assert_eq!((t.nodes.as_ptr(), t.strings.as_ptr()), (np, sp), "warm rebuild reuses the buffers");
    assert_eq!(t.revision, 2);
    let mut copy = A11yTree::new();
    copy.copy_from(&t);
    assert_eq!(copy.revision, t.revision);
    assert_eq!(copy.find(node_id(3, 0)), Some(3));
}

#[test]
fn diff_reports_added_removed_and_changed() {
    let old = sample("Apply", false);
    let mut d = A11yDiff::default();
    diff(&old, &old, &mut d);
    assert!(d.is_empty());

    let mut new = A11yTree::new();
    build(&mut new, "Apply all", true, false);
    diff(&old, &new, &mut d);
    assert!(d.added.is_empty());
    assert_eq!(d.removed, vec![node_id(4, 7)]);
    assert_eq!(d.changed, vec![(node_id(2, 0), change::NAME), (node_id(3, 0), change::STATES)]);

    diff(&new, &old, &mut d);
    assert_eq!(d.added, vec![node_id(4, 7)]);
    assert!(d.removed.is_empty());

    let mut edited = sample("Apply", false);
    build(&mut edited, "Apply", false, true);
    let field = edited.find(node_id(4, 7)).unwrap();
    edited.nodes[field as usize].value = Value::Number { now: 1.0, min: 0.0, max: 2.0, step: 0.5 };
    edited.nodes[field as usize].selection = (0, 0);
    diff(&old, &edited, &mut d);
    assert_eq!(d.changed, vec![(node_id(4, 7), change::VALUE | change::SELECTION)]);
}

#[test]
#[should_panic(expected = "pushed twice")]
fn duplicate_ids_are_refused() {
    let mut t = A11yTree::new();
    t.begin();
    let w = t.push(NO_PARENT, 9, Role::Window, Rect::default());
    t.push(w, 9, Role::Button, Rect::default());
    t.end();
}

#[test]
fn mapping_tables_cover_every_role_and_state() {
    // Every role appears once (Role is a dense repr(u8) enum ending at Separator).
    for k in 0..=Role::Separator as u8 {
        let hits = map::ROLES.iter().filter(|r| r.0 as u8 == k).count();
        assert_eq!(hits, 1, "role #{k} is mapped exactly once");
    }
    assert_eq!(map::role(Role::Button), (43, "push button"));
    assert_eq!(map::role(Role::TextField), (79, "entry"));
    assert_eq!(map::role(Role::Slider), (51, "slider"));
    assert_eq!(map::role(Role::Window), (23, "frame"));
    // Every PXUI state bit maps, once.
    let mut seen = 0u32;
    for (bit, atspi) in map::STATES {
        assert_eq!(bit.count_ones(), 1);
        assert_eq!(seen & bit, 0, "state bit {bit:#x} listed twice");
        assert!(!atspi.is_empty());
        seen |= bit;
    }
    assert_eq!(seen, state::DEFAULT * 2 - 1, "every state bit up to DEFAULT is mapped");

    let mut t = sample("Apply", true);
    let toggle = &t.nodes[3];
    let set = map::states(toggle);
    let has = |s: u32| set[(s / 32) as usize] & (1 << (s % 32)) != 0;
    use map::atspi_state as s;
    assert!(has(s::CHECKED) && has(s::CHECKABLE) && has(s::ENABLED) && has(s::SENSITIVE) && has(s::VISIBLE) && has(s::SHOWING));
    assert!(!has(s::FOCUSED) && !has(s::SINGLE_LINE));
    t.nodes[4].states |= state::EDITABLE | state::EXPANDABLE | state::READ_ONLY;
    let set = map::states(&t.nodes[4]);
    let has = |s: u32| set[(s / 32) as usize] & (1 << (s % 32)) != 0;
    assert!(has(s::EDITABLE) && has(s::SINGLE_LINE) && has(s::COLLAPSED) && has(s::READ_ONLY), "second word: read-only is 43");
    assert_eq!(set[1], 1 << (s::READ_ONLY - 32));
}
