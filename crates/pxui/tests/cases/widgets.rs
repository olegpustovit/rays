//! The desktop widget set (`ui_widgets.rs`): one interaction case per widget through synthetic
//! input, their accessibility roles and states, zero allocations warm, busy animations through
//! patches with no rebuild, and virtual list, tree and table building only visible rows of 100,000.
use crate::support::*;
use input::{Event, Key};
use pxui::a11y::{state, A11yAction, A11yNode, A11yTree, Action, Role};
use pxui::ui::*;
use std::rc::Rc;

pub const W: i64 = 480;
pub const H: i64 = 760;
pub const NAMES: [&str; 8] = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta"];
pub const MENUS: [(&str, &[&str]); 2] = [("File", &["New", "Open", "Quit"]), ("Edit", &["Undo", "Redo", "Cut", "Copy"])];
pub const BIG: usize = 100_000;
pub const TOAST_SECONDS: f64 = 5.0;

fn at_keys(keys: Vec<Key>, t: f64, events: Vec<Event>) -> input::frame::Frame {
    let mut f = frame_at(keys, 1.0, t, events);
    f.input.size = (W, H);
    f
}

fn at(t: f64, events: Vec<Event>) -> input::frame::Frame {
    at_keys(vec![], t, events)
}

fn find<'a>(t: &'a A11yTree, role: Role, name: &str) -> Option<&'a A11yNode> {
    t.nodes.iter().find(|n| n.role == role && t.str(n.name) == name)
}

fn node(ui: &Ui, role: Role, name: &str) -> A11yNode {
    let t = ui.output().a11y;
    *find(t, role, name).unwrap_or_else(|| panic!("no {role:?} {name:?} in {:?}", t.nodes.iter().map(|n| (n.role, t.str(n.name))).collect::<Vec<_>>()))
}

fn centre(n: &A11yNode) -> (i64, i64) {
    ((n.rect.x + n.rect.w / 2.0) as i64, (n.rect.y + n.rect.h / 2.0) as i64)
}

/// Everything the widgets hold between frames, as an app keeps it.
pub struct S {
    pub check: bool,
    pub radio: usize,
    pub slider: f32,
    pub tab: usize,
    pub seg: usize,
    pub drop: usize,
    pub picked: Option<(usize, usize)>,
    pub list: Vec<bool>,
    pub multi: bool,
    pub activated: Option<usize>,
    pub nodes: Vec<TreeNode>,
    pub open: Vec<bool>,
    pub tree_sel: Option<usize>,
    pub visible: Vec<u32>,
    pub widths: [f32; 3],
    pub table_sel: Option<usize>,
    pub sorted: Option<usize>,
    pub search: String,
    pub icon: u32,
    pub toasts: usize,
    pub gone: Option<usize>,
    pub busy: Vec<Handle>,
    pub image: Option<Image>,
    pub add: Option<Handle>,
    pub scroll: Option<Handle>,
    pub list_frame: Option<Handle>,
}

impl S {
    pub fn new() -> S {
        let nodes = (0..BIG).map(|i| TreeNode { depth: if i % 10 == 0 { 0 } else { 1 }, children: i % 10 == 0 }).collect();
        S {
            check: false, radio: 0, slider: 0.25, tab: 0, seg: 0, drop: 0, picked: None, list: vec![false; BIG], multi: false, activated: None,
            nodes, open: vec![false; BIG], tree_sel: None, visible: Vec::with_capacity(BIG), widths: [80.0, 80.0, 80.0], table_sel: None, sorted: None,
            search: String::new(), icon: 0, toasts: 0, gone: None, busy: Vec::with_capacity(4), image: None, add: None, scroll: None, list_frame: None,
        }
    }
}

fn names(i: usize, put: &mut dyn FnMut(&str)) {
    put(NAMES[i % NAMES.len()]);
}

fn cells(i: usize, c: usize, put: &mut dyn FnMut(&str)) {
    put(NAMES[(i + c) % NAMES.len()]);
}

/// A fixed-height box for a widget that fills its parent.
fn sized(ui: &mut Ui, label: &str, h: f32, f: &mut dyn FnMut(&mut Ui)) {
    let b = ui.node(label, Node { h: Size::Px(h), ..Node::DEFAULT });
    ui.within(&b, f);
}

/// The menu bar at the root, then one panel of every new widget (two columns), then the toasts.
pub fn all(ui: &mut Ui, s: &mut S) {
    body(ui, s);
    toasts(ui, s);
}

/// The toasts, at the root (outside any cached view).
pub fn toasts(ui: &mut Ui, s: &mut S) {
    let toasts = ["Saved", "Exported"];
    if let Some(i) = ui.toasts("toasts", &toasts[..s.toasts], TOAST_SECONDS) {
        s.gone = Some(i);
    }
}

/// The menu bar and both columns of widgets.
pub fn body(ui: &mut Ui, s: &mut S) {
    if let Some(p) = ui.menu_bar("menus", &MENUS) {
        s.picked = Some(p);
    }
    s.busy.clear();
    let row = ui.node("columns", Node { w: Size::Px(W as f32), h: Size::Fit, axis: Axis::Row, at: Some((0.0, 24.0)), ..Node::DEFAULT });
    ui.within(&row, &mut |ui| {
        let left = ui.node("left", Node { w: Size::Px(240.0), ..Node::DEFAULT });
        ui.within(&left, &mut |ui| {
            s.check = ui.checkbox("Check", s.check);
            s.radio = ui.radio("Radio", &["One", "Two", "Three"], s.radio);
            s.slider = ui.slider("Level", s.slider, 0.0..=1.0);
            ui.progress("Done", Some(0.5));
            let p = ui.progress("Busy", None);
            s.busy.push(p);
            let sp = ui.spinner("Spin");
            s.busy.push(sp);
            s.tab = ui.tabs("Tabs", &["Alpha", "Beta", "Gamma"], s.tab);
            s.seg = ui.segmented("Mode", &["A", "B", "C"], s.seg);
            s.drop = ui.dropdown("Pick", &NAMES, s.drop);
            let holder = ui.node("add-row", Node { h: Size::Px(24.0), axis: Axis::Row, padding: Edges { l: 12.0, t: 2.0, ..Edges::ZERO }, ..Node::DEFAULT });
            ui.within(&holder, &mut |ui| {
                if ui.icon_button("Add", "+") {
                    s.icon += 1;
                }
            });
            s.add = ui.box_of_key(key_of(holder.box_key, "Add"));
            ui.badge("count", "12");
            if let Some(q) = ui.search_field("Search", "/", &s.search, "Search") {
                s.search = q;
            }
            s.scroll = Some(ui.scroll_view("Scroll", Size::Grow, Size::Px(60.0), &mut |ui| {
                for name in NAMES {
                    let n = ui.node(name, Node { w: Size::Px(400.0), h: Size::Px(24.0), ..Node::DEFAULT });
                    ui.text(&n, name);
                }
            }));
            if let Some(image) = s.image.clone() {
                ui.image("Picture", &image, pxui::draw::Fit::Contain, Size::Px(48.0), Size::Px(32.0));
            }
        });
        let right = ui.node("right", Node { w: Size::Px(240.0), ..Node::DEFAULT });
        ui.within(&right, &mut |ui| {
            let b = ui.node("list-box", Node { h: Size::Px(120.0), ..Node::DEFAULT });
            ui.within(&b, &mut |ui| {
                if let Some(a) = ui.list_view("List", BIG, s.multi, &mut s.list, &names) {
                    s.activated = Some(a);
                }
            });
            s.list_frame = ui.box_of_key(key_of(b.box_key, "List"));
            sized(ui, "tree-box", 120.0, &mut |ui| {
                ui.tree("Tree", &s.nodes, &mut s.open, &mut s.tree_sel, &mut s.visible, &names);
            });
            sized(ui, "table-box", 150.0, &mut |ui| {
                if let Some(c) = ui.table_view("Table", &["Name", "Next", "More"], &mut s.widths, BIG, Some((0, true)), &mut s.table_sel, &cells) {
                    s.sorted = Some(c);
                }
            });
        });
    });
}

/// A UI with `s` built over two frames.
fn warm(s: &mut S) -> Ui {
    let mut ui = new_ui(None);
    ui.set_a11y(true); // these cases read roles and values from the tree
    for t in 0..2 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut |ui| all(ui, s));
    }
    ui
}

/// Click the centre of `p` (move, press, release, then a quiet frame), from time `t`.
fn click(ui: &mut Ui, s: &mut S, t: f64, p: (i64, i64)) {
    click_with(ui, s, t, p, vec![]);
}

fn click_with(ui: &mut Ui, s: &mut S, t: f64, p: (i64, i64), keys: Vec<Key>) {
    for (i, e) in [vec![move_to(p)], vec![press(p)], vec![release(p)], vec![]].into_iter().enumerate() {
        ui.frame(&at_keys(keys.clone(), t + i as f64 * 0.1, e), &mut |ui| all(ui, s));
    }
}

/// Press keys (one frame each, then a quiet frame), from time `t`.
fn keys(ui: &mut Ui, s: &mut S, t: f64, ks: &[Key]) {
    for (i, k) in ks.iter().enumerate() {
        ui.frame(&at(t + i as f64 * 0.1, vec![Event::KeyPressed(*k)]), &mut |ui| all(ui, s));
    }
    ui.frame(&at(t + ks.len() as f64 * 0.1, vec![]), &mut |ui| all(ui, s));
}

/// Tab until the focused node is `role` `name`; panics after 60 stops.
fn tab_to(ui: &mut Ui, s: &mut S, t: f64, role: Role, name: &str) {
    for i in 0..60 {
        if node(ui, role, name).states & state::FOCUSED != 0 {
            return;
        }
        ui.frame(&at(t + i as f64 * 0.01, vec![Event::KeyPressed(Key::Tab)]), &mut |ui| all(ui, s));
    }
    panic!("Tab never reached {role:?} {name}");
}

fn widgets_checkbox() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let n = node(&ui, Role::CheckBox, "Check");
    check(n.states & state::CHECKABLE != 0 && n.states & state::CHECKED == 0, "checkbox starts unchecked in the tree");
    click(&mut ui, &mut s, 1.0, centre(&n));
    check(s.check && node(&ui, Role::CheckBox, "Check").states & state::CHECKED != 0, "a click checks it");
    tab_to(&mut ui, &mut s, 2.0, Role::CheckBox, "Check");
    keys(&mut ui, &mut s, 3.0, &[Key::Space]);
    check(!s.check, "Space unchecks it");
    ui.destroy();
}

fn widgets_radio() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    { let p = centre(&node(&ui, Role::Radio, "Three")); click(&mut ui, &mut s, 1.0, p); }
    check(s.radio == 2 && node(&ui, Role::Radio, "Three").states & state::CHECKED != 0, "a click chooses Three");
    tab_to(&mut ui, &mut s, 2.0, Role::Group, "Radio");
    keys(&mut ui, &mut s, 3.0, &[Key::ArrowDown]);
    check(s.radio == 0, &format!("Down wraps to One: {}", s.radio));
    keys(&mut ui, &mut s, 4.0, &[Key::ArrowUp]);
    check(s.radio == 2, "Up wraps back to Three");
    ui.destroy();
}

fn widgets_slider() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let n = node(&ui, Role::Slider, "Level");
    let y = (n.rect.y + n.rect.h / 2.0) as i64;
    let x = |f: f32| (n.rect.x + 4.0 + f * (n.rect.w - 8.0)).round() as i64;
    // press at a quarter, drag to three quarters, release
    for (t, e) in [(1.0, vec![move_to((x(0.1), y))]), (1.1, vec![press((x(0.1), y))]), (1.2, vec![move_to((x(0.75), y))]), (1.3, vec![release((x(0.75), y))])] {
        ui.frame(&at(t, e), &mut |ui| all(ui, &mut s));
    }
    check((s.slider - 0.75).abs() < 0.02, &format!("a drag to three quarters: {}", s.slider));
    tab_to(&mut ui, &mut s, 2.0, Role::Slider, "Level");
    keys(&mut ui, &mut s, 3.0, &[Key::Home, Key::ArrowRight, Key::ArrowRight]);
    check((s.slider - 0.02).abs() < 1e-4, &format!("Home then two steps: {}", s.slider));
    let n = node(&ui, Role::Slider, "Level");
    check(ui.output().a11y.value_text(&n) == "0.02", &format!("the value in the tree: {:?}", ui.output().a11y.value_text(&n)));
    ui.inject_a11y(&[A11yAction { id: n.id, action: Action::SetValue, payload: 0.5 }]);
    ui.frame(&at(4.0, vec![]), &mut |ui| all(ui, &mut s));
    check(s.slider == 0.5, "SetValue moves it");
    // right to left: the end is at the left
    ui.set_dir(Direction::Rtl);
    for t in 0..2 {
        ui.frame(&at(5.0 + t as f64 * 0.1, vec![]), &mut |ui| all(ui, &mut s));
    }
    let n = node(&ui, Role::Slider, "Level");
    let p = ((n.rect.x + 5.0) as i64, y);
    click(&mut ui, &mut s, 6.0, p);
    check(s.slider > 0.97, &format!("a press at the left end under RTL is the top: {}", s.slider));
    ui.destroy();
}

/// The indeterminate bar and the spinner move through patches: no frame, no build, no allocation.
fn widgets_busy_patches() {
    let mut s = S::new();
    let mut ui = new_ui(None);
    ui.set_a11y(true);
    let build = |ui: &mut Ui, s: &mut S| {
        ui.view("busy", 1, Size::Px(240.0), Size::Px(100.0), &mut |ui| {
            s.busy.clear();
            let p = ui.progress("Busy", None);
            s.busy.push(p);
            let sp = ui.spinner("Spin");
            s.busy.push(sp);
            ui.progress("Done", Some(0.5));
        });
    };
    for t in 0..3 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut |ui| build(ui, &mut s));
    }
    let tree = ui.output().a11y;
    let done = find(tree, Role::Progress, "Done").expect("determinate");
    check(tree.value_text(done) == "50", "a half bar reads 50");
    check(find(tree, Role::Progress, "Busy").unwrap().states & state::BUSY != 0, "the sweep is busy");
    let busy = s.busy.clone();
    let revision = ui.draw_revision();
    // a kept frame declares the root and its view box only
    ui.frame(&at(0.4, vec![]), &mut |ui| build(ui, &mut s));
    check(ui.built_nodes() <= 2, &format!("a kept frame built {} nodes", ui.built_nodes()));
    for i in 0..5 {
        for h in &busy {
            ui.busy_step(h, 1.0 + i as f64 / 60.0);
        }
    }
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    let mut moved = 0;
    for i in 0..60 {
        for h in &busy {
            moved += ui.busy_step(h, 2.0 + i as f64 / 60.0) as u32;
        }
        check(!ui.wants_frame(&[], 2.0 + i as f64 / 60.0), "a patch asked for a frame");
    }
    let allocs = mem_sys::local_alloc_count() - before;
    check(allocs == 0, &format!("{allocs} allocations in 60 animated steps"));
    check(moved == 120 && ui.draw_revision() > revision, &format!("{moved} steps moved"));
    check(ui.built_nodes() <= 2, "a patch built nodes");
    // reduced motion: nothing moves
    let mut theme = *ui.theme();
    theme.tokens.reduced_motion = true;
    ui.set_theme(theme);
    check(!ui.busy_step(&busy[1], 3.0), "a spinner turns under reduced motion");
    ui.destroy();
}

fn widgets_tabs_segmented() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    { let p = centre(&node(&ui, Role::Tab, "Gamma")); click(&mut ui, &mut s, 1.0, p); }
    check(s.tab == 2 && node(&ui, Role::Tab, "Gamma").states & state::SELECTED != 0, "a click selects Gamma");
    tab_to(&mut ui, &mut s, 2.0, Role::TabList, "Tabs");
    keys(&mut ui, &mut s, 3.0, &[Key::ArrowRight]);
    check(s.tab == 0, "Right wraps to Alpha");
    { let p = centre(&node(&ui, Role::Radio, "B")); click(&mut ui, &mut s, 4.0, p); }
    check(s.seg == 1, "a click picks segment B");
    tab_to(&mut ui, &mut s, 5.0, Role::Group, "Mode");
    keys(&mut ui, &mut s, 6.0, &[Key::ArrowRight, Key::ArrowRight]);
    check(s.seg == 2, "Right moves to C and holds there");
    ui.destroy();
}

fn widgets_dropdown() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let button = node(&ui, Role::ComboBox, "Pick");
    click(&mut ui, &mut s, 1.0, centre(&button));
    check(node(&ui, Role::ComboBox, "Pick").states & state::EXPANDED != 0, "a click opens it");
    { let p = centre(&node(&ui, Role::MenuItem, "delta")); click(&mut ui, &mut s, 2.0, p); }
    check(s.drop == 3 && find(ui.output().a11y, Role::MenuItem, "beta").is_none(), &format!("picking delta closes it: {}", s.drop));
    tab_to(&mut ui, &mut s, 3.0, Role::ComboBox, "Pick");
    keys(&mut ui, &mut s, 4.0, &[Key::ArrowDown, Key::ArrowDown, Key::Enter]);
    check(s.drop == 4, &format!("Down opens, Down moves, Enter picks: {}", s.drop));
    keys(&mut ui, &mut s, 5.0, &[Key::ArrowDown]);
    click(&mut ui, &mut s, 6.0, (470, 700));
    check(find(ui.output().a11y, Role::MenuItem, "beta").is_none(), "a press outside closes it");
    ui.destroy();
}

fn widgets_menu_bar() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    { let p = centre(&node(&ui, Role::MenuItem, "File")); click(&mut ui, &mut s, 1.0, p); }
    check(find(ui.output().a11y, Role::MenuItem, "Open").is_some(), "File opens");
    // hovering Edit while File is open switches to it
    let edit = centre(&node(&ui, Role::MenuItem, "Edit"));
    for t in [2.0, 2.1] {
        ui.frame(&at(t, vec![move_to(edit)]), &mut |ui| all(ui, &mut s));
    }
    check(find(ui.output().a11y, Role::MenuItem, "Redo").is_some() && find(ui.output().a11y, Role::MenuItem, "Open").is_none(), "hover switches to Edit");
    { let p = centre(&node(&ui, Role::MenuItem, "Cut")); click(&mut ui, &mut s, 3.0, p); }
    check(s.picked == Some((1, 2)) && find(ui.output().a11y, Role::MenuItem, "Cut").is_none(), &format!("Cut picked: {:?}", s.picked));
    tab_to(&mut ui, &mut s, 4.0, Role::MenuBar, "menus");
    keys(&mut ui, &mut s, 5.0, &[Key::ArrowRight, Key::ArrowDown, Key::ArrowDown, Key::Enter]);
    check(s.picked == Some((0, 1)), &format!("from Edit, Right, Down, Down, Enter picks File/Open: {:?}", s.picked));
    ui.destroy();
}

fn widgets_tip_and_icon() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let add = node(&ui, Role::Button, "Add");
    click(&mut ui, &mut s, 1.0, centre(&add));
    check(s.icon == 1, "the icon button clicks");
    let tip = |ui: &mut Ui, s: &mut S| {
        let h = all_and_tip(ui, s);
        ui.tip(&h, "Add a row");
    };
    for t in 0..6 {
        ui.frame(&at(2.0 + t as f64 * 0.1, vec![move_to(centre(&add))]), &mut |ui| tip(ui, &mut s));
    }
    check(find(ui.output().a11y, Role::Tooltip, "Add a row").is_some(), "a resting pointer shows the tip");
    let badge = node(&ui, Role::Label, "count");
    check(ui.output().a11y.value_text(&badge) == "12", "the badge's count");
    ui.destroy();
}

/// `all` plus a handle on the icon button.
fn all_and_tip(ui: &mut Ui, s: &mut S) -> Handle {
    all(ui, s);
    s.add.expect("the icon button")
}

fn widgets_scroll_view() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let n = node(&ui, Role::ScrollArea, "Scroll");
    let p = centre(&n);
    ui.frame(&at(1.0, vec![move_to(p)]), &mut |ui| all(ui, &mut s));
    for t in 0..20 {
        ui.frame(&at(1.1 + t as f64 * 0.05, vec![Event::Scrolled((-3.0, -3.0))]), &mut |ui| all(ui, &mut s));
    }
    for t in 0..40 {
        ui.frame(&at(2.5 + t as f64 * 0.05, vec![]), &mut |ui| all(ui, &mut s));
    }
    let scroll = s.scroll.expect("scroll view");
    check(ui.scroll_offset(&scroll) > 0.0, &format!("the wheel scrolls it down: {}", ui.scroll_offset(&scroll)));
    check(ui.scroll_offset_x(&scroll) > 0.0, &format!("and sideways: {}", ui.scroll_offset_x(&scroll)));
    ui.destroy();
}

fn widgets_list() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let rows = ui.output().a11y.nodes.iter().filter(|n| n.role == Role::ListItem).count();
    check(rows > 0 && rows < 30, &format!("100,000 rows built {rows}"));
    let third = *ui.output().a11y.nodes.iter().filter(|n| n.role == Role::ListItem).nth(2).unwrap();
    click(&mut ui, &mut s, 1.0, centre(&third));
    check(s.list[2] && s.list.iter().filter(|b| **b).count() == 1, "a click selects row 2 alone");
    check(ui.output().a11y.nodes.iter().filter(|n| n.role == Role::ListItem && n.states & state::SELECTED != 0).count() == 1, "the tree shows it selected");
    // double click activates
    let p = centre(&third);
    for (t, e) in [(2.0, vec![press(p)]), (2.05, vec![release(p)]), (2.1, vec![press(p)]), (2.15, vec![release(p)])] {
        ui.frame(&at(t, e), &mut |ui| all(ui, &mut s));
    }
    check(s.activated == Some(2), "a double click activates row 2");
    ui.destroy();
}

fn widgets_list_keys_multi() {
    let mut s = S::new();
    s.multi = true;
    let mut ui = warm(&mut s);
    let item = |ui: &Ui, i: usize| *ui.output().a11y.nodes.iter().filter(|n| n.role == Role::ListItem).nth(i).unwrap();
    { let p = centre(&item(&ui, 1)); click(&mut ui, &mut s, 1.0, p); }
    { let p = centre(&item(&ui, 3)); click_with(&mut ui, &mut s, 2.0, p, vec![Key::Meta]); }
    check(s.list[1] && s.list[3] && !s.list[2], "Command-click adds row 3");
    { let p = centre(&item(&ui, 4)); click_with(&mut ui, &mut s, 3.0, p, vec![Key::Shift]); }
    check((3..=4).all(|i| s.list[i]) && !s.list[1], &format!("Shift-click selects the run 3..4: {:?}", &s.list[..8]));
    // Tab to the list (its frame holds the focus), then Down and Space
    for i in 0..40 {
        if s.list_frame.is_some_and(|b| ui.focused(&b)) {
            break;
        }
        ui.frame(&at(4.0 + i as f64 * 0.01, vec![Event::KeyPressed(Key::Tab)]), &mut |ui| all(ui, &mut s));
    }
    check(s.list_frame.is_some_and(|b| ui.focused(&b)), "Tab reaches the list");
    keys(&mut ui, &mut s, 5.0, &[Key::ArrowDown, Key::Space]);
    check(s.list[5] && s.list[4], "Down then Space adds row 5");
    ui.destroy();
}

fn widgets_tree() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let items: Vec<A11yNode> = ui.output().a11y.nodes.iter().filter(|n| n.role == Role::TreeItem).copied().collect();
    check(!items.is_empty() && items.len() < 30, &format!("100,000 nodes built {} rows", items.len()));
    check(s.visible.len() == BIG / 10, "collapsed: one row per top node");
    let first = items[0];
    check(first.states & state::EXPANDABLE != 0 && first.states & state::EXPANDED == 0, "a parent is expandable and closed");
    // the chevron sits at the row's start
    click(&mut ui, &mut s, 1.0, ((first.rect.x + 12.0 + 5.0) as i64, centre(&first).1));
    check(s.open[0] && s.visible.len() == BIG / 10 + 9, "the chevron opens node 0");
    let first = *ui.output().a11y.nodes.iter().find(|n| n.role == Role::TreeItem).unwrap();
    check(first.states & state::EXPANDED != 0, "the tree shows it expanded");
    let child = *ui.output().a11y.nodes.iter().filter(|n| n.role == Role::TreeItem).nth(1).unwrap();
    check(child.rect.w > 0.0 && ui.output().a11y.str(child.name) == "beta", "its first child is beta, indented under it");
    click(&mut ui, &mut s, 2.0, centre(&child));
    check(s.tree_sel == Some(1), "a click selects node 1");
    ui.destroy();
}

fn widgets_table() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let rows = ui.output().a11y.nodes.iter().filter(|n| n.role == Role::Row).count();
    check(rows > 0 && rows < 30, &format!("100,000 rows built {rows}"));
    let head = node(&ui, Role::ColumnHeader, "Next");
    click(&mut ui, &mut s, 1.0, centre(&head));
    check(s.sorted == Some(1), "a header click asks to sort by it");
    // drag the grip after the first column 30 points right
    let name = node(&ui, Role::ColumnHeader, "Name");
    let g = ((name.rect.x + name.rect.w) as i64, centre(&name).1);
    for (t, e) in [(2.0, vec![move_to(g)]), (2.1, vec![press(g)]), (2.2, vec![move_to((g.0 + 30, g.1))]), (2.3, vec![release((g.0 + 30, g.1))]), (2.4, vec![])] {
        ui.frame(&at(t, e), &mut |ui| all(ui, &mut s));
    }
    check((s.widths[0] - 110.0).abs() < 0.5, &format!("the grip widens Name to 110: {:?}", s.widths));
    let row = *ui.output().a11y.nodes.iter().filter(|n| n.role == Role::Row).nth(1).unwrap();
    click(&mut ui, &mut s, 3.0, centre(&row));
    check(s.table_sel == Some(1), "a click selects row 1");
    check(ui.output().a11y.nodes.iter().any(|n| n.role == Role::Cell), "cells are in the tree");
    ui.destroy();
}

fn widgets_dialog() {
    let mut ui = new_ui(None);
    ui.set_a11y(true);
    let mut out = DialogOut::Open;
    let mut run = |ui: &mut Ui, t: f64, events: Vec<Event>| {
        ui.frame(&at(t, events), &mut |ui| {
            out = ui.dialog("Delete file", &["Cancel", "Delete"], &mut |ui| ui.label("This cannot be undone"));
        });
        out
    };
    run(&mut ui, 0.0, vec![]);
    run(&mut ui, 0.1, vec![]);
    let tree = ui.output().a11y;
    check(find(tree, Role::Dialog, "Delete file").is_some_and(|n| n.states & state::MODAL != 0), "a modal dialog");
    check(find(tree, Role::Heading, "Delete file").is_some() && find(tree, Role::Button, "Delete").is_some(), "its title and buttons");
    let cancel = centre(&node(&ui, Role::Button, "Cancel"));
    run(&mut ui, 0.2, vec![move_to(cancel)]);
    run(&mut ui, 0.3, vec![press(cancel)]);
    check(run(&mut ui, 0.4, vec![release(cancel)]) == DialogOut::Button(0), "Cancel clicks");
    check(run(&mut ui, 0.5, vec![Event::KeyPressed(Key::Enter)]) == DialogOut::Button(1), "Enter presses the default");
    check(run(&mut ui, 0.6, vec![Event::KeyPressed(Key::Escape)]) == DialogOut::Closed, "Escape closes it");
    ui.destroy();
}

fn widgets_toast_and_search() {
    let mut s = S::new();
    s.toasts = 2;
    let mut ui = warm(&mut s);
    let saved = node(&ui, Role::StatusBar, "Saved");
    let exported = node(&ui, Role::StatusBar, "Exported");
    check(saved.rect.y < exported.rect.y && exported.rect.x + exported.rect.w <= W as f32 - 12.0 + 0.5, "stacked in the lower right, newest lowest");
    check(exported.rect.y + exported.rect.h <= H as f32 - 12.0 + 0.5, "12 points from the bottom");
    click(&mut ui, &mut s, 1.0, centre(&saved));
    check(s.gone == Some(0), "a click dismisses Saved");
    let field = node(&ui, Role::TextField, "Search");
    click(&mut ui, &mut s, 2.0, centre(&field));
    for (i, c) in "abc".chars().enumerate() {
        ui.frame(&at(3.0 + i as f64 * 0.1, vec![Event::TextInput(c.to_string())]), &mut |ui| all(ui, &mut s));
    }
    check(s.search == "abc", &format!("typing searches: {:?}", s.search));
    check(ui.output().a11y.value_text(&node(&ui, Role::TextField, "Search")) == "abc", "the query in the tree");
    { let p = centre(&node(&ui, Role::Button, "Clear")); click(&mut ui, &mut s, 4.0, p); }
    check(s.search.is_empty(), "Clear empties it");
    ui.destroy();
}

/// A toast goes by itself: the UI asks for the frame at its expiry and reports it then.
fn widgets_toast_expires() {
    let mut s = S::new();
    s.toasts = 2;
    let mut ui = warm(&mut s); // frames at 0.0 and 0.1: both toasts showed at 0.0
    check(ui.repaint_at() == Some(TOAST_SECONDS), &format!("the expiry is the next frame: {:?}", ui.repaint_at()));
    check(!ui.wants_frame(&[], TOAST_SECONDS - 0.01) && s.gone.is_none(), "nothing before it");
    ui.frame(&at(TOAST_SECONDS, vec![]), &mut |ui| all(ui, &mut s));
    check(s.gone == Some(0), &format!("the first toast went at its time: {:?}", s.gone));
    // the app drops it; the other keeps its own time (keyed by text, not place)
    (s.toasts, s.gone) = (0, None);
    ui.frame(&at(TOAST_SECONDS + 0.1, vec![]), &mut |ui| all(ui, &mut s));
    check(ui.repaint_at().is_none(), &format!("no toast, no timer: {:?}", ui.repaint_at()));
    ui.destroy();
}

/// Keyboard focus asks for no frames, and a click focuses as on macOS: what edits or selects
/// (slider, list, search field) takes the focus, buttons and boxes do not.
fn widgets_focus_idle_and_click() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let focused = |ui: &Ui, role: Role, name: &str| node(ui, role, name).states & state::FOCUSED != 0;
    tab_to(&mut ui, &mut s, 1.0, Role::CheckBox, "Check");
    ui.frame(&at(2.0, vec![]), &mut |ui| all(ui, &mut s));
    check(ui.repaint_at().is_none() && !ui.wants_frame(&[], 100.0), &format!("a focused checkbox asks for frames: {:?}", ui.repaint_at()));
    { let p = centre(&node(&ui, Role::TextField, "Search")); click(&mut ui, &mut s, 3.0, p); }
    check(focused(&ui, Role::TextField, "Search"), "a click focuses the search field");
    check(ui.repaint_at().is_none() && !ui.wants_frame(&[], 100.0), &format!("a focused text field asks for frames: {:?}", ui.repaint_at()));
    { let p = centre(&node(&ui, Role::CheckBox, "Check")); click(&mut ui, &mut s, 4.0, p); }
    check(!focused(&ui, Role::CheckBox, "Check") && !focused(&ui, Role::TextField, "Search"), "a click on a checkbox takes no focus and drops the field's");
    { let p = centre(&node(&ui, Role::Slider, "Level")); click(&mut ui, &mut s, 5.0, p); }
    check(focused(&ui, Role::Slider, "Level"), "a click focuses the slider");
    let row = *ui.output().a11y.nodes.iter().find(|n| n.role == Role::ListItem).expect("a list row");
    click(&mut ui, &mut s, 6.0, centre(&row));
    check(s.list_frame.is_some_and(|b| ui.focused(&b)), "a click on a row focuses its list");
    ui.destroy();
}

fn widgets_image() {
    let mut s = S::new();
    s.image = Some(Image { handle: Rc::new(()), width: 64, height: 32 });
    let ui = warm(&mut s);
    let n = node(&ui, Role::Image, "Picture");
    check(n.rect.w == 48.0 && n.rect.h == 32.0, &format!("the image box: {:?}", n.rect));
    let images: usize = ui.output().draw.iter().map(|l| l.breaks.iter().filter(|b| b.image != pxui::draw::ATLAS).count()).sum();
    check(images == 1, &format!("one image break: {images}"));
}

/// Tab visits each widget's one stop in build order, and Shift-Tab goes back.
fn widgets_tab_order() {
    let mut s = S::new();
    let mut ui = warm(&mut s);
    let mut seen: Vec<(Role, String)> = Vec::new();
    for i in 0..18 {
        ui.frame(&at(1.0 + i as f64 * 0.01, vec![Event::KeyPressed(Key::Tab)]), &mut |ui| all(ui, &mut s));
        let t = ui.output().a11y;
        if let Some(n) = t.nodes.iter().find(|n| n.states & state::FOCUSED != 0) {
            seen.push((n.role, t.str(n.name).to_owned()));
        }
    }
    let order: Vec<&str> = seen.iter().map(|(_, n)| n.as_str()).collect();
    let want = ["menus", "Check", "Radio", "Level", "Tabs", "Mode", "Pick", "Add"];
    check(order.len() >= want.len() && order[..want.len()] == want, &format!("Tab order {order:?}"));
    let mut back = Vec::new();
    for i in 0..2 {
        let shift = Event::KeyPressed(Key::Tab);
        ui.frame(&at_keys(vec![Key::Shift], 2.0 + i as f64 * 0.01, vec![shift]), &mut |ui| all(ui, &mut s));
        let t = ui.output().a11y;
        if let Some(n) = t.nodes.iter().find(|n| n.states & state::FOCUSED != 0) {
            back.push(t.str(n.name).to_owned());
        }
    }
    check(back.len() == 2 && back[0] != back[1], &format!("Shift-Tab steps back: {back:?}"));
    ui.destroy();
}

/// A warm frame of every new widget allocates nothing, rebuilt every frame and kept in a view.
fn widgets_zero_alloc() {
    let mut s = S::new();
    s.toasts = 2;
    s.image = Some(Image { handle: Rc::new(()), width: 64, height: 32 });
    let mut ui = new_ui(None);
    for (name, keep) in [("rebuilt", false), ("kept", true)] {
        let mut frame = 0u64;
        let mut run = |ui: &mut Ui, s: &mut S, t: f64| {
            frame += 1;
            let revision = if keep { 1 } else { frame };
            ui.frame(&at(t, vec![]), &mut |ui| {
                ui.view("all", revision, Size::Px(W as f32), Size::Px(H as f32), &mut |ui| body(ui, s));
                toasts(ui, s);
            });
        };
        for t in 0..10 {
            run(&mut ui, &mut s, t as f64 * 0.1);
        }
        mem_sys::local_reset();
        let before = mem_sys::local_alloc_count();
        for t in 10..40 {
            run(&mut ui, &mut s, t as f64 * 0.1);
        }
        let allocs = mem_sys::local_alloc_count() - before;
        println!("PXUI widgets {name}: {allocs} allocations in 30 warm frames, {} nodes built", ui.built_nodes());
        check(allocs == 0, &format!("{name}: {allocs} allocations in 30 warm frames"));
        check(!keep || ui.built_nodes() < 12, &format!("the kept view rebuilt: {} nodes", ui.built_nodes()));
    }
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("widgets_checkbox", widgets_checkbox),
    ("widgets_radio", widgets_radio),
    ("widgets_slider", widgets_slider),
    ("widgets_busy_patches", widgets_busy_patches),
    ("widgets_tabs_segmented", widgets_tabs_segmented),
    ("widgets_dropdown", widgets_dropdown),
    ("widgets_menu_bar", widgets_menu_bar),
    ("widgets_tip_and_icon", widgets_tip_and_icon),
    ("widgets_scroll_view", widgets_scroll_view),
    ("widgets_list", widgets_list),
    ("widgets_list_keys_multi", widgets_list_keys_multi),
    ("widgets_tree", widgets_tree),
    ("widgets_table", widgets_table),
    ("widgets_dialog", widgets_dialog),
    ("widgets_toast_and_search", widgets_toast_and_search),
    ("widgets_toast_expires", widgets_toast_expires),
    ("widgets_focus_idle_and_click", widgets_focus_idle_and_click),
    ("widgets_image", widgets_image),
    ("widgets_tab_order", widgets_tab_order),
    ("widgets_zero_alloc", widgets_zero_alloc),
];
