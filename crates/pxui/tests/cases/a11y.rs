//! The accessibility tree the engine emits from the kit's roles, and actions injected back.
use crate::support::*;
use input::Event;
use pxui::a11y::{state, A11yAction, Action, A11yTree, Role, NO_PARENT};
use pxui::ui::*;

fn at(t: f64, events: Vec<Event>) -> input::frame::Frame {
    frame_at(vec![], 1.0, t, events)
}

fn find<'a>(t: &'a A11yTree, role: Role, name: &str) -> Option<&'a pxui::a11y::A11yNode> {
    t.nodes.iter().find(|n| n.role == role && t.str(n.name) == name)
}

/// Run `f` as the kit sketch does, with `value` and `on` fed back.
struct Kit {
    on: bool,
    caption: String,
    quit: u32,
}

fn kit(ui: &mut Ui, k: &mut Kit) {
    ui.panel(PanelOpts::default(), "panel", &mut |ui| {
        ui.label("PXUI");
        k.on = ui.toggle("Animate", k.on);
        if let Some(v) = ui.text_field("Caption", &k.caption) {
            k.caption = v;
        }
        if ui.button("Quit") {
            k.quit += 1;
        }
    });
}

pub fn a11y_kit_tree() {
    let mut ui = new_ui(None);
    ui.set_a11y(true);
    let mut k = Kit { on: false, caption: "Functional UI".to_owned(), quit: 0 };
    for t in 0..2 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut |ui| kit(ui, &mut k));
    }
    let tree = ui.output().a11y.clone();
    let panel = find(&tree, Role::Panel, "panel").expect("the panel");
    check(panel.parent == NO_PARENT && panel.rect == Rect::new(12.0, 12.0, 280.0, panel.rect.h), &format!("panel {panel:?}"));
    for (role, name) in [(Role::Label, "PXUI"), (Role::Toggle, "Animate"), (Role::TextField, "Caption"), (Role::Button, "Quit")] {
        let n = find(&tree, role, name).unwrap_or_else(|| panic!("no {role:?} {name}: {:?}", tree.nodes.iter().map(|n| (n.role, tree.str(n.name))).collect::<Vec<_>>()));
        check(tree.nodes[n.parent as usize].role == Role::Panel, &format!("{name} is not in the panel"));
        check(n.states & state::VISIBLE != 0 && n.states & state::ENABLED != 0, &format!("{name} states {:x}", n.states));
        check(n.rect.w > 0.0 && n.rect.h > 0.0, &format!("{name} rect {:?}", n.rect));
    }
    let toggle = find(&tree, Role::Toggle, "Animate").unwrap();
    check(toggle.states & state::CHECKABLE != 0 && toggle.states & state::CHECKED == 0, "the toggle starts unchecked");
    check(toggle.actions & pxui::a11y::actions::PRESS != 0, "a toggle can be pressed");
    let field = find(&tree, Role::TextField, "Caption").unwrap();
    check(tree.value_text(field) == "Functional UI" && field.states & state::EDITABLE != 0, "the field's value");
    let quit = find(&tree, Role::Button, "Quit").unwrap();
    check(quit.rect.h == 20.0, &format!("the button is its 20-point box: {:?}", quit.rect));
    // an unchanged frame keeps the revision
    let revision = tree.revision;
    ui.frame(&at(0.3, vec![]), &mut |ui| kit(ui, &mut k));
    check(ui.output().a11y.revision == revision, "an unchanged frame bumped the revision");
    // a click on the toggle checks it in the tree
    let r = toggle.rect;
    let p = ((r.x + r.w - 20.0) as i64, (r.y + r.h / 2.0) as i64);
    for (t, e) in [(0.4, vec![move_to(p)]), (0.5, vec![press(p)]), (0.6, vec![release(p)]), (0.7, vec![])] {
        ui.frame(&at(t, e), &mut |ui| kit(ui, &mut k));
    }
    let tree = ui.output().a11y;
    check(k.on && find(tree, Role::Toggle, "Animate").unwrap().states & state::CHECKED != 0, "the clicked toggle is not checked in the tree");
    check(tree.revision > revision, "a change did not bump the revision");
    ui.destroy();
}

pub fn a11y_inject_actions() {
    let mut ui = new_ui(None);
    ui.set_a11y(true);
    let mut k = Kit { on: false, caption: "Functional UI".to_owned(), quit: 0 };
    for t in 0..2 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut |ui| kit(ui, &mut k));
    }
    let tree = ui.output().a11y;
    let quit = find(tree, Role::Button, "Quit").unwrap().id;
    let field = find(tree, Role::TextField, "Caption").unwrap().id;
    ui.inject_a11y(&[A11yAction { id: quit, action: Action::Press, payload: 0.0 }, A11yAction { id: field, action: Action::SetValue, payload: 42.0 }]);
    ui.frame(&at(0.2, vec![]), &mut |ui| kit(ui, &mut k));
    check(k.quit == 1, "an injected Press did not click the button");
    check(k.caption == "42", &format!("an injected SetValue did not edit the field: {:?}", k.caption));
    ui.frame(&at(0.3, vec![]), &mut |ui| kit(ui, &mut k));
    check(k.quit == 1, "the press clicked twice");
    check(ui.output().a11y.value_text(find(ui.output().a11y, Role::TextField, "Caption").unwrap()) == "42", "the field's new value is not in the tree");
    // a stale id is dropped; Focus focuses
    ui.inject_a11y(&[A11yAction { id: quit ^ (1 << 40), action: Action::Press, payload: 0.0 }, A11yAction { id: field, action: Action::Focus, payload: 0.0 }]);
    ui.frame(&at(0.4, vec![]), &mut |ui| kit(ui, &mut k));
    check(k.quit == 1, "a stale id clicked");
    check(find(ui.output().a11y, Role::TextField, "Caption").unwrap().states & state::FOCUSED != 0, "Focus did not focus the field");
    ui.destroy();
}

/// A warm frame with the tree unchanged allocates nothing.
pub fn a11y_warm_zero_alloc() {
    let mut ui = new_ui(None);
    ui.set_a11y(true);
    let mut k = Kit { on: true, caption: "Functional UI".to_owned(), quit: 0 };
    for t in 0..10 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut |ui| kit(ui, &mut k));
    }
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    for t in 10..40 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut |ui| kit(ui, &mut k));
    }
    let allocs = mem_sys::local_alloc_count() - before;
    check(allocs == 0, &format!("{allocs} allocations in 30 warm frames with the tree"));
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("a11y_kit_tree", a11y_kit_tree),
    ("a11y_inject_actions", a11y_inject_actions),
    ("a11y_warm_zero_alloc", a11y_warm_zero_alloc),
];
