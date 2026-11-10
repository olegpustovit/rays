//! Node-menu interaction and typed intent contracts.
use crate::support::*;
use flow_syntax::ty;
use input::{Event, Key, MouseButton};
use pxui::theme;
use pxui::ui::*;
use pxui_graph::node_menu::{self, Entry, Menu};
use rays::frame as rframe;

const BOUNDS: (i64, i64, i64, i64) = (20, 30, 800, 520);

fn one_frame(ui: &mut Ui, menu: Menu, fr: &rframe::Frame) -> (Option<Menu>, Option<String>) {
    let mut cell = Some(menu);
    let mut out: Option<(Option<Menu>, Option<String>)> = None;
    ui.frame(fr, &mut |ui: &mut Ui| {
        out = Some(node_menu::update(cell.take().expect("menu"), ui, BOUNDS));
    });
    out.expect("frame")
}

/// The menu is built inside a UI frame; hit rectangles come from the previous frame, so each step first
/// settles the layout without events.
fn menu_step(ui: &mut Ui, menu: Menu, fr: &rframe::Frame) -> (Option<Menu>, Option<String>) {
    let mut settle = fr.clone();
    settle.input.events = Vec::new();
    match one_frame(ui, menu, &settle) {
        (None, _) => (None, None),
        (Some(menu), _) => one_frame(ui, menu, fr),
    }
}

fn menu_of(entries: Vec<Entry>) -> Menu {
    node_menu::create(None, 400, 250, entries)
}

fn entry(arity: i64, category: &[&str], key: &str, label: &str) -> Entry {
    Entry { key: key.to_owned(), label: label.to_owned(), category: category.iter().map(|s| s.to_string()).collect(), arity, context: "sop".to_owned(), output: ty::geometry(), off: None }
}

fn picks(ui: &mut Ui, menu: Menu, events: Vec<Event>) -> Option<String> {
    menu_step(ui, menu, &frame_ev((400, 250), events)).1
}

fn opened(ui: &mut Ui, entries: Vec<Entry>) -> Menu {
    match menu_step(ui, menu_of(entries), &frame_ev((400, 250), Vec::new())) {
        (Some(menu), _) => menu,
        (None, _) => fail("the menu closed without input"),
    }
}

fn key(k: Key) -> Event {
    Event::Key_pressed(k)
}

fn run_menu() {
    let mut ui = new_ui();
    // a SOP with an input picks like any other: typing its name and Enter
    let null = || vec![entry(1, &["Utility"], "null", "Null")];
    let m = opened(&mut ui, null());
    check(picks(&mut ui, m, vec![Event::Text_input("null".into()), key(Key::Enter)]) == Some("null".to_owned()), "the menu search did not pick a node");
    let m = opened(&mut ui, null());
    let r = menu_step(&mut ui, m, &frame_ev((400, 250), vec![key(Key::Escape)]));
    check(r.0.is_none() && r.1.is_none(), "Escape did not close the menu");
    // the kinds are listed from the start: Enter places the first row, and a click on a row picks it.
    // The first row's middle: the window's 1-point edge, its 28-point title (4 above a 24-point label),
    // the 28-point search row (a 20-point field in 4 points of padding), then half a 24-point row.
    let nested = || vec![entry(0, &["Create", "Primitive"], "box", "Box")];
    let m = opened(&mut ui, nested());
    check(picks(&mut ui, m, vec![key(Key::Enter)]) == Some("box".to_owned()), "the menu did not list its kinds from the start");
    let row_y = 250 + 1 + 28 + 28 + 12;
    let point = (420.0, row_y as f64);
    let m = opened(&mut ui, nested());
    let (_, picked) = menu_step(&mut ui, m, &frame_ev((420, row_y), vec![Event::Pointer_pressed(MouseButton::LeftButton, point), Event::Pointer_released(MouseButton::LeftButton, point)]));
    check(picked == Some("box".to_owned()), "a click on a row of the menu did not pick it");
    // after a node, the kinds that take an input come first
    let fitting = node_menu::create(Some("grid1"), 400, 250, vec![entry(0, &["Utility"], "box", "Box"), entry(1, &["Utility"], "null", "Null")]);
    check(node_menu::keys(&fitting, "") == vec!["null".to_owned(), "box".to_owned()], "the menu did not list what fits after the node first");
    // one colour per type, for the menu's squares and the pane's ports: a list is its elements'
    let th = theme::default();
    check(
        node_menu::port_color(&th, &ty::Ty::List(Box::new(ty::Ty::Float))) == theme::ports(&th).float && node_menu::port_color(&th, &ty::scene()) == theme::ports(&th).output,
        "a list or a scene is not coloured as the graph pane colours it",
    );
    // the visual row window never makes later nodes inaccessible
    let many: Vec<Entry> = (0..15).map(|i| entry(0, &["Utility"], &format!("node_{:02}", i), &format!("Node {:02}", i))).collect();
    let m = opened(&mut ui, many.clone());
    let menu = match menu_step(&mut ui, m, &frame_ev((400, 250), vec![Event::Text_input("node".into())])) {
        (Some(menu), _) => menu,
        (None, _) => fail("typing closed the menu"),
    };
    let mut events: Vec<Event> = (0..12).map(|_| key(Key::ArrowDown)).collect();
    events.push(key(Key::Enter));
    check(picks(&mut ui, menu, events) == Some("node_12".to_owned()), "the menu's row window made later nodes inaccessible");
    // global search matches a category breadcrumb and uses the shared fuzzy rule
    let m = opened(&mut ui, nested());
    check(picks(&mut ui, m, vec![Event::Text_input("primitive".into()), key(Key::Enter)]) == Some("box".to_owned()), "global search did not match a category breadcrumb");
    let m = opened(&mut ui, nested());
    check(picks(&mut ui, m, vec![Event::Text_input("prmtv".into()), key(Key::Enter)]) == Some("box".to_owned()), "node search did not use the shared fuzzy matching rule");
    // every generated SOP can be reached by typing its key
    let factories = sop::nodes::factories();
    let entries: Vec<Entry> = factories
        .iter()
        .map(|f| Entry {
            key: sop::edit_graph::factory_key(f).to_owned(),
            label: sop::edit_graph::factory_label(f).to_owned(),
            category: sop::edit_graph::factory_category(f).to_vec(),
            arity: sop::edit_graph::factory_arity(f) as i64,
            context: "sop".to_owned(),
            output: ty::geometry(),
            off: None,
        })
        .collect();
    check(entries.len() == factories.len(), "the menu conversion dropped a generated SOP descriptor");
    for e in &entries {
        let menu = opened(&mut ui, entries.clone());
        let index = node_menu::keys(&menu, &e.key).iter().position(|k| *k == e.key).expect("listed");
        let mut events = vec![Event::Text_input(e.key.clone())];
        events.extend((0..index).map(|_| key(Key::ArrowDown)));
        events.push(key(Key::Enter));
        check(picks(&mut ui, menu, events) == Some(e.key.clone()), &format!("the menu cannot reach generated SOP {}", e.key));
    }
}

pub const CASES: &[(&str, fn())] = &[("run_menu", run_menu)];
