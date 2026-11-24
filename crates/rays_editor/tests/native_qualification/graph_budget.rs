//! What a graph-pane gesture costs (UI fleet L4): allocations on the frame of each interaction and the
//! settled frame after it, on isogrid. Ceilings are exact counts at measured + 5%, never raised.
use super::click_budget::{session, sketch, P};
use input::{Event, Key, MouseButton};

/// (action, ceiling on the key frame, ceiling on the third frame after it)
/// Set 2026-11-25 at measured + 5% (L4).
const CEILINGS: &[(&str, u64, u64)] = &[
    ("zoom rest", 636, 507),
    ("pan across overscan", 522, 1141),
    ("node select", 520, 508),
    ("box select", 519, 7587),
    ("after box select", 896, 611),
    ("escape", 698, 611),
    ("hover moves", 739, 618),
    ("enter", 704, 611),
    ("up", 708, 611),
];

fn rest(p: &mut P, name: &str, first: (f64, u64)) {
    let mut frames = vec![first];
    for _ in 0..3 {
        frames.push(p.step(&[], vec![], (1080., 200.)));
    }
    p.record(name, frames);
}

fn tour(p: &mut P) {
    let at = (1080., 200.);
    let ch = Key::KeyChar;
    // the graph pane is the one the tour's keys address: bring it up first
    p.step(&[], vec![Event::PointerMoved(at)], at);
    let f = p.step(&[], vec![Event::Scrolled((0., 1.))], at);
    rest(p, "zoom rest", f);
    let mut frames = Vec::new();
    frames.push(p.step(&[], vec![Event::PointerPressed(MouseButton::RightButton, at)], at));
    for i in 1..4 {
        let q = (at.0 + 100. * i as f64 - 150., at.1 + 20. * i as f64);
        frames.push(p.step(&[], vec![Event::PointerMoved(q)], q));
    }
    p.record("pan across overscan", frames);
    p.step(&[], vec![Event::PointerReleased(MouseButton::RightButton, (at.0 + 150., at.1 + 60.))], at);
    for _ in 0..3 {
        p.step(&[], vec![], at);
    }
    let f = p.step(&[], vec![Event::PointerPressed(MouseButton::LeftButton, at), Event::PointerReleased(MouseButton::LeftButton, at)], at);
    rest(p, "node select", f);
    let f = p.step(&[], vec![Event::PointerPressed(MouseButton::LeftButton, (900., 60.))], (900., 60.));
    let mut frames = vec![f];
    for i in 1..4 {
        let q = (900. + 100. * i as f64, 60. + 80. * i as f64);
        frames.push(p.step(&[], vec![Event::PointerMoved(q)], q));
    }
    p.record("box select", frames);
    p.step(&[], vec![Event::PointerReleased(MouseButton::LeftButton, (1200., 300.))], (1200., 300.));
    let f = p.step(&[], vec![], (1200., 300.));
    rest(p, "after box select", f);
    let f = p.step(&[], vec![Event::KeyPressed(Key::Escape)], (1200., 300.));
    rest(p, "escape", f);
    let frames = (0..4).map(|i| { let q = (1000. + 35. * i as f64, 150. + 30. * i as f64); p.step(&[], vec![Event::PointerMoved(q)], q) }).collect();
    p.record("hover moves", frames);
    let f = p.step(&[], vec![Event::KeyPressed(ch('i'))], at);
    rest(p, "enter", f);
    let f = p.step(&[Key::Meta], vec![Event::KeyPressed(Key::ArrowUp)], at);
    rest(p, "up", f);
}

fn isogrid() {
    let rows = session("isogrid graph", &sketch("isogrid.rays"), &tour);
    let mut over = Vec::new();
    let mut table = String::new();
    for (name, _, key, settled) in &rows {
        table.push_str(&format!("    (\"{name}\", {key}, {settled}),\n"));
        if let Some((_, k, s)) = CEILINGS.iter().find(|c| c.0 == name) {
            if key > k || settled > s {
                over.push(format!("{name}: {key}/{settled} over {k}/{s}"));
            }
        } else if !CEILINGS.is_empty() {
            over.push(format!("{name}: no ceiling"));
        }
    }
    eprintln!("CLICK graph ceilings now:\n{table}");
    assert!(over.is_empty(), "{}", over.join("; "));
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("graph_budget_isogrid", isogrid)]
}
