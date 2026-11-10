//! Interaction steps use a 240-point panel, 24-point rows, a value column at 120 and toggle bounds [197,237).
use crate::support::*;
use input::{Event, Key};
use pxui::ui::*;
use std::cell::Cell;

thread_local! {
    static CLOCK: Cell<f64> = const { Cell::new(0.0) };
}

pub fn reset_clock() {
    CLOCK.with(|c| c.set(0.0));
}

pub fn advance() -> f64 {
    CLOCK.with(|c| {
        c.set(c.get() + 0.5);
        c.get()
    })
}

pub fn row(y: i64) -> i64 {
    3 + y * 24 + 12
}

pub fn label(scale: f64, message: &str) -> String {
    format!("{}x: {}", cfmt::g(scale, 6), message)
}

pub fn step(scale: f64, ui: &mut Ui, events: Vec<Event>, build: &mut dyn FnMut(&mut Ui)) {
    let t = advance();
    ui.frame(&frame_at(vec![], scale, t, events), &mut |ui| {
        ui.panel(PanelOpts::default().at(0.0, 0.0).width(240.0), "panel", &mut |ui| build(ui));
    });
}

pub fn modified_step(scale: f64, ui: &mut Ui, modifiers: Vec<Key>, k: Key, build: &mut dyn FnMut(&mut Ui)) {
    let t = advance();
    ui.frame(&frame_at(modifiers, scale, t, vec![key_down(k)]), &mut |ui| {
        ui.panel(PanelOpts::default().at(0.0, 0.0).width(240.0), "panel", &mut |ui| build(ui));
    });
}

pub fn command_step(scale: f64, ui: &mut Ui, c: char, build: &mut dyn FnMut(&mut Ui)) {
    modified_step(scale, ui, vec![Key::Meta], Key::KeyChar(c), build);
}

pub fn settle(scale: f64, ui: &mut Ui, build: &mut dyn FnMut(&mut Ui)) {
    step(scale, ui, vec![], build);
}

/// Runs `f` with the clipboard restored afterwards.
pub fn with_clipboard(f: &mut dyn FnMut()) {
    let previous = rays::clipboard::get_text();
    f();
    if let Ok(text) = previous {
        let _ = rays::clipboard::set_text(&text);
    }
}
