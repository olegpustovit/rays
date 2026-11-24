#![allow(clippy::all)]
//! the SOP-network presentation. `scope_pane` is the graph pane (`Scope`),
//! `node_menu` the add menu. The pane emits typed `Change` requests and never edits.
pub mod scope_wires;
pub mod scope_state;
pub mod node_menu;
pub mod scope_paint;
pub mod scope_commands;
pub mod scope_frame;
pub mod scope_hover;
pub mod scope_pane;

fn p32((x, y): (f64, f64)) -> (f32, f32) {
    (x as f32, y as f32)
}

fn p64((x, y): (f32, f32)) -> (f64, f64) {
    (f64::from(x), f64::from(y))
}

pub(crate) fn sized_view(ui: &mut pxui::Ui, label: &str, revision: u64, width: pxui::Size, height: pxui::Size, build: &mut dyn FnMut(&mut pxui::Ui)) -> pxui::Handle {
    let key = pxui::hash_of(label);
    let host = ui.node(key, pxui::Node::DEFAULT.w(width).h(height).at(0.0, 0.0));
    let mut view = None;
    ui.within(&host, &mut |ui| view = Some(ui.view_of(key, &revision, &mut *build)));
    view.expect("sized retained view")
}
