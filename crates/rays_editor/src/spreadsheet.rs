//! The spreadsheet panel's table over a cooked geometry's point, vertex, primitive or detail
//! attributes.
use std::rc::Rc;

use pxui::ui::{Rect, Size, Ui};
use rdk_rays::geometry_view as gv;

pub const LABELS: [&str; 4] = ["Point", "Vertex", "Primitive", "Detail"];

pub fn labels() -> Vec<String> {
    LABELS.iter().map(|s| s.to_string()).collect()
}

pub fn count(owner: i64, geometry: &gv::T) -> i64 {
    gv::count(owner, geometry)
}

pub fn columns(owner: i64, geometry: &gv::T) -> Vec<gv::Column> {
    gv::columns(owner, geometry)
}

/// Draws the owner tabs and the table in `bounds`; the tab clicked.
pub fn view(ui: &mut Ui, bounds: (i64, i64, i64, i64), owner: i64, geometry: Option<&gv::T>, state: &mut Rc<pxui::TableState>) -> Option<i64> {
    use std::hash::Hash;
    let revision = rays_shell::kit::revision(&|h| {
        (bounds, owner, geometry.map(rdk_core::geometry::data_id)).hash(h);
    });
    let mut changed = None;
    rays_shell::kit::cached(ui, "spreadsheet-view", revision, &mut |ui| {
        changed = build(ui, bounds, owner, geometry, Rc::make_mut(state));
    });
    changed
}

fn build(ui: &mut Ui, bounds: (i64, i64, i64, i64), owner: i64, geometry: Option<&gv::T>, state: &mut pxui::TableState) -> Option<i64> {
    let (x, y, w, h) = bounds;
    let owner = owner.clamp(0, 3);
    let (changed, _) = rays_shell::kit::segments(ui, "sheet-owners", (x + w) as f32 - 8.0, y as f32 + 2.0, &LABELS, owner);
    match geometry {
        None => {
            let b = ui.node(
                "sheet-empty",
                pxui::Node::DEFAULT.at(x as f32, y as f32 + 24.0).w(Size::Px(w as f32)).h(Size::Px(24.0)),
            );
            ui.canvas(&b, |paint, Rect { x, y, w: _, h: _ }| { paint.text_chars(pxui::Point::new(x + 8.0, y + 5.0), pxui::Label::BODY, "Select a cooked geometry node."); });
        }
        Some(geometry) => {
            let values = columns(owner, geometry);
            let headers: Vec<_> = values.iter().map(|(name, _)| pxui::Column::new(name)).collect();
            let table = ui.node("sheet-body", pxui::Node::DEFAULT.at(x as f32, y as f32 + 24.0)
                .w_px(w as f32).h_px((h - 24).max(0) as f32));
            let rows = count(owner, geometry);
            let mut body = None;
            ui.within(&table, &mut |ui| {
                let result = ui.table("sheet-values", &headers, rows as usize, &|row| row as i64, state,
                    &pxui::Table::DEFAULT, &mut |ui, row, column, _| {
                        let text = (values[column].1)(row as i64);
                        ui.label(&text, pxui::Label::BODY);
                    });
                body = Some(result.rows.handle);
            });
            if let Some(body) = body
                && let Some((_, (px, py))) = rays_shell::kit::context_menu(ui, &body, &[("Copy value", true)])
            {
                let bounds = ui.rect(&body);
                let row = ((py - bounds.y + ui.scroll(&body).painted) / ui.row_height()).floor() as i64;
                if (0..rows).contains(&row) && px >= bounds.x {
                    let mut stop = bounds.x;
                    let column = state.widths.iter().position(|(_, width)| {
                        stop += *width;
                        px < stop
                    });
                    if let Some(column) = column { let _ = rays::clipboard::set_text(&(values[column].1)(row)); }
                }
            }
        }
    }
    changed
}
