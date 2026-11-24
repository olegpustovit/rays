//! The spreadsheet panel's table over a cooked geometry's point, vertex, primitive or detail
//! attributes.

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
pub fn view(ui: &mut Ui, bounds: (i64, i64, i64, i64), owner: i64, geometry: Option<&gv::T>, state: &mut pxui::TableState) -> Option<i64> {
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
            ui.within(&table, &mut |ui| {
                ui.table("sheet-values", &headers, count(owner, geometry) as usize, &|row| row as i64, state,
                    &pxui::Table::DEFAULT, &mut |ui, row, column, _| {
                        let text = (values[column].1)(row as i64);
                        ui.label(&text, pxui::Label::BODY);
                    });
            });
        }
    }
    changed
}
