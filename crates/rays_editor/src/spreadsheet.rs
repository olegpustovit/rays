//! The spreadsheet panel's table over a cooked geometry's point, vertex, primitive or detail
//! attributes.
use std::rc::Rc;

use pxui::ui::{BoxOpts, Rect, Size, Ui};
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
pub fn view(ui: &mut Ui, bounds: (i64, i64, i64, i64), owner: i64, geometry: Option<&gv::T>) -> Option<i64> {
    let (x, y, w, h) = bounds;
    let owner = owner.clamp(0, 3);
    let (changed, _) = rays_shell::kit::segments(ui, "sheet-owners", (x + w) as f32 - 8.0, y as f32 + 2.0, &LABELS, owner);
    match geometry {
        None => {
            let b = ui.box_(
                "sheet-empty",
                BoxOpts::default().at((x as f32, y as f32 + 24.0)).w(Size::Px(w as f32)).h(Size::Px(24.0)),
            );
            ui.draw(&b, |paint, Rect { x, y, w: _, h: _ }| paint.text((x + 8.0, y + 5.0), "Select a cooked geometry node."));
        }
        Some(geometry) => {
            let columns = Rc::new(columns(owner, geometry));
            let headers: Vec<String> = columns.iter().map(|(name, _)| name.clone()).collect();
            let cells = columns.clone();
            ui.table(
                (x as f32, y as f32 + 24.0),
                w as f32,
                (h - 24).max(0) as f32,
                &headers,
                count(owner, geometry),
                Rc::new(move |row, column| (cells[column as usize].1)(row)),
                "sheet-values",
            );
        }
    }
    changed
}
