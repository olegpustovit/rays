//! The spreadsheet panel's table over a cooked geometry's point, vertex, primitive or detail
//! attributes.
use std::rc::Rc;

use pxui::ui::{BoxOpts, Size, Ui};
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
    let (changed, _) = rays_shell::kit::segments(ui, "sheet-owners", (x + w) as f64 - 8.0, y as f64 + 2.0, &labels(), owner);
    match geometry {
        None => {
            let b = ui.box_(
                "sheet-empty",
                BoxOpts::default().at((x as f64, y as f64 + 24.0)).w(Size::Px(w as f64)).h(Size::Px(24.0)),
            );
            ui.draw(&b, |paint, (x, y, _, _)| paint.text((x + 8.0, y + 5.0), "Select a cooked geometry node."));
        }
        Some(geometry) => {
            let columns = Rc::new(columns(owner, geometry));
            let headers: Vec<String> = columns.iter().map(|(name, _)| name.clone()).collect();
            let cells = columns.clone();
            ui.table(
                (x as f64, y as f64 + 24.0),
                w as f64,
                (h - 24).max(0) as f64,
                &headers,
                count(owner, geometry),
                Rc::new(move |row, column| (cells[column as usize].1)(row)),
                "sheet-values",
            );
        }
    }
    changed
}
