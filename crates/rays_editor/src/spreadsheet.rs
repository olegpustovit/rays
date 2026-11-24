//! The spreadsheet panel's table over a cooked geometry's point, vertex, primitive or detail
//! attributes.
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
    use std::hash::Hash;
    let revision = rays_shell::kit::revision(&|h| {
        (bounds, owner, geometry.map(rdk_core::geometry::data_id)).hash(h);
    });
    let mut changed = None;
    rays_shell::kit::cached(ui, "spreadsheet-view", revision, &mut |ui| {
        changed = build(ui, bounds, owner, geometry);
    });
    changed
}

fn build(ui: &mut Ui, bounds: (i64, i64, i64, i64), owner: i64, geometry: Option<&gv::T>) -> Option<i64> {
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
            let columns = columns(owner, geometry);
            let headers: Vec<String> = columns.iter().map(|(name, _)| name.clone()).collect();
            let height = (h - 24).max(0) as f32;
            let table = ui.box_("sheet-table-context", BoxOpts::default()
                .at((x as f32, y as f32 + 24.0)).w(Size::Px(w as f32)).h(Size::Px(height)));
            let n = count(owner, geometry);
            ui.within(&table, &mut |ui| {
                ui.table((0.0, 0.0), w as f32, height, &headers, n, |row, column| (columns[column as usize].1)(row), "sheet-values");
            });
            use pxui::ui_core::key_of;
            let body = key_of(key_of(table.box_key, "sheet-values"), "sheet-values-body");
            if let Some(body) = ui.box_of_key(body)
                && let Some((_, (px, py))) = rays_shell::kit::context_menu(ui, &body, &[("Copy value", true)])
            {
                let bounds = ui.rect(&body);
                let row = ((py - bounds.y + ui.scroll_offset(&body)) / ui.row_height() as f32).floor() as i64;
                if (0..n).contains(&row) && px >= bounds.x {
                    let mut stop = bounds.x;
                    // ponytail: Ui::table's column widths; use an ABI cell hit once PXUI exposes one.
                    let column = headers.iter().position(|head| {
                        stop += 72.0f32.max(ui.text_width(None, head) + 24.0);
                        px < stop
                    });
                    if let Some(column) = column { let _ = rays::clipboard::set_text(&(columns[column].1)(row)); }
                }
            }
        }
    }
    changed
}
