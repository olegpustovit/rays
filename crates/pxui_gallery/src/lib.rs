//! The PXUI kit gallery: every kit widget, old and new, on four pages, each page a cached view.
//! The page functions are shared by the window app (`main.rs`) and the headless goldens
//! (`tests/gpu_gallery.rs`); the host loop and the GPU half are in `main.rs` and `gpu.rs`.
use pxui::a11y::Role;
use pxui::draw::Fit;
use pxui::ui::*;
use std::fmt::Write;
use std::rc::Rc;

pub mod gpu;

/// The page area every page is laid out for (the goldens render it at this size).
pub const PAGE_W: i64 = 640;
pub const PAGE_H: i64 = 464;
/// The window: the menu bar, the chrome (tabs, switches), the page and the readout.
pub const WINDOW_W: i64 = PAGE_W;
pub const CHROME_H: f32 = 48.0;
pub const WINDOW_H: i64 = 24 + CHROME_H as i64 + PAGE_H + 24;
pub const PAGES: [&str; 4] = ["Kit", "Controls", "Collections", "Overlays"];
pub const MENUS: [(&str, &[&str]); 2] = [("File", &["Reload theme", "Toast", "Quit"]), ("View", &PAGES)];
const NAMES: [&str; 12] = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu"];
pub const ROWS: usize = 100_000;

/// What the gallery holds between frames. `page_rev` and `chrome_rev` are the views' revisions: an
/// app change that input did not cause bumps them.
pub struct Gallery {
    pub page: usize,
    pub dusk: bool,
    pub compact: bool,
    pub rtl: bool,
    pub reduced: bool,
    /// the switches changed what the host applies (theme, direction) since it last looked
    pub host_dirty: bool,
    pub chrome_rev: u64,
    pub page_rev: u64,
    pub quit: bool,
    pub reload: bool,
    // kit page
    pub animate: bool,
    pub wire: bool,
    pub caption: String,
    pub snap: bool,
    pub scale: String,
    pub notes: String,
    pub hints: Vec<(String, String)>,
    // controls page
    pub check: bool,
    pub radio: usize,
    pub slider: f32,
    pub tab: usize,
    pub segment: usize,
    pub choice: usize,
    pub search: String,
    pub adds: u32,
    pub image: Option<Image>,
    // collections page
    pub list: Vec<bool>,
    pub nodes: Vec<TreeNode>,
    pub open: Vec<bool>,
    pub tree_sel: Option<usize>,
    pub visible: Vec<u32>,
    pub widths: [f32; 3],
    pub table_sel: Option<usize>,
    pub sort: (usize, bool),
    pub split: f32,
    // overlays page
    pub dialog: bool,
    pub menu_at: Option<(f32, f32)>,
    pub picker: bool,
    pub said: &'static str,
    /// toasts and when each goes (window time)
    pub toasts: Vec<(&'static str, f64)>,
    pub now: f64,
    /// the busy widgets of the last build of the page (for `busy_step`) and the readout
    pub busy: Vec<Handle>,
    pub readout: Option<Handle>,
}

impl Gallery {
    pub fn new() -> Gallery {
        Gallery {
            page: 0, dusk: false, compact: false, rtl: false, reduced: false, host_dirty: false, chrome_rev: 0, page_rev: 0, quit: false, reload: false,
            animate: true, wire: false, caption: "Functional UI".to_owned(), snap: true, scale: "1.5".to_owned(), notes: "(circle 4)\n(grid 8 8)".to_owned(),
            hints: vec![("enter".to_owned(), "apply".to_owned()), ("esc".to_owned(), "close".to_owned())],
            check: true, radio: 1, slider: 0.4, tab: 0, segment: 1, choice: 2, search: String::new(), adds: 0, image: None,
            list: vec![false; ROWS], nodes: (0..ROWS).map(|i| TreeNode { depth: [0, 1, 2, 2, 1, 2][i % 6], children: matches!(i % 6, 0 | 1 | 4) }).collect(),
            open: (0..ROWS).map(|i| i < 6).collect(), tree_sel: Some(2), visible: Vec::with_capacity(ROWS), widths: [96.0, 72.0, 72.0], table_sel: Some(1), sort: (0, true), split: 0.34,
            dialog: false, menu_at: None, picker: false, said: "", toasts: Vec::with_capacity(8), now: 0.0, busy: Vec::with_capacity(4), readout: None,
        }
    }

    /// The theme the switches ask for, over the Rays theme or `dusk`.
    pub fn theme(&self, dusk: &Theme) -> Theme {
        let mut t = if self.dusk { *dusk } else { Theme::default() };
        t.tokens.density = if self.compact { 0.84 } else { 1.0 };
        t.tokens.reduced_motion = self.reduced;
        t
    }

    /// Add a toast that goes `secs` from now.
    pub fn toast(&mut self, text: &'static str, secs: f64) {
        if self.toasts.len() == self.toasts.capacity() {
            self.toasts.remove(0);
        }
        self.toasts.push((text, self.now + secs));
    }

    /// Drop the toasts whose time is up; the next one's deadline.
    pub fn expire(&mut self, now: f64) -> (bool, Option<f64>) {
        let before = self.toasts.len();
        self.toasts.retain(|(_, at)| *at > now);
        (self.toasts.len() != before, self.toasts.iter().map(|(_, at)| *at).reduce(f64::min))
    }
}

impl Default for Gallery {
    fn default() -> Gallery {
        Gallery::new()
    }
}

fn names(i: usize, put: &mut dyn FnMut(&str)) {
    put(NAMES[i % NAMES.len()]);
}

fn cells(i: usize, c: usize, put: &mut dyn FnMut(&str)) {
    put(NAMES[(i * 7 + c * 3) % NAMES.len()]);
}

/// A column `w` wide at `at` in its parent (a page's free-standing panel spot).
fn column(ui: &mut Ui, label: &str, at: (f32, f32), w: f32, h: Size, f: &mut dyn FnMut(&mut Ui)) {
    let n = ui.node(label, Node { w: Size::Px(w), h, at: Some(at), ..Node::DEFAULT });
    ui.within(&n, f);
}

/// The kit page: the original kit panel (exactly the `kit_panel` golden's) and, to its right, the
/// rest of the old kit: inspector section, rows and button, a value field, a text area, a footer
/// and the read-only table.
pub fn kit_page(ui: &mut Ui, s: &mut Gallery) {
    ui.panel(PanelOpts::default(), "panel", &mut |ui| {
        ui.label("PXUI");
        s.animate = ui.toggle("Animate", s.animate);
        s.wire = ui.toggle("Wire", s.wire);
        if let Some(v) = ui.text_field("Caption", &s.caption) {
            s.caption = v;
        }
        if ui.button("Quit") {
            s.quit = true;
        }
    });
    ui.panel(PanelOpts::default().at(332.0, 12.0).width(296.0).window(true), "inspector", &mut |ui| {
        ui.inspector_section("section", true, None, "Inspector", &mut |ui| {
            s.snap = ui.inspector_toggle("snap", "Snap", s.snap);
            ui.inspector_readout(None, "count", "Points", "1,024");
            let (row, cx, cy, cw) = ui.inspector_row(None, None, "scale", "Scale");
            ui.within(&row, &mut |ui| {
                let (v, _) = ui.value_field((cx, cy), cw, 20.0, &ValueFieldOpts::default(), &|t| t.parse::<f64>().is_ok(), "scale-value", &s.scale);
                if v != s.scale {
                    s.scale = v;
                }
            });
            if ui.inspector_button("reset", "Reset") {
                s.scale = "1.5".to_owned();
            }
        });
        let notes = ui.node("notes", Node { h: Size::Px(64.0), ..Node::DEFAULT });
        ui.within(&notes, &mut |ui| {
            let (text, _) = ui.text_area_submit((12.0, 4.0), 270.0, 56.0, &TextAreaOpts::default(), "notes-area", &s.notes);
            if text != s.notes {
                s.notes = text;
            }
        });
        ui.footer(Some("2 keys"), &s.hints);
    });
    column(ui, "old-table", (332.0, 250.0), 296.0, Size::Px(200.0), &mut |ui| {
        let headers = [String::from("row"), String::from("name"), String::from("x")];
        ui.table((0.0, 0.0), 296.0, 200.0, &headers, 1000, Rc::new(|r, c| if c == 0 { r.to_string() } else if c == 1 { NAMES[r as usize % NAMES.len()].to_owned() } else { format!("{:.2}", r as f64 * 0.25) }), "old-table");
    });
}

/// The controls page: every value widget, two columns.
pub fn controls_page(ui: &mut Ui, s: &mut Gallery) {
    s.busy.clear();
    ui.panel(PanelOpts::default().width(296.0), "values", &mut |ui| {
        ui.label("Values");
        s.check = ui.checkbox("Snap to grid", s.check);
        s.radio = ui.radio("Quality", &["Draft", "Normal", "Final"], s.radio);
        s.slider = ui.slider("Opacity", s.slider, 0.0..=1.0);
        ui.progress("Export", Some(s.slider));
        let p = ui.progress("Indexing", None);
        s.busy.push(p);
        let row = ui.node("spin-row", Node { h: Size::Px(24.0), axis: Axis::Row, align: Align::Center, padding: Edges { l: 12.0, ..Edges::ZERO }, gap: 8.0, ..Node::DEFAULT });
        ui.within(&row, &mut |ui| {
            let sp = ui.spinner("Loading");
            s.busy.push(sp);
            let b = ui.badge("count", "NEW");
            ui.tip(&b, "A badge: a small label in the accent");
        });
        s.segment = ui.segmented("Align", &["Left", "Centre", "Right"], s.segment);
        s.choice = ui.dropdown("Blend", &["Normal", "Multiply", "Screen", "Overlay", "Add"], s.choice);
    });
    ui.panel(PanelOpts::default().at(332.0, 12.0).width(296.0), "more", &mut |ui| {
        s.tab = ui.tabs("Sections", &["Layer", "Mask", "Effects"], s.tab);
        if let Some(q) = ui.search_field("Find", "/", &s.search, "Search layers") {
            s.search = q;
        }
        let row = ui.node("icons", Node { h: Size::Px(28.0), axis: Axis::Row, align: Align::Center, padding: Edges { l: 12.0, ..Edges::ZERO }, gap: 4.0, ..Node::DEFAULT });
        ui.within(&row, &mut |ui| {
            for (label, glyph) in [("Add", "+"), ("Remove", "-"), ("Close", "\u{d7}")] {
                if ui.icon_button(label, glyph) {
                    s.adds += 1;
                }
            }
        });
        ui.scroll_view("Scroll both ways", Size::Grow, Size::Px(96.0), &mut |ui| {
            for (i, name) in NAMES.iter().enumerate() {
                let n = ui.node(name, Node { w: Size::Px(420.0), h: Size::Px(22.0), inset: Edges { l: 8.0 + 8.0 * i as f32, ..Edges::ZERO }, look: Look { text: ui.theme().value.idle.text, ..Look::NONE }, ..Node::DEFAULT });
                ui.text(&n, name);
            }
        });
        if let Some(image) = s.image.clone() {
            let row = ui.node("images", Node { h: Size::Px(56.0), axis: Axis::Row, align: Align::Center, padding: Edges { l: 12.0, ..Edges::ZERO }, gap: 8.0, ..Node::DEFAULT });
            ui.within(&row, &mut |ui| {
                for (label, fit) in [("Contain", Fit::Contain), ("Cover", Fit::Cover), ("Fill", Fit::Fill), ("None", Fit::None)] {
                    ui.image(label, &image, fit, Size::Px(40.0), Size::Px(40.0));
                }
            });
        }
    });
}

/// The collections page: a list beside a split of a tree over a table, all virtual over 100,000.
pub fn collections_page(ui: &mut Ui, s: &mut Gallery) {
    let pad = ui.node("collections", Node { w: Size::Grow, h: Size::Grow, padding: Edges::all(12.0), ..Node::DEFAULT });
    ui.within(&pad, &mut |ui| {
        let ratio = s.split;
        if let Some(r) = ui.split("split", Axis::Row, ratio, 120.0, 200.0, &mut |ui| {
            ui.list_view("Files", ROWS, true, &mut s.list, &names);
        }, &mut |ui| {
            let half = ui.node("right", Node { w: Size::Grow, h: Size::Grow, padding: Edges { l: 8.0, ..Edges::ZERO }, gap: 8.0, ..Node::DEFAULT });
            ui.within(&half, &mut |ui| {
                let top = ui.node("tree-box", Node { h: Size::Grow, ..Node::DEFAULT });
                ui.within(&top, &mut |ui| {
                    ui.tree("Outline", &s.nodes, &mut s.open, &mut s.tree_sel, &mut s.visible, &names);
                });
                let bottom = ui.node("table-box", Node { h: Size::Grow, ..Node::DEFAULT });
                ui.within(&bottom, &mut |ui| {
                    if let Some(c) = ui.table_view("Points", &["Name", "Group", "Tag"], &mut s.widths, ROWS, Some(s.sort), &mut s.table_sel, &cells) {
                        s.sort = (c, if s.sort.0 == c { !s.sort.1 } else { true });
                    }
                });
            });
        }) {
            s.split = r;
        }
    });
}

/// The overlays page: buttons that open the dialog, a context menu, the picker in a modal and a
/// toast; a tooltip on a resting pointer.
pub fn overlays_page(ui: &mut Ui, s: &mut Gallery) {
    ui.panel(PanelOpts::default().width(296.0), "overlays", &mut |ui| {
        ui.label("Overlays");
        if ui.button("Open dialog") {
            s.dialog = true;
        }
        if ui.button("Context menu") {
            s.menu_at = Some((140.0, 120.0));
        }
        if ui.button("Pick a preset") {
            s.picker = true;
        }
        if ui.button("Show a toast") {
            s.toast("Saved to disk", 3.0);
        }
        let row = ui.node("tip-row", Node { h: Size::Px(28.0), axis: Axis::Row, align: Align::Center, padding: Edges { l: 12.0, ..Edges::ZERO }, ..Node::DEFAULT });
        ui.within(&row, &mut |ui| {
            ui.icon_button("Help", "?");
        });
        if let Some(help) = ui.box_of_key(key_of(row.box_key, "Help")) {
            ui.tip(&help, "Rest the pointer on a control to see its tip");
        }
        if !s.said.is_empty() {
            ui.inspector_message("said", s.said);
        }
    });
    if s.dialog {
        match ui.dialog("Delete layer", &["Cancel", "Delete"], &mut |ui| ui.inspector_message("body", "This cannot be undone.")) {
            DialogOut::Open => {}
            DialogOut::Closed | DialogOut::Button(0) => (s.dialog, s.said) = (false, "Kept the layer"),
            DialogOut::Button(_) => (s.dialog, s.said) = (false, "Deleted the layer"),
        }
    }
    if let Some(at) = s.menu_at {
        let items = [("Cut".to_owned(), true), ("Copy".to_owned(), true), (String::new(), true), ("Delete".to_owned(), true)];
        match ui.context_menu(at, &MenuOpts::default(), "context", &items) {
            MenuResult::Open => {}
            MenuResult::Pick(_) => (s.menu_at, s.said) = (None, "Picked from the menu"),
            MenuResult::Dismiss => s.menu_at = None,
        }
    }
    if s.picker {
        let mut close = false;
        ui.modal(Some(240.0), "presets", &mut |ui| {
            ui.label("Presets");
            let rows = |_: &str| NAMES[..5].iter().map(|n| (n.to_string(), "09-24".to_owned())).collect();
            let (_, pick) = ui.picker(&PickerOpts { limit: 4, ..PickerOpts::default() }, "Search presets", "", &rows);
            close = !matches!(pick, Pick::None);
        });
        if close {
            s.picker = false;
        }
    }
}

/// Page `i` (wraps).
pub fn page(ui: &mut Ui, s: &mut Gallery, i: usize) {
    match i % PAGES.len() {
        0 => kit_page(ui, s),
        1 => controls_page(ui, s),
        2 => collections_page(ui, s),
        _ => overlays_page(ui, s),
    }
}

/// The whole window: the menu bar, the chrome (page tabs and the theme, density, direction and
/// motion switches), the page in its view, the readout (patched by the host, never rebuilt) and
/// the toasts.
pub fn app(ui: &mut Ui, s: &mut Gallery) {
    // one Window node holds the menu bar and the page: the screen reader's application -> window
    let (w, h) = ui.view_size();
    let window = ui.node("window", Node { w: Size::Px(w), h: Size::Px(h), role: Role::Window, ..Node::DEFAULT });
    ui.a11y_name(&window, "PXUI gallery");
    ui.within(&window, &mut |ui| {
        match ui.menu_bar("menus", &MENUS) {
            Some((0, 0)) => s.reload = true,
            Some((0, 1)) => s.toast("Hello from the menu", 3.0),
            Some((0, _)) => s.quit = true,
            Some((_, i)) if i != s.page => (s.page, s.page_rev, s.chrome_rev) = (i, s.page_rev + 1, s.chrome_rev + 1),
            _ => {}
        }
        let body = ui.node("body", Node { w: Size::Px(w), h: Size::Px(h - 24.0), at: Some((0.0, 24.0)), ..Node::DEFAULT });
        ui.within(&body, &mut |ui| {
            ui.view("chrome", s.chrome_rev, Size::Grow, Size::Px(CHROME_H), &mut |ui| {
                let page = ui.tabs("Pages", &PAGES, s.page);
                if page != s.page {
                    (s.page, s.page_rev, s.chrome_rev) = (page, s.page_rev + 1, s.chrome_rev + 1); // the chrome shows the tab: rebuild it too
                }
                let row = ui.node("switches", Node { h: Size::Px(24.0), axis: Axis::Row, ..Node::DEFAULT });
                ui.within(&row, &mut |ui| {
                    let theme = ui.segmented("Theme", &["Rays", "Dusk"], s.dusk as usize) == 1;
                    let compact = ui.checkbox("Compact", s.compact);
                    let rtl = ui.checkbox("RTL", s.rtl);
                    let reduced = ui.checkbox("Still", s.reduced);
                    if (theme, compact, rtl, reduced) != (s.dusk, s.compact, s.rtl, s.reduced) {
                        (s.dusk, s.compact, s.rtl, s.reduced, s.host_dirty) = (theme, compact, rtl, reduced, true);
                    }
                });
            });
            let rev = s.page_rev;
            let index = s.page;
            ui.view("page", rev, Size::Grow, Size::Grow, &mut |ui| page(ui, s, index));
            ui.view("readout", 0, Size::Grow, Size::Px(24.0), &mut |ui| {
                let look = ui.theme().footer.idle;
                let hint = ui.theme().hint.idle;
                let n = ui.node("readout", Node { h: Size::Px(24.0), look: Look { text: hint.text, font: hint.font, ..look }, inset: Edges { l: 12.0, t: 1.0, ..Edges::ZERO }, role: Role::StatusBar, ..Node::DEFAULT });
                ui.text(&n, "-");
                s.readout = Some(n);
            });
        });
    });
    let mut texts = [""; 8];
    for (slot, (text, _)) in texts.iter_mut().zip(&s.toasts) {
        *slot = text;
    }
    if let Some(i) = ui.toasts("toasts", &texts[..s.toasts.len().min(8)]) {
        s.toasts.remove(i);
    }
}

/// What one frame cost, for the readout.
#[derive(Clone, Copy, Debug, Default)]
pub struct Cost {
    pub us: f64,
    pub nodes: usize,
    pub laid_out: usize,
    pub bytes: u64,
    pub allocs: Option<u64>,
    pub frames: u64,
}

/// The readout's text, written into `out` (kept, so a warm frame does not allocate).
pub fn readout(out: &mut String, c: &Cost) {
    out.clear();
    let _ = write!(out, "frame {:.0} us   nodes built {}   laid out {}   instance bytes {}   frames {}", c.us, c.nodes, c.laid_out, c.bytes, c.frames);
    if let Some(a) = c.allocs {
        let _ = write!(out, "   allocs {a}");
    }
}
