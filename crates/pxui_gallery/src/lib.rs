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
pub const PAGES: [&str; 5] = ["Kit", "Controls", "Collections", "Overlays", "Layout"];
pub const MENUS: [(&str, &[&str]); 2] = [("File", &["Reload theme", "Toast", "Quit"]), ("View", &PAGES)];
const NAMES: [&str; 12] = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu"];
pub const ROWS: usize = 100_000;
/// How long a toast shows.
pub const TOAST_SECONDS: f64 = 3.0;

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
    // layout page: the keyframed chip and whether its button asked for a run
    pub bounce: bool,
    /// the toasts shown (each goes `TOAST_SECONDS` after it showed, or when clicked)
    pub toasts: Vec<&'static str>,
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
            dialog: false, menu_at: None, picker: false, said: "", bounce: false, toasts: Vec::with_capacity(8), busy: Vec::with_capacity(4), readout: None,
        }
    }

    /// The theme the switches ask for, over the Rays theme or `dusk`.
    pub fn theme(&self, dusk: &Theme) -> Theme {
        let mut t = if self.dusk { *dusk } else { Theme::default() };
        t.tokens.density = if self.compact { 0.84 } else { 1.0 };
        t.tokens.reduced_motion = self.reduced;
        t
    }

    /// Add a toast (the UI times it out).
    pub fn toast(&mut self, text: &'static str) {
        if self.toasts.len() == self.toasts.capacity() {
            self.toasts.remove(0);
        }
        self.toasts.push(text);
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
            s.toast("Saved to disk");
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

/// A captioned demo box of the layout page, `w` x `h` at `at`: its caption, then `f`'s nodes in a
/// box with a hairline (`body` its layout).
fn demo(ui: &mut Ui, title: &str, at: (f32, f32), (w, h): (f32, f32), body: Node, f: &mut dyn FnMut(&mut Ui)) {
    let t = *ui.theme();
    let outer = ui.node(title, Node { w: Size::Px(w), h: Size::Px(h), at: Some(at), gap: 4.0, ..Node::DEFAULT });
    ui.within(&outer, &mut |ui| {
        let cap = ui.node("caption", Node { w: Size::Grow, h: Size::Px(14.0), look: text_only(&t.hint.idle), ..Node::DEFAULT });
        ui.text(&cap, title);
        let frame = Look { border: t.field.idle.border, border_w: 1.0, ..Look::NONE };
        let b = ui.node("body", Node { w: Size::Grow, h: Size::Grow, look: frame, padding: Edges::all(4.0), ..body });
        ui.within(&b, f);
    });
}

/// A filled chip: `k` picks the accent, the control or the track colour.
fn chip(ui: &mut Ui, label: &str, k: usize, n: Node) -> Handle {
    let t = ui.tokens();
    let fill = [t.accent, t.control, t.track][k % 3];
    let border = if k % 3 == 2 { t.foreground } else { fill };
    ui.node(label, Node { look: Look { fill, border, border_w: 1.0, ..Look::NONE }, ..n })
}

fn px(w: f32, h: f32) -> Node {
    Node { w: Size::Px(w), h: Size::Px(h), ..Node::DEFAULT }
}

/// The layout page: flex weights and shrink, min/max/aspect, every justify and align mode with
/// baselines, wrap, a grid with spans, a z-ordered stack, a both-axis scroll box with a sticky header,
/// a virtual grid, an anchored popup and a keyframed chip; right to left with the RTL switch.
pub fn layout_page(ui: &mut Ui, s: &mut Gallery) {
    const COLS: &[Track] = &[Track::Fr(1.0), Track::Fit, Track::Px(24.0)];
    let row = Node { axis: Axis::Row, ..Node::DEFAULT };
    let ink = text_only(&ui.theme().value.idle);
    let (col_w, gap) = (196.0, 14.0);
    let x = |c: usize| 12.0 + c as f32 * (col_w + gap);
    demo(ui, "Weights and shrink", (x(0), 12.0), (col_w, 104.0), Node { gap: 6.0, ..Node::DEFAULT }, &mut |ui| {
        for (r, weights) in [[1.0, 2.0, 3.0], [1.0, 1.0, 1.0]].iter().enumerate() {
            let line = ui.node(&format!("grow{r}"), Node { w: Size::Grow, h: Size::Px(16.0), gap: 4.0, ..row });
            ui.within(&line, &mut |ui| {
                for (i, g) in weights.iter().enumerate() {
                    chip(ui, &format!("g{i}"), i, Node { w: Size::Grow, h: Size::Grow, grow: *g, ..Node::DEFAULT });
                }
            });
        }
        // two 100-point chips in 120 points, shrinking 1 : 3
        let line = ui.node("shrink", Node { w: Size::Px(120.0), h: Size::Px(16.0), ..row });
        ui.within(&line, &mut |ui| {
            for (i, sh) in [1.0, 3.0].iter().enumerate() {
                chip(ui, &format!("s{i}"), i + 1, Node { shrink: *sh, ..px(100.0, 16.0) });
            }
        });
        let line = ui.node("minmax", Node { w: Size::Grow, h: Size::Px(16.0), gap: 4.0, ..row });
        ui.within(&line, &mut |ui| {
            chip(ui, "max", 0, Node { w: Size::Grow, h: Size::Grow, max_w: 40.0, ..Node::DEFAULT });
            chip(ui, "free", 1, Node { w: Size::Grow, h: Size::Grow, ..Node::DEFAULT });
            chip(ui, "min", 2, Node { min_w: 50.0, ..px(10.0, 16.0) });
            chip(ui, "aspect", 0, Node { w: Size::Fit, h: Size::Px(16.0), aspect: 2.0, ..Node::DEFAULT });
        });
    });
    const JUSTIFY: [Justify; 6] = [Justify::Start, Justify::Center, Justify::End, Justify::SpaceBetween, Justify::SpaceAround, Justify::SpaceEvenly];
    demo(ui, "Justify, gap 4", (x(1), 12.0), (col_w, 104.0), Node { gap: 4.0, ..Node::DEFAULT }, &mut |ui| {
        for (r, j) in JUSTIFY.iter().enumerate() {
            let line = ui.node(&format!("j{r}"), Node { w: Size::Grow, h: Size::Px(10.0), justify: *j, gap: 4.0, ..row });
            ui.within(&line, &mut |ui| {
                for i in 0..3 {
                    chip(ui, &format!("c{i}"), i, px(18.0, 10.0));
                }
            });
        }
    });
    const ALIGN: [Align; 5] = [Align::Start, Align::Center, Align::End, Align::Stretch, Align::Baseline];
    demo(ui, "Align, baseline last", (x(2), 12.0), (col_w, 104.0), Node { gap: 6.0, ..row }, &mut |ui| {
        for (c, a) in ALIGN.iter().enumerate() {
            let col = ui.node(&format!("a{c}"), Node { w: Size::Grow, h: Size::Grow, align: *a, gap: 2.0, ..row });
            ui.within(&col, &mut |ui| {
                for (i, size) in [9i64, 13, 20].iter().enumerate() {
                    let h = if *a == Align::Stretch { Size::Fit } else { Size::Px(*size as f32 + 4.0) };
                    let n = chip(ui, &format!("t{i}"), i + 1, Node { w: Size::Px(10.0), h, look: Look::NONE, ..Node::DEFAULT });
                    let look = Look { font: Font { size: *size, ..Font::BODY }, ..ink };
                    let fill = ui.tokens().track;
                    ui.node_mut(&n).look = Look { fill, ..look };
                    ui.text(&n, "x");
                }
            });
        }
    });
    demo(ui, "Wrap at the given width", (x(0), 128.0), (col_w, 104.0), Node::DEFAULT, &mut |ui| {
        // a wrapping Fit row whose width the column gives it
        let wrap = ui.node("wrap", Node { w: Size::Grow, h: Size::Fit, wrap: true, gap: 4.0, row_gap: 4.0, ..row });
        ui.within(&wrap, &mut |ui| {
            for i in 0..11 {
                chip(ui, &format!("w{i}"), i, px(14.0 + (i % 4) as f32 * 8.0, 14.0));
            }
        });
        chip(ui, "below", 1, Node { w: Size::Grow, h: Size::Px(8.0), margin: Edges { t: 4.0, ..Edges::ZERO }, ..Node::DEFAULT });
    });
    demo(ui, "Grid: fr, fit, 24; spans", (x(1), 128.0), (col_w, 104.0), Node { grid: Some(Grid { cols: COLS, rows: &[] }), gap: 4.0, row_gap: 4.0, ..Node::DEFAULT }, &mut |ui| {
        chip(ui, "a", 0, Node { w: Size::Grow, h: Size::Px(14.0), ..Node::DEFAULT });
        chip(ui, "b", 1, px(30.0, 14.0));
        chip(ui, "c", 2, Node { w: Size::Grow, h: Size::Px(14.0), ..Node::DEFAULT });
        // two rows of 14 under a 40-point row span: the content rows grow to hold it
        chip(ui, "row-span", 1, Node { cell: Cell::at(0, 1).span(1, 2), w: Size::Grow, h: Size::Px(40.0), ..Node::DEFAULT });
        // 70 points over the Fit and the 24 columns: the Fit one grows to hold it
        chip(ui, "col-span", 0, Node { cell: Cell::at(1, 1).span(2, 1), ..px(70.0, 14.0) });
        chip(ui, "d", 2, Node { cell: Cell::at(1, 2), w: Size::Grow, h: Size::Px(14.0), ..Node::DEFAULT });
    });
    demo(ui, "Stack in z order", (x(2), 128.0), (col_w, 104.0), Node { axis: Axis::Stack, justify: Justify::Center, align: Align::Center, ..Node::DEFAULT }, &mut |ui| {
        for (i, (w, z, l)) in [(120.0, 0i16, 0.0), (70.0, 2, 40.0), (50.0, 1, 110.0)].iter().enumerate() {
            chip(ui, &format!("z{i}"), i, Node { z: *z, margin: Edges { l: *l, ..Edges::ZERO }, ..px(*w, *w / 2.0) });
        }
    });
    demo(ui, "Scroll both ways, sticky", (x(0), 244.0), (col_w, 104.0), Node { flags: SCROLL | SCROLL_X | CLIP, gap: 2.0, ..Node::DEFAULT }, &mut |ui| {
        let head = chip(ui, "head", 0, Node { flags: STICKY, w: Size::Px(320.0), h: Size::Px(14.0), ..Node::DEFAULT });
        ui.text(&head, "sticky header");
        ui.node_mut(&head).look.text = ui.tokens().input;
        for i in 0..8 {
            chip(ui, &format!("r{i}"), 1 + i % 2, px(320.0 - 20.0 * i as f32, 14.0));
        }
    });
    demo(ui, "grid_list of 100,000", (x(1), 244.0), (col_w, 104.0), Node::DEFAULT, &mut |ui| {
        let t = ui.tokens();
        ui.grid_list("cells", 100_000, 22.0, 14.0, &mut |ui, i| {
            let look = Look { fill: if i % 7 == 0 { t.accent } else { t.control }, border: t.panel, border_w: 1.0, ..Look::NONE };
            ui.node("cell", Node { w: Size::Grow, h: Size::Grow, look, ..Node::DEFAULT });
        });
    });
    let mut anchor = None;
    demo(ui, "Anchored popup, keyframes", (x(2), 244.0), (col_w, 104.0), Node { gap: 6.0, ..Node::DEFAULT }, &mut |ui| {
        if ui.button("Bounce") {
            s.bounce = true;
        }
        let a = chip(ui, "anchor", 2, px(60.0, 16.0));
        anchor = Some(ui.rect(&a));
        let c = chip(ui, "bouncer", 0, px(16.0, 16.0));
        if std::mem::take(&mut s.bounce) {
            let ease = Curve::Bezier { duration: 0.0, points: [0.25, 0.1, 0.25, 1.0] };
            let keys = [(0.2, Target::Offset(120.0, 0.0), ease), (0.35, Target::Opacity(0.3), ease), (0.6, Target::Offset(0.0, 0.0), Curve::Spring { omega: 18.0 }), (0.7, Target::Opacity(1.0), ease)];
            ui.animate_keys(&c, &keys);
        }
    });
    // the popup sits by the anchor's last laid-out rectangle, outside the page's flow (the first
    // build has none yet: one more frame)
    if anchor.is_some_and(|a| a.w <= 0.0) {
        ui.request_frame();
    }
    if let Some(a) = anchor.filter(|a| a.w > 0.0) {
        let place = Placement { side: Side::Right, align: Align::Center, gap: 4.0, margin: 4.0, flip: true };
        let pop = *ui.theme();
        ui.anchored("popup", a, place, Size::Fit, Size::Fit, &mut |ui| {
            let n = ui.node("popup-body", Node { w: Size::Px(96.0), h: Size::Px(24.0), look: Look { fill: pop.tokens.input, border: pop.field.idle.border, border_w: 1.0, text: pop.tokens.foreground, ..Look::NONE }, inset: Edges { l: 6.0, ..Edges::ZERO }, ..Node::DEFAULT });
            ui.text(&n, "anchored, flips");
        });
    }
    demo(ui, "Column of rows, fit height", (x(0), 360.0), (col_w * 3.0 + gap * 2.0, 92.0), Node { axis: Axis::Row, gap: 8.0, align: Align::Start, ..Node::DEFAULT }, &mut |ui| {
        for (i, words) in [["one", "two", "three"], ["start", "middle", "end"], ["a", "bb", "ccc"]].iter().enumerate() {
            let col = ui.node(&format!("col{i}"), Node { w: Size::Grow, h: Size::Fit, gap: 2.0, padding: Edges { l: 4.0, t: 2.0, r: 4.0, b: 2.0 }, look: Look { border: ui.theme().field.idle.border, border_w: 1.0, ..Look::NONE }, ..Node::DEFAULT });
            ui.within(&col, &mut |ui| {
                for w in words {
                    let n = ui.node(w, Node { w: Size::Grow, h: Size::Px(16.0), look: ink, inset: Edges { l: 2.0, ..Edges::ZERO }, ..Node::DEFAULT });
                    ui.text(&n, w);
                }
            });
        }
    });
}

/// Page `i` (wraps).
pub fn page(ui: &mut Ui, s: &mut Gallery, i: usize) {
    match i % PAGES.len() {
        0 => kit_page(ui, s),
        1 => controls_page(ui, s),
        2 => collections_page(ui, s),
        3 => overlays_page(ui, s),
        _ => layout_page(ui, s),
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
            Some((0, 1)) => s.toast("Hello from the menu"),
            Some((0, _)) => s.quit = true,
            Some((_, i)) if i != s.page => (s.page, s.page_rev, s.chrome_rev) = (i, s.page_rev + 1, s.chrome_rev + 1),
            _ => {}
        }
        let body = ui.node("body", Node { w: Size::Px(w), h: Size::Px(h - 24.0), at: Some((0.0, 24.0)), ..Node::DEFAULT });
        ui.within(&body, &mut |ui| {
            ui.view("chrome", s.chrome_rev, Size::Grow, Size::Px(CHROME_H), &mut |ui| {
                let page = ui.tabs("Pages", &PAGES, s.page);
                if page != s.page {
                    (s.page, s.page_rev) = (page, s.page_rev + 1); // the tabs show the click in this build
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
    if let Some(i) = ui.toasts("toasts", &s.toasts, TOAST_SECONDS) {
        s.toasts.remove(i);
    }
}

/// What one frame cost, for the readout.
#[derive(Clone, Copy, Debug, Default)]
pub struct Cost {
    pub us: f64,
    pub nodes: usize,
    pub views: usize,
    pub laid_out: usize,
    pub bytes: u64,
    pub allocs: Option<u64>,
    pub frames: u64,
}

/// The readout's text, written into `out` (kept, so a warm frame does not allocate).
pub fn readout(out: &mut String, c: &Cost) {
    out.clear();
    let _ = write!(out, "frame {:.0} us   views built {}   nodes built {}   laid out {}   instance bytes {}   frames {}", c.us, c.views, c.nodes, c.laid_out, c.bytes, c.frames);
    if let Some(a) = c.allocs {
        let _ = write!(out, "   allocs {a}");
    }
}
