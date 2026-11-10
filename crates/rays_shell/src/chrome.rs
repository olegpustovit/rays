//! chrome of the retained workspace, painted and hit through
//! PXUI boxes: panel backgrounds, splitters, and header bars with a collapse button and a
//! right-click menu (split, close, retype).
use std::borrow::Cow;
use crate::kit;
use crate::layout::*;
use crate::layout::Size as PSize;
use crate::layout::{Axis, Panels};
use editor_core::panels;
use input::Key;
use pxui::theme;
use pxui::ui::*;
use pxui::ui::Size;
use rays::frame;
use std::cell::RefCell;
use std::rc::Rc;

pub use editor_core::panels::Side;

#[derive(Clone, Debug, PartialEq)]
pub enum Intent {
    /// a splitter is being dragged: a ratio, or the whole points of the fixed side
    Resize { node: Path, size: PSize },
    /// the drag ended
    Settled,
    /// a header's collapse button
    Toggle(Path),
    /// undock, move/resize, or dock a panel
    Window(Path, Option<Bounds>),
    /// Transient bounds; `true` commits the released gesture.
    WindowDrag(Path, Bounds, bool),
    /// a panel drag; `true` on release
    Dragging(Path, bool),
    DockPanel(Path, Path, Side),
    /// the header menu
    SplitPanel(Path, Axis),
    /// split a panel where the pointer is: the first half keeps this share of the extent, the
    /// second is a panel of the kind
    SplitPanelAt(Path, Axis, f64, Panel),
    ClosePanel(Path),
    /// `View ""` means a viewport
    RetypePanel(Path, Panel),
    /// a click on part `i` of a docked header's breadcrumb, not its last
    Crumb(Path, i64),
}

fn floating(ui: &mut Ui, flags: Flags, (x, y, width, height): Bounds, label: std::fmt::Arguments) -> UiBox {
    ui.box_fmt(
        label,
        BoxOpts::default().flags(flags).w(Size::Px(width as f64)).h(Size::Px(height as f64)).at((x as f64, y as f64)),
    )
}

/// A pane's PXUI hit ancestor; children keep screen-space coordinates.
pub fn pane_root(ui: &mut Ui, frame: &frame::Frame, bounds: Bounds, label: &str) -> UiBox {
    let (x, y, width, height) = bounds;
    let rect = (x as f64, y as f64, width as f64, height as f64);
    let mut opts = BoxOpts::default()
        .flags(CLICKABLE)
        .w(Size::Px(frame.input.size.0 as f64))
        .h(Size::Px(frame.input.size.1 as f64))
        .at((0.0, 0.0));
    opts.hit_rect = Some(rect);
    ui.box_(label, opts)
}

/// a panel's path as text, to key its boxes
/// A leaf path as box-label text, "1.0.2": formatted in place into the label it ends.
pub fn key(path: &[usize]) -> PathKey<'_> {
    PathKey(path)
}

pub struct PathKey<'a>(&'a [usize]);

impl std::fmt::Display for PathKey<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (n, i) in self.0.iter().enumerate() {
            if n > 0 {
                f.write_str(".")?;
            }
            write!(f, "{i}")?;
        }
        Ok(())
    }
}

/// a title's path is its parts joined by " / "; a bare slash ("fixed dt 1/60") is part of a word
fn crumbs(sub: &str) -> std::iter::Map<std::str::Split<'_, &'static str>, fn(&str) -> &str> {
    sub.split(" / ").map(str::trim as fn(&str) -> &str)
}

fn numbered((i, part): (usize, &str)) -> (i64, &str) {
    (i as i64, part)
}

/// A path too long for its room loses its head, not its subject: "… / sop". The parts shown are
/// `from` on, behind a "…" when `dots`; each keeps its index in the title, the dots are -1.
struct Fit {
    dots: bool,
    from: usize,
}

impl Fit {
    fn parts<'a>(&self, sub: &'a str) -> std::iter::Chain<std::option::IntoIter<(i64, &'a str)>, std::iter::Map<std::iter::Skip<std::iter::Enumerate<std::iter::Map<std::str::Split<'a, &'static str>, fn(&str) -> &str>>>, fn((usize, &str)) -> (i64, &str)>> {
        self.dots.then_some((-1i64, "\u{2026}")).into_iter().chain(crumbs(sub).enumerate().skip(self.from).map(numbered as fn((usize, &str)) -> (i64, &str)))
    }

    fn len(&self, sub: &str) -> usize {
        self.dots as usize + crumbs(sub).count() - self.from
    }
}

fn fit_crumbs(width: &mut dyn FnMut(&str) -> f64, room: f64, sub: &str) -> Fit {
    let mut fit = Fit { dots: false, from: 0 };
    loop {
        let len = fit.len(sub);
        let mut row = ((len as f64 - 1.0) * (width("/") + 8.0)) - 8.0;
        for (_, part) in fit.parts(sub) {
            row = row + width(part) + 8.0;
        }
        let over = row > room;
        if over && len == 2 && fit.dots {
            fit.dots = false;
            return fit;
        }
        if over && len >= 3 && fit.dots {
            fit.from += 1;
            continue;
        }
        if over && len >= 2 {
            fit = Fit { dots: true, from: 1 };
            continue;
        }
        return fit;
    }
}

/// "Title<TAB>a / b": the kind and its breadcrumb
fn split_title(text: &str) -> (&str, &str) {
    match text.find('\t') {
        Some(i) => (&text[..i], &text[i + 1..]),
        None => (text, ""),
    }
}

fn has_crumbs(sub: &str) -> bool {
    sub.contains(" / ")
}

fn retypes() -> Vec<(&'static str, Panel)> {
    editor_core::panels::PANELS.iter().map(|entry| (entry.menu_title, entry.panel.clone())).collect()
}

fn drag_origin(ui: &mut Ui, boxed: &UiBox, bounds: Bounds, signal: &Signal) -> Bounds {
    if signal.pressed {
        let (x, y, w, h) = bounds;
        ui.set_text_state(boxed, Some(format!("{x} {y} {w} {h}")));
    }
    match ui.text_state(boxed) {
        Some(text) => {
            let parts: Vec<i64> = text.split(' ').filter_map(|p| p.parse::<i64>().ok()).collect();
            if parts.len() == 4 { (parts[0], parts[1], parts[2], parts[3]) } else { bounds }
        }
        None => bounds,
    }
}

fn no_key(_: &str) -> String {
    String::new()
}

fn last_char(key: &str) -> String {
    if key.is_empty() { String::new() } else { key[key.len() - 1..].to_string() }
}

/// The shared panel menu, opened at a right-click and retained on `boxed`.
pub fn panel_menu(
    state: Option<&dyn Fn(&[usize]) -> panels::State>,
    key_of: Option<&dyn Fn(&str) -> String>,
    ui: &mut Ui,
    l: &Leaf,
    boxed: &UiBox,
    at: Option<(f64, f64)>,
) -> Vec<Intent> {
    let state: &dyn Fn(&[usize]) -> panels::State = state.unwrap_or(&|_: &[usize]| panels::default_state());
    let key_of: &dyn Fn(&str) -> String = key_of.unwrap_or(&no_key);
    if let Some((x, y)) = at {
        ui.set_state(boxed, 1);
        ui.set_text_state(boxed, Some(format!("{} {}", cfmt::g(x, 6), cfmt::g(y, 6))));
    }
    if ui.state(boxed, 0) != 1 {
        return Vec::new();
    }
    let (x, y, w, h) = l.frame;
    let default_at = (x as f64, (y + h) as f64);
    let at = match ui.text_state(boxed) {
        Some(text) => {
            let parts: Vec<&str> = text.split(' ').collect();
            if parts.len() == 2 {
                match ((parts[0]).parse::<f64>().ok(), (parts[1]).parse::<f64>().ok()) {
                    (Some(mx), Some(my)) => (mx, my),
                    _ => default_at,
                }
            } else {
                default_at
            }
        }
        None => default_at,
    };
    let kinds = retypes();
    let mut rows: Vec<(String, bool)> = vec![
        ("Split right".to_string(), true),
        ("Split down".to_string(), true),
        ((if state(&l.path).window.is_none() { "Float" } else { "Dock" }).to_string(), true),
        ("Close".to_string(), true),
        (String::new(), false),
    ];
    for (name, _) in &kinds {
        rows.push((name.to_string(), true));
    }
    let mut keys: Vec<String> = vec![
        key_of("panel.split-right"),
        key_of("panel.split-below"),
        key_of("panel.float"),
        key_of("panel.close"),
        String::new(),
    ];
    for (_, panel) in &kinds {
        let id = editor_core::panels::definition(panel).command;
        keys.push(last_char(&key_of(id)));
    }
    let current = 5 + kinds
        .iter()
        .position(|(_, panel)| match (panel, &l.panel) {
            (Panel::View(_), Panel::View(_)) | (Panel::Canvas(_), Panel::Canvas(_)) => true,
            (a, b) => a == b,
        })
        .unwrap_or(0) as i64;
    let opts = MenuOpts { width: Some(232.0), keys, selected: Some(current), lead_from: 5, ..MenuOpts::default() };
    match ui.context_menu(at, &opts, &format!("workspace-menu-{}", key(&l.path)), &rows) {
        MenuResult::Open => Vec::new(),
        MenuResult::Dismiss => {
            ui.set_state(boxed, 0);
            Vec::new()
        }
        MenuResult::Pick(i) => {
            ui.set_state(boxed, 0);
            vec![match i {
                0 => Intent::SplitPanel(l.path.clone(), Axis::H),
                1 => Intent::SplitPanel(l.path.clone(), Axis::V),
                2 => Intent::Window(
                    l.path.clone(),
                    if state(&l.path).window.is_some() { None } else { Some((x, y, 120.max(w), 80.max(h))) },
                ),
                3 => Intent::ClosePanel(l.path.clone()),
                i => Intent::RetypePanel(l.path.clone(), kinds[(i - 5) as usize].1.clone()),
            }]
        }
    }
}

/// The one layout of a header's right end: the groups of tabs standing there, outermost first and
/// `widths` wide, end 8 points apart before the collapse button (a window's dock and close). The
/// right edge of each, and where the title must end. The title gives way first (its breadcrumb,
/// then its label and the focus square: tabs are how a pane is used, the title only names it); a
/// group that would not fit in the header is left out, with those after it.
pub fn header_slots(l: &Leaf, widths: &[f64]) -> (Vec<Option<f64>>, f64) {
    let (x, _, w, _) = l.header;
    let left = x as f64 + 4.0;
    let mut right = (x + w) as f64 - if l.floating { 64.0 } else { 36.0 };
    let mut fits = true;
    let mut slots = Vec::new();
    for width in widths {
        fits = fits && right - width >= left;
        if fits {
            let at = right;
            right = at - width - 8.0;
            slots.push(Some(at));
        } else {
            slots.push(None);
        }
    }
    (slots, right)
}

fn default_title(l: &Leaf) -> String {
    panels::name(&l.panel).to_string()
}

fn no_groups(_: &Leaf) -> Vec<f64> {
    Vec::new()
}

fn edge_color(theme: &theme::Theme, moving: &Rc<RefCell<Option<Path>>>, path: &Path) -> rays_math::color::T {
    if moving.borrow().as_ref() == Some(path) { theme.accent } else { theme::border(theme) }
}

fn cut<'a>(paint: &mut Paint, size: Option<i64>, room: f64, part: &'a str) -> Cow<'a, str> {
    ellipsis(&mut |s| paint.text_width(size, s), room.max(0.0), part)
}

struct Header {
    l: Leaf,
    title: String,
    boxed: UiBox,
    button: UiBox,
}

/// A `title` is "Kind<TAB>a / b": the kind is the header's label, the rest its breadcrumb (the
/// last part in ink), cut to the room `groups` (the widths `header_slots` takes) leave. The header
/// of the `focus` leaf wears the accent square. A docked header ends in its collapse chevron, a
/// window's in dock and close. Panel backgrounds, the drawn gutters, headers with their collapse
/// button and right-click menu. Pure: the host applies the intents.
pub fn update(
    state: Option<&dyn Fn(&[usize]) -> panels::State>,
    hidden: Option<&[Panel]>,
    title: Option<&dyn Fn(&Leaf) -> String>,
    groups: Option<&dyn Fn(&Leaf) -> Vec<f64>>,
    key_of: Option<&dyn Fn(&str) -> String>,
    focus: Option<&Path>,
    tree: &Panels,
    ui: &mut Ui,
    frame: &frame::Frame,
) -> Vec<Intent> {
    let state: &dyn Fn(&[usize]) -> panels::State = state.unwrap_or(&|_: &[usize]| panels::default_state());
    let default_hidden = [Panel::Timeline];
    let hidden: &[Panel] = hidden.unwrap_or(&default_hidden);
    let title: &dyn Fn(&Leaf) -> String = title.unwrap_or(&default_title);
    let groups: &dyn Fn(&Leaf) -> Vec<f64> = groups.unwrap_or(&no_groups);
    let geometry = geometry(Some(state), Some(hidden), tree, frame);
    let theme = ui.theme();
    // the window being carried: its edge is the accent and four brackets stand round it
    let moving: Rc<RefCell<Option<Path>>> = Rc::new(RefCell::new(None));
    for (order, l) in geometry.leaves.iter().enumerate() {
        let order = order as i64;
        match &l.panel {
            // a view paints its own picture; a window over one still has its line-3 edge and the
            // sheet fill of its title row's margin
            Panel::View(_) | Panel::Canvas(_) | Panel::Timeline if l.floating => {
                let boxed = floating(ui, NO_FLAGS, l.frame, format_args!("workspace-window-edge{}", key(&l.path)));
                ui.to_front(order, &boxed);
                let l = l.clone();
                let moving = moving.clone();
                ui.draw_over(&boxed, move |paint, (x, y, w, h)| {
                    let line = edge_color(&theme, &moving, &l.path);
                    let hy = l.header.1;
                    let fy = l.frame.1;
                    paint.fill(x, y, w, (hy - fy) as f64, theme.input);
                    // a viewport window: the sheet's white round the picture and a line-2 frame on it
                    if matches!(l.panel, Panel::View(_)) && l.body.2 > 0 {
                        let (bx, by, bw, bh) = l.body;
                        let (bx, by, bw, bh) = (bx as f64, by as f64, bw as f64, bh as f64);
                        let hy = hy as f64;
                        paint.fill(x, hy, w, by - hy, theme.input);
                        paint.fill(x, by, bx - x, y + h - by, theme.input);
                        paint.fill(bx + bw, by, x + w - bx - bw, y + h - by, theme.input);
                        paint.fill(bx, by + bh, bw, y + h - by - bh, theme.input);
                        paint.frame(bx, by, bw, bh, theme::edge(&theme));
                    }
                    paint.fill(x, y, w, 1.0, line);
                    paint.fill(x, y, 1.0, h, line);
                    paint.fill(x + w - 1.0, y, 1.0, h, line);
                    paint.fill(x, y + h - 1.0, w, 1.0, line);
                });
            }
            // docked, its picture is only its body: the margin over its header is the ground every
            // other pane stands on, not whatever the picture under the chrome shows
            Panel::View(_) | Panel::Canvas(_) | Panel::Timeline => {
                let fy = l.frame.1;
                let hy = l.header.1;
                if hy > fy {
                    let boxed = floating(ui, NO_FLAGS, l.frame, format_args!("workspace-margin{}", key(&l.path)));
                    ui.draw(&boxed, move |paint, (x, y, w, _)| {
                        paint.fill(x, y, w, (hy - fy) as f64, theme.panel);
                    });
                }
            }
            p => {
                let boxed = floating(
                    ui,
                    NO_FLAGS,
                    l.frame,
                    format_args!("workspace-{}{}", panels::lower_name(p), key(&l.path)),
                );
                if l.floating {
                    ui.to_front(order, &boxed);
                }
                let is_floating = l.floating;
                let path = l.path.clone();
                let moving = moving.clone();
                ui.draw(&boxed, move |paint, (x, y, w, h)| {
                    // a window is a sheet with one line-3 edge; a docked panel is the ground
                    if is_floating {
                        paint.fill(x, y, w, h, theme.input);
                        paint.stroke(x + 0.5, y + 0.5, w - 1.0, h - 1.0, edge_color(&theme, &moving, &path));
                    } else {
                        paint.fill(x, y, w, h, theme.panel);
                    }
                });
            }
        }
    }
    let mut intents: Vec<Intent> = Vec::new();
    // where the drag tip goes: over the right of the window being moved
    let mut tip: Option<(i64, i64, i64)> = None;
    for (n, s) in geometry.splitters.iter().enumerate() {
        let boxed = floating(ui, NO_FLAGS, s.bounds, format_args!("workspace-gutter-{n}"));
        ui.draw(&boxed, move |paint, (x, y, w, h)| paint.fill(x, y, w, h, theme::edge(&theme)));
    }
    let mut headers: Vec<Header> = Vec::new();
    for (order, l) in geometry.leaves.iter().enumerate() {
        let order = order as i64;
        let (x, y, w, h) = l.header;
        if h <= 0 {
            continue;
        }
        let (name, path) = (panels::lower_name(&l.panel), key(&l.path));
        let boxed = floating(ui, CLICKABLE | CLIP, l.header, format_args!("workspace-header-{name}{path}"));
        if l.floating {
            ui.to_front(order, &boxed);
        }
        let size = h.min(20);
        // docked: the collapse chevron, a 20-point button 8 points from the edge. A window: dock,
        // then close.
        let windowed = state(&l.path).window.is_some() && !state(&l.path).collapsed;
        let button = floating(
            ui,
            TAB_STOP_MARKED | CLICKABLE,
            (x + 0.max(w - 8 - size), y + ((h - size) / 2), size, size),
            format_args!("workspace-collapse-{name}{path}"),
        );
        if l.floating {
            ui.to_front(order, &button);
        }
        if ui.signal(&button).clicked {
            intents.push(if windowed { Intent::ClosePanel(l.path.clone()) } else { Intent::Toggle(l.path.clone()) });
        }
        let tools = 8 + size + if windowed { size + 8 } else { 0 };
        if windowed {
            let dock = floating(
                ui,
                TAB_STOP_MARKED | CLICKABLE,
                (x + 0.max(w - tools), y + ((h - size) / 2), size, size),
                format_args!("workspace-dock-{name}{path}"),
            );
            ui.to_front(order, &dock);
            let signal = ui.signal(&dock);
            if signal.clicked {
                intents.push(Intent::Window(l.path.clone(), None));
            }
            let hovered = signal.hovered;
            let focused = ui.focused(&dock);
            ui.draw(&dock, move |paint, (x, y, w, h)| {
                if hovered {
                    paint.fill(x, y, w, h, theme::hover_fill(&theme));
                }
                if focused {
                    paint.fill(x, y + h - 1.0, w, 1.0, theme.accent);
                }
                // the dock mark: a chevron-down in ink-2, as the sheet's window rows
                paint.chevron((x + (w / 2.0), y + (h / 2.0)), Dir::Down, theme::ink_2(&theme));
            });
        }
        let grip = floating(
            ui,
            CLICKABLE | BLOCKING,
            (x, y, 0.max(w - tools), h),
            format_args!("workspace-drag-{name}{path}"),
        );
        if l.floating {
            ui.to_front(order, &grip);
        }
        let drag = ui.signal(&grip);
        // a window is dragged from where it is drawn (the layout keeps it inside the frame, whatever
        // its saved place says), with the size it has when it is not collapsed
        let original = match state(&l.path).window {
            Some((_, _, ww, wh)) if l.floating => (l.frame.0, l.frame.1, ww, wh),
            Some(window) => window,
            None => {
                if l.floating {
                    l.frame
                } else {
                    (x, y, 120.max(w), 80.max(h + l.body.3))
                }
            }
        };
        let (ox, oy, ow, oh) = drag_origin(ui, &grip, original, &drag);
        // Escape puts the panel back where the drag began (a docked one stays docked)
        let cancelled = (ui.state(&grip, 0) == 1 && !drag.pressed) || (drag.held && ui.key_pressed(Key::Escape));
        ui.set_state(&grip, if cancelled && !drag.released { 1 } else { 0 });
        if drag.held || drag.released {
            let (px, py) = if drag.released { drag.release_point } else { drag.pointer };
            let (sx, sy) = drag.press_point;
            if (px - sx).hypot(py - sy) >= 4.0 {
                if cancelled {
                    if state(&l.path).window.is_some() {
                        intents.push(Intent::WindowDrag(l.path.clone(), (ox, oy, ow, oh), drag.released));
                    }
                } else {
                    intents.push(Intent::Dragging(l.path.clone(), drag.released));
                    // as far as the layout shows it: a place past the edge would be saved and not
                    // drawn, and the next drag would start from where the window is not
                    let fw = frame.input.size.0;
                    let fh = frame.input.size.1;
                    let wx = 0.max((fw - fw.min(ow)).min(ox + ((px - sx).round() as i64)));
                    let wy = 0.max((fh - HEADER_HEIGHT).min(oy + ((py - sy).round() as i64)));
                    if !drag.released {
                        tip = Some((wx, wy, ow));
                        *moving.borrow_mut() = Some(l.path.clone());
                    }
                    intents.push(Intent::WindowDrag(l.path.clone(), (wx, wy, ow, oh), drag.released));
                }
            }
        }
        // the whole empty header drags; a click that did not move opens the menu
        let still = (drag.release_point.0 - drag.press_point.0).hypot(drag.release_point.1 - drag.press_point.1) < 4.0;
        let at = if context_clicked(&drag) {
            Some(drag.release_point)
        } else if drag.clicked && still {
            Some((x as f64, (y + h) as f64))
        } else {
            None
        };
        intents.extend(panel_menu(Some(state), key_of, ui, l, &boxed, at));
        // the parts of a docked breadcrumb before its last are buttons: a click is `Crumb` of its index
        let text = title(l);
        let (main, sub) = split_title(&text);
        if !sub.is_empty() && !l.floating && !state(&l.path).collapsed && w as f64 >= 60.0 {
            let focused = focus == Some(&l.path);
            let limit = header_slots(l, &groups(l)).1;
            let mut tx = x as f64 + 12.0 + (if focused { 12.0 } else { 0.0 }) + kit::cap_width(ui, &main) + 8.0;
            let fit = fit_crumbs(&mut |p| ui.text_width(None, p), limit - tx, sub);
            let count = fit.len(sub);
            for (n, (index, part)) in fit.parts(sub).enumerate() {
                let pw = ui.text_width(None, part);
                if index >= 0 && n + 1 < count && tx + pw <= limit {
                    let b = floating(ui, CLICKABLE, (tx as i64, y, pw.ceil() as i64, h), format_args!("workspace-crumb-{name}{path}-{index}"));
                    if l.floating {
                        ui.to_front(order, &b);
                    }
                    if ui.signal(&b).clicked {
                        intents.push(Intent::Crumb(l.path.clone(), index));
                    }
                }
                tx = tx + pw + 8.0 + (ui.text_width(None, "/") + 8.0);
            }
        }
        headers.push(Header { l: l.clone(), title: text, boxed, button });
    }
    // the tip of a panel being moved: a 20-point sheet with a line-2 edge, the label size in ink
    if let Some((wx, wy, ww)) = tip {
        let words = "release to dock \u{b7} esc keeps it floating";
        let size = kit::cap_size(ui);
        let tw = ui.text_width(Some(size), words);
        let w = tw + 14.0;
        let tx = ((frame.input.size.0 - 4) as f64 - w).min(wx as f64 + (0.625 * ww as f64));
        let ty = 4.0f64.max(wy as f64 - 30.0);
        let boxed = ui.box_("workspace-drag-tip", BoxOpts::default().w(Size::Px(w)).h(Size::Px(20.0)).at((tx, ty)));
        ui.to_front(i64::MAX, &boxed);
        ui.draw(&boxed, move |paint, (x, y, w, h)| {
            paint.fill(x, y, w, h, theme.input);
            paint.stroke(x + 0.5, y + 0.5, w - 1.0, h - 1.0, theme::edge(&theme));
            let cy = kit::cap_y_paint(paint, y, h);
            paint.text_s((x + 7.0, cy), size, theme.foreground, words);
        });
    }
    let moving_path = moving.borrow().clone();
    if let Some(path) = moving_path {
        for l in &geometry.leaves {
            if l.path == path {
                let (fx, fy, fw, fh) = l.frame;
                let boxed = floating(ui, NO_FLAGS, (fx - 8, fy - 8, fw + 16, fh + 16), format_args!("workspace-moving"));
                ui.to_front(i64::MAX - 1, &boxed);
                // the padding box of the window, brackets 4 points outside it
                ui.draw(&boxed, move |paint, _| {
                    paint.brackets(fx as f64 + 1.0, fy as f64 + 1.0, fw as f64 - 2.0, fh as f64 - 2.0, 4.0, 8.0, 2.0, theme.accent);
                });
            }
        }
    }
    for hd in &headers {
        let l = &hd.l;
        let collapse_hovered = ui.signal(&hd.button).hovered;
        // the keyboard's mark is the sheet's: an accent line over the last row of the button
        if ui.focused(&hd.button) {
            ui.draw_over(&hd.button, move |paint, (x, y, w, h)| paint.fill(x, y + h - 1.0, w, 1.0, theme.accent));
        }
        let panel_state = state(&l.path);
        let collapsed = panel_state.collapsed || hidden.contains(&l.panel);
        let windowed = panel_state.window.is_some() && !panel_state.collapsed;
        let focused = focus == Some(&l.path);
        let mut text_box: Option<UiBox> = None;
        ui.within(&hd.boxed, &mut |ui| {
            text_box = Some(ui.box_("header-text", BoxOpts::default().w(Size::Grow).h(Size::Grow)));
        });
        let text_box = text_box.expect("header-text");
        let limit_slots = header_slots(l, &groups(l)).1;
        let l = l.clone();
        let text = hd.title.clone();
        let cap = kit::cap_size(ui);
        ui.draw(&text_box, move |paint, (x, y, w, h)| {
            paint.fill(x, y, w, h, if l.floating { theme.input } else { theme.panel });
            let ink_2 = theme::ink_2(&theme);
            let ink_3 = theme::ink_3(&theme);
            // "Title<TAB>a / b": the kind as a label, then the breadcrumb, its last part in ink
            let (main, sub) = split_title(&text);
            if w >= 60.0 {
                let mut tx = x + 12.0;
                // the accent square marks the focused pane, docked or floating; the title ends
                // before what follows it: the pane's tabs, then the window's tools; a square or a
                // label with no room is left out whole, and the breadcrumb with it
                let mut limit = limit_slots;
                if focused {
                    if tx + 6.0 <= limit {
                        paint.fill(tx, y + (h / 2.0) - 3.0, 6.0, 6.0, theme.accent);
                    }
                    tx += 12.0;
                }
                let labelled = tx + paint.cap_width(&main) <= limit;
                if !labelled {
                    limit = tx;
                }
                if labelled {
                    let cy = kit::cap_y_paint(paint, y, h);
                    paint.cap((tx, cy), Some(if focused { theme.foreground } else { ink_2 }), &main);
                }
                tx += paint.cap_width(&main) + 8.0;
                let ty = paint.text_top(None, y, h);
                // put: cut to the room left, paint, and advance
                let put = |paint: &mut Paint, tx: &mut f64, color: rays_math::color::T, part: &str| {
                    let part = cut(paint, None, limit - *tx, part);
                    if !part.is_empty() {
                        paint.text_c((*tx, ty), color, &part);
                    }
                    *tx = *tx + paint.text_width(None, &part) + 8.0;
                };
                // a window's title row: the path is one string in ink-2 at the label size
                if !sub.is_empty() && l.floating && !collapsed {
                    // the subject follows the kind, 8 points on (windows.html's title row)
                    let size = cap;
                    let cy = kit::cap_y_paint(paint, y, h);
                    let shown = cut(paint, Some(size), limit - tx, &sub);
                    paint.text_s((tx, cy), size, rays_math::color::with_alpha(ink_2, 179), &shown);
                } else if !sub.is_empty() && !collapsed {
                    let fit = fit_crumbs(&mut |p| paint.text_width(None, p), limit - tx, sub);
                    let count = fit.len(sub);
                    for (i, (_, part)) in fit.parts(sub).enumerate() {
                        if i + 1 == count {
                            put(paint, &mut tx, if has_crumbs(sub) { theme.foreground } else { ink_2 }, part);
                        } else {
                            put(paint, &mut tx, ink_2, part);
                            put(paint, &mut tx, ink_3, "/");
                        }
                    }
                }
                if collapsed {
                    put(paint, &mut tx, ink_3, "collapsed");
                }
            }
            let cx = x + w - 18.0;
            let cy = y + (h / 2.0);
            if collapse_hovered {
                paint.fill(cx - 10.0, cy - 10.0, 20.0, 20.0, theme::hover_fill(&theme));
            }
            if windowed {
                // the window's close mark is the label-size multiplication sign, centred in its button
                let size = cap;
                let tw = paint.text_width(Some(size), "\u{d7}");
                let ty = kit::cap_y_paint(paint, y, h);
                paint.text_s((cx - (tw / 2.0), ty), size, ink_2, "\u{d7}");
            } else {
                paint.chevron((cx, cy), if collapsed { Dir::Down } else { Dir::Left }, if l.floating { ink_2 } else { theme.foreground });
            }
        });
    }
    intents
}

/// where a header's tools begin: after its title, the 8-point gap, the 1 x 12 rule between two
/// 4-point margins and the next 8-point gap (the title is laid out as `update` draws it)
pub fn tools_start(ui: &mut Ui, focused: bool, floating: bool, collapsed: bool, text: &str) -> f64 {
    let (main, sub) = split_title(text);
    let mut x = 12.0 + (if focused { 12.0 } else { 0.0 }) + kit::cap_width(ui, &main);
    if sub.is_empty() {
    } else if floating {
        let size = kit::cap_size(ui);
        x = x + 8.0 + ui.text_width(Some(size), &sub);
    } else {
        for (i, part) in crumbs(sub).enumerate() {
            if i > 0 {
                x = x + 8.0 + ui.text_width(None, "/");
            }
            x = x + 8.0 + ui.text_width(None, part);
        }
    }
    if collapsed {
        x = x + 8.0 + ui.text_width(None, "collapsed");
    }
    x + 25.0
}

/// Dock targets built after pane bodies, with PXUI hover feedback during a panel drag.
pub fn drop_targets(ui: &mut Ui, dragging: Option<(Path, bool)>, geometry: &Geometry, state: &dyn Fn(&[usize]) -> panels::State) -> Vec<Intent> {
    let mut out = Vec::new();
    let Some((source, released)) = dragging else {
        return out;
    };
    for leaf in &geometry.leaves {
        if leaf.path == source || leaf.floating || state(&leaf.path).collapsed {
            continue;
        }
        let (x, y, w, h) = leaf.body;
        let ex = 36.min(w / 3);
        let ey = 36.min(h / 3);
        // the pane's own box sits under its four edges, so the pane under the pointer is known
        let pane = floating(ui, CLICKABLE, (x, y, w, h), format_args!("workspace-drop-pane-{}", key(&leaf.path)));
        ui.to_front(i64::MAX, &pane);
        let pane_hovered = ui.signal(&pane).hovered;
        let mut edges: Vec<(Side, &str, UiBox, bool)> = Vec::new();
        for (side, rect) in [
            (Side::Left, (x, y + ey, ex, 0.max(h - (2 * ey)))),
            (Side::Right, (x + w - ex, y + ey, ex, 0.max(h - (2 * ey)))),
            (Side::Top, (x, y, w, ey)),
            (Side::Bottom, (x, y + h - ey, w, ey)),
        ] {
            let suffix = match side {
                Side::Left => "left",
                Side::Right => "right",
                Side::Top => "top",
                Side::Bottom => "bottom",
            };
            let boxed = floating(ui, CLICKABLE | BLOCKING, rect, format_args!("workspace-drop-{}{}", key(&leaf.path), suffix));
            ui.to_front(i64::MAX, &boxed);
            let hovered = ui.signal(&boxed).hovered;
            edges.push((side, suffix, boxed, hovered));
        }
        // only the pane under the pointer shows its four bars; the nearest turns accent and the
        // half it would take is tinted
        let under = pane_hovered || edges.iter().any(|e| e.3);
        let theme = ui.theme();
        let (fx, fy, fw, fh) = (x as f64, y as f64, w as f64, h as f64);
        for (side, suffix, boxed, hovered) in edges {
            if under {
                let label = format!("split {suffix}");
                ui.draw(&boxed, move |paint, _| {
                    let color = if hovered { theme.accent } else { theme::border(&theme) };
                    match side {
                        Side::Top => paint.fill(fx + (fw / 2.0) - 24.0, fy + 6.0, 48.0, 4.0, color),
                        Side::Bottom => paint.fill(fx + (fw / 2.0) - 24.0, fy + fh - 10.0, 48.0, 4.0, color),
                        Side::Left => paint.fill(fx + 6.0, fy + (fh / 2.0) - 24.0, 4.0, 48.0, color),
                        Side::Right => paint.fill(fx + fw - 10.0, fy + (fh / 2.0) - 24.0, 4.0, 48.0, color),
                    }
                    if hovered {
                        let (hx, hy, hw, hh) = match side {
                            Side::Top => (fx, fy, fw, fh / 2.0),
                            Side::Bottom => (fx, fy + (fh / 2.0), fw, fh / 2.0),
                            Side::Left => (fx, fy, fw / 2.0, fh),
                            Side::Right => (fx + (fw / 2.0), fy, fw / 2.0, fh),
                        };
                        paint.fill(hx, hy, hw, hh, theme::tint(&theme));
                        paint.dashed_rect(hx + 0.5, hy + 0.5, hw - 1.0, hh - 1.0, theme.accent);
                        let cw = paint.cap_width(&label);
                        paint.cap((hx + ((hw - cw) / 2.0), hy + (hh / 2.0) - 6.0), Some(theme.accent), &label);
                    }
                });
            }
            if under && released && hovered {
                out.push(Intent::DockPanel(source.clone(), leaf.path.clone(), side));
            }
        }
    }
    out
}

/// The draggable gutters, wider than they are drawn. Build them after the panes so they sit on
/// top of the neighbours' hit rectangles.
pub fn splitters(state: Option<&dyn Fn(&[usize]) -> panels::State>, hidden: Option<&[Panel]>, tree: &Panels, ui: &mut Ui, frame: &frame::Frame) -> Vec<Intent> {
    let state: &dyn Fn(&[usize]) -> panels::State = state.unwrap_or(&|_: &[usize]| panels::default_state());
    let mut intents: Vec<Intent> = Vec::new();
    let geo = geometry(Some(state), hidden, tree, frame);
    let theme = ui.theme();
    for (n, s) in geo.splitters.iter().enumerate() {
        let Some(node) = &s.node else {
            continue;
        };
        let (x, y, width, height) = s.bounds;
        let (gx, gy) = if s.axis == Axis::H { (3, 0) } else { (0, 3) };
        let hit_rect = ((x - gx) as f64, (y - gy) as f64, (width + (2 * gx)) as f64, (height + (2 * gy)) as f64);
        let mut opts = BoxOpts::default()
            .flags(CLICKABLE | BLOCKING)
            .at((x as f64, y as f64))
            .w(Size::Px(width as f64))
            .h(Size::Px(height as f64));
        opts.hit = Some(Rc::new(move |_| hit_rect));
        let boxed = ui.box_fmt(format_args!("workspace-grip-{n}"), opts);
        let signal = ui.signal(&boxed);
        if signal.hovered || signal.held {
            ui.request_cursor(if s.axis == Axis::H { Cursor::HorizontalResize } else { Cursor::VerticalResize });
        }
        if signal.held {
            let accent = theme.accent;
            ui.draw(&boxed, move |paint, (x, y, w, h)| paint.fill(x, y, w, h, accent));
            // the ratio it makes, in a tip beside the gutter: "58 / 42"
            let (first, second) = sides(s);
            let a = (100.0 * first as f64 / (1.max(first + second)) as f64).round() as i64;
            let tw = kit::cap_width(ui, &format!("{} / {}", a, 100 - a));
            let (px, py) = signal.pointer;
            let (tx, ty) = if s.axis == Axis::H { ((x + 8) as f64, py - 10.0) } else { (px + 8.0, (y + 8) as f64) };
            let tip = ui.box_("workspace-gutter-tip", BoxOpts::default().w(Size::Px(tw)).h(Size::Px(20.0)).at((tx, ty)));
            ui.to_front(i64::MAX, &tip);
            ui.draw(&tip, move |paint, (x, y, _, h)| {
                let cy = kit::cap_y_paint(paint, y, h);
                paint.ratio((x, cy), a, 100 - a);
            });
        }
        let (dx, dy) = signal.drag;
        if (signal.held || signal.released) && (if s.axis == Axis::H { dx } else { dy }) != 0.0 {
            let (px, py) = signal.pointer;
            let along = if s.axis == Axis::H { px } else { py };
            // a fixed split is dragged in whole points of its fixed side, any other by ratio
            let size = match s.size {
                PSize::Ratio(_) => PSize::Ratio((along - s.start as f64) / (1.max(s.span)) as f64),
                PSize::First(_) => PSize::First(along.round() as i64 - s.start),
                PSize::Second(_) => PSize::Second(s.start + s.span - SPLITTER_WIDTH - along.round() as i64),
            };
            intents.push(Intent::Resize { node: node.clone(), size: panels::clamp_size(size) });
        }
        if signal.released {
            intents.push(Intent::Settled);
        }
        // a right-click sizes the split another way, keeping what it shows
        let clicked = context_clicked(&signal);
        // the menu opens where the gutter was clicked
        if clicked {
            let (px, py) = signal.release_point;
            ui.set_text_state(&boxed, Some(format!("{} {}", cfmt::g(px, 6), cfmt::g(py, 6))));
        }
        let opened = ui.state(&boxed, 0) == 1 || clicked;
        ui.set_state(&boxed, if opened { 1 } else { 0 });
        if opened {
            let default_at = (x as f64, y as f64);
            let at = match ui.text_state(&boxed) {
                Some(text) => {
                    let parts: Vec<&str> = text.split(' ').collect();
                    if parts.len() == 2 {
                        match ((parts[0]).parse::<f64>().ok(), (parts[1]).parse::<f64>().ok()) {
                            (Some(a), Some(b)) => (a, b),
                            _ => default_at,
                        }
                    } else {
                        default_at
                    }
                }
                None => default_at,
            };
            match ui.context_menu(at, &MenuOpts::default(), &format!("workspace-size-menu-{n}"), &size_rows(s)) {
                MenuResult::Open => {}
                MenuResult::Dismiss => ui.set_state(&boxed, 0),
                MenuResult::Pick(i) => {
                    ui.set_state(&boxed, 0);
                    intents.push(Intent::Resize { node: node.clone(), size: resized(s, SIZE_WAYS[i as usize]) });
                    intents.push(Intent::Settled);
                }
            }
        }
    }
    for (order, leaf) in geo.leaves.iter().enumerate() {
        let order = order as i64;
        if state(&leaf.path).window.is_none() || state(&leaf.path).collapsed {
            continue;
        }
        let bounds = leaf.frame;
        let (x, y, w, h) = bounds;
        let boxed = floating(ui, CLICKABLE | BLOCKING, (x + w - 12, y + h - 12, 12, 12), format_args!("workspace-window-size-{}", key(&leaf.path)));
        ui.to_front(order, &boxed);
        let signal = ui.signal(&boxed);
        let (x, y, w, h) = drag_origin(ui, &boxed, bounds, &signal);
        if signal.held || signal.released {
            let (px, py) = if signal.released { signal.release_point } else { signal.pointer };
            let (sx, sy) = signal.press_point;
            if px != sx || py != sy {
                intents.push(Intent::WindowDrag(
                    leaf.path.clone(),
                    (x, y, 120.max(w + (px - sx).round() as i64), 80.max(h + (py - sy).round() as i64)),
                    signal.released,
                ));
            }
        }
        let ink = theme::ink_3(&theme);
        ui.draw(&boxed, move |paint, (x, y, w, h)| {
            // an 8-point corner of two 1-point lines, 4 points in from the window's edges
            paint.fill(x + w - 5.0, y + h - 12.0, 1.0, 8.0, ink);
            paint.fill(x + w - 12.0, y + h - 5.0, 8.0, 1.0, ink);
        });
    }
    intents
}
