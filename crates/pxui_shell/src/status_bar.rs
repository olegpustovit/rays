//! Port of lib/pxui_shell/status_bar.ml: the status strip, the guide strip and the echo tips.
use crate::kit;
use editor_core::guide_context;
use pxui::theme;
use pxui::ui::*;

/// What the dot after the file says: checked and cooked, cooking, refused.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum State {
    Ok,
    Busy,
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TipKind {
    Info,
    Refusal,
}

/// The optional arguments of `guide` and `draw`.
#[derive(Clone, Debug)]
pub struct Opts {
    pub file: String,
    pub state: State,
    pub layout: String,
    pub text: String,
    pub fps: Option<i64>,
    pub notes: Vec<String>,
    pub readout: String,
    pub accent: bool,
    pub extra: Vec<(String, String)>,
    pub leader: Option<String>,
    pub kind: Option<String>,
    pub selection: Option<String>,
}

impl Default for Opts {
    fn default() -> Opts {
        Opts {
            file: String::new(),
            state: State::Ok,
            layout: String::new(),
            text: String::new(),
            fps: None,
            notes: Vec::new(),
            readout: String::new(),
            accent: false,
            extra: Vec::new(),
            leader: None,
            kind: None,
            selection: None,
        }
    }
}

fn string_of_float(x: f64) -> String {
    let s = cfmt::g(x, 12);
    if s.bytes().all(|c| c == b'-' || c.is_ascii_digit()) { format!("{s}.") } else { s }
}

/// where the lead's parts go: the file, the dot, the status cut to `limit`, and the end
fn lead_plan(width: &mut dyn FnMut(&str) -> f64, x: f64, file: &str, limit: f64, text: &str) -> (f64, f64, f64, String, f64) {
    let mut tx = x + 12.0;
    let file_x = tx;
    if !file.is_empty() {
        tx = tx + width(file) + 8.0;
    }
    let dot_x = tx;
    tx += 14.0;
    let shown = ellipsis(width, (limit - tx).max(0.0), text);
    let after = tx + width(&shown) + 8.0;
    (file_x, dot_x, tx, shown, after)
}

/// What every strip starts with: the file, a dot for the state (checked, cooking, refused) and
/// the status line in ink-2, cut to `limit`; where the next thing goes.
fn lead(paint: &mut Paint, (x, y, h): (f64, f64, f64), file: &str, state: State, limit: f64, text: &str) -> f64 {
    let theme = paint.theme;
    let ty = paint.text_top(None, y, h);
    let (file_x, dot_x, text_x, shown, after) = lead_plan(&mut |s| paint.text_width(None, s), x, file, limit, text);
    if !file.is_empty() {
        paint.text_c((file_x, ty), theme.foreground, file);
    }
    let fill = match state {
        State::Ok => theme::ports(&theme).int,
        State::Busy => theme.accent,
        State::Error => theme::invalid(),
    };
    paint.circle((dot_x + 3.0, y + (h / 2.0)), 3.0, Some(fill), None);
    paint.text_c((text_x, ty), if state == State::Error { theme::invalid() } else { theme::ink_2(&theme) }, &shown);
    after
}

/// where the end's parts start (the layout, then the frame rate), 12 from the edge, 8 apart
fn trail_start(ui: &mut Ui, x: f64, w: f64, notes: &[String], readout: &str, layout: &str, fps: Option<i64>) -> f64 {
    let mut right = x + w - 12.0;
    if let Some(fps) = fps {
        right = right - kit::cap_width(ui, &format!("{fps} fps")) - 8.0;
    }
    if !layout.is_empty() {
        right = right - kit::cap_width(ui, layout) - 8.0;
    }
    for note in notes.iter().rev() {
        right = right - kit::cap_width(ui, note) - 8.0;
    }
    if readout.is_empty() { right } else { right - ui.text_width(None, readout) - 8.0 }
}

/// the strip's end: the layout in use, then the frame rate in ink, 8 apart; where they start
fn trail(paint: &mut Paint, (x, y, w, h): (f64, f64, f64, f64), notes: &[String], readout: &str, layout: &str, fps: Option<i64>) -> f64 {
    let theme = paint.theme;
    let cy = kit::cap_y_paint(paint, y, h);
    let mut right = x + w - 12.0;
    if let Some(fps) = fps {
        let fps = format!("{fps} fps");
        right -= paint.cap_width(&fps);
        paint.cap((right, cy), Some(theme.foreground), &fps);
        right -= 8.0;
    }
    if !layout.is_empty() {
        right -= paint.cap_width(layout);
        paint.cap((right, cy), None, layout);
        right -= 8.0;
    }
    // notes ("3 floating", "3 graphs") stand left of the layout
    for note in notes.iter().rev() {
        right -= paint.cap_width(note);
        paint.cap((right, cy), None, note);
        right -= 8.0;
    }
    // a graph alone in the strip: its counts in ink-2 before the zoom
    if !readout.is_empty() {
        right -= paint.text_width(None, readout);
        let ty = paint.text_top(None, y, h);
        paint.text_c((right, ty), theme::ink_2(&theme), readout);
        right -= 8.0;
    }
    right
}

/// the hairline above the bar; the bar is the 24 points under it
fn ground(paint: &mut Paint, (x, y, w, h): (f64, f64, f64, f64)) -> (f64, f64, f64, f64) {
    let theme = paint.theme;
    paint.fill(x, y, w, h, theme.panel);
    paint.fill(x, y, w, 1.0, theme::edge(&theme));
    (x, y + 1.0, w, h - 1.0)
}

/// the rule, then the focused pane's kind (ink) and what is selected (ink-2), both labels; where
/// the next thing goes
fn focus_labels(paint: &mut Paint, (y, h): (f64, f64), rule: bool, accent: bool, after: f64, kind: Option<&str>, selection: Option<&str>) -> f64 {
    let theme = paint.theme;
    let mut tx = after + 17.0;
    if rule {
        paint.fill(after + 4.0, y + ((h - 12.0) / 2.0), 1.0, 12.0, theme::border(&theme));
    }
    let mut items: Vec<(&str, rays_math::color::T)> = Vec::new();
    if let Some(k) = kind {
        items.push((k, if accent { theme.accent } else { theme.foreground }));
    }
    if let Some(s) = selection {
        items.push((s, theme::ink_2(&theme)));
    }
    for (text, color) in items {
        let cy = kit::cap_y_paint(paint, y, h);
        paint.cap((tx, cy), Some(color), text);
        tx = tx + paint.cap_width(text) + 8.0;
    }
    tx
}

/// where the labels of `focus_labels` end, without painting
fn focus_end(ui: &mut Ui, after: f64, kind: Option<&str>, selection: Option<&str>) -> f64 {
    let mut tx = after + 17.0;
    for text in [kind, selection].into_iter().flatten() {
        tx = tx + kit::cap_width(ui, text) + 8.0;
    }
    tx
}

/// The status strip without the keys: the file, its state, the whole status line, the layout in
/// use and the frame rate. `o.text` is the status line.
pub fn draw(ui: &mut Ui, bounds: (i64, i64, i64, i64), o: &Opts) {
    let (x, y, width, height) = bounds;
    if height > 0 {
        let boxed = ui.box_(
            "workspace-status",
            BoxOpts::default().flags(CLIP).w(Size::Px(width as f64)).h(Size::Px(height as f64)).at((x as f64, y as f64)),
        );
        let o = o.clone();
        ui.draw(&boxed, move |paint, b| {
            let (x, y, w, h) = ground(paint, b);
            let right = trail(paint, (x, y, w, h), &o.notes, &o.readout, &o.layout, o.fps);
            let limit = if o.kind.is_none() && o.selection.is_none() { right } else { right - 160.0 };
            let after = lead(paint, (x, y, h), &o.file, o.state, limit, &o.text);
            if o.kind.is_some() || o.selection.is_some() {
                focus_labels(paint, (y, h), true, false, after, o.kind.as_deref(), o.selection.as_deref());
            }
        });
    }
}

/// The strip of the workspace sheet: a hairline over a 24-point bar of the file, its state dot
/// and status line (a quarter of the strip at most), a rule, the focused pane's `kind` and the
/// `selection` as labels, each `extra` pair's key in ink-3 before what it does in ink-2, then the
/// `notes`, the layout in use and the frame rate at the right. With `leader` (the pending prefix)
/// the strip is the sheet's `09` one: the file, a rule, the prefix in the accent and `waiting for a
/// key`.
pub fn guide(ui: &mut Ui, bounds: (i64, i64, i64, i64), o: &Opts, context: guide_context::t) {
    let (x, y, width, height) = bounds;
    if height <= 0 {
        return;
    }
    let theme = ui.theme();
    match &o.leader {
        Some(pending) => {
            // an open leader: the file and its state, a rule, the pending prefix in the accent and
            // `waiting for a key`, the frame rate at the end
            let boxed = ui.box_(
                "workspace-guide",
                BoxOpts::default().flags(CLIP).w(Size::Px(width as f64)).h(Size::Px(height as f64)).at((x as f64, y as f64)),
            );
            let o = o.clone();
            let pending = pending.clone();
            ui.draw(&boxed, move |paint, b| {
                let (x, y, w, h) = ground(paint, b);
                trail(paint, (x, y, w, h), &o.notes, &o.readout, &o.layout, o.fps);
                let after = lead(paint, (x, y, h), &o.file, o.state, x + 320.0f64.min(w / 4.0), &o.text);
                let tx = focus_labels(paint, (y, h), true, true, after, Some(&pending), None);
                let ty = paint.text_top(None, y, h);
                paint.text_c((tx, ty), theme::ink_2(&theme), "waiting for a key");
            });
        }
        None => {
            let bar = ui.box_(
                "workspace-guide",
                BoxOpts::default()
                    .flags(CLICKABLE | CLIP)
                    .w(Size::Px(width as f64))
                    .h(Size::Px(height as f64))
                    .at((x as f64, y as f64)),
            );
            let keys = o.extra.clone();
            let title = guide_context::name(context);
            // the kind and what is selected; a context that is no node's (the leader, a search) names itself
            let kind: Option<String> = Some(o.kind.clone().unwrap_or_else(|| title.to_string()));
            let selection: Option<String> = match context {
                guide_context::t::Canvas
                | guide_context::t::Node
                | guide_context::t::Multi
                | guide_context::t::List
                | guide_context::t::Text => o.selection.clone(),
                _ => Some(title.to_string()),
            };
            let fx = x as f64;
            let fw = width as f64;
            let limit = trail_start(ui, fx, fw, &o.notes, &o.readout, &o.layout, o.fps);
            let has_lead = !(o.file.is_empty() && o.text.is_empty());
            // the lead (file, dot, status) takes what the labels and the key hints leave, at least a
            // quarter of the strip (320 at most); its rule drags it to a width kept in the strip's box
            // state, for the session
            let labels_w = focus_end(ui, 0.0, kind.as_deref(), selection.as_deref());
            let small = kit::cap_size(ui);
            let mut keys_w = 0.0;
            for (k, l) in &keys {
                keys_w = keys_w + ui.text_width(Some(small), k) + ui.text_width(None, l) + 16.0;
            }
            let max_w = (limit - labels_w - 8.0 - fx).max(0.0);
            let natural_limit = (fx + 320.0f64.min(fw / 4.0)).max(limit - labels_w - keys_w - 8.0);
            let natural_end = if has_lead {
                let (_, _, _, _, after) = lead_plan(&mut |s| ui.text_width(None, s), fx, &o.file, natural_limit, &o.text);
                after
            } else {
                fx - 5.0
            };
            let stored = |ui: &Ui| -> Option<f64> {
                ui.text_state(&bar).and_then(|t| cfmt::parse_ocaml_float(&t)).map(|w| w.min(max_w))
            };
            let split_x = (match stored(ui) {
                Some(w) => fx + w,
                None => natural_end,
            }) + 1.0;
            let split = ui.box_(
                "workspace-guide-split",
                BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(7.0)).h(Size::Px(height as f64)).at((split_x, y as f64)),
            );
            let (lead_limit, after_lead) = if !has_lead {
                (natural_limit, natural_end)
            } else {
                let sg = ui.signal(&split);
                let w = stored(ui);
                // the hit box sits on the rule: it follows the stored width once there is one
                if sg.hovered || sg.held {
                    ui.request_cursor(Cursor::Horizontal_resize);
                }
                if sg.pressed && w.is_none() {
                    ui.set_text_state(&bar, Some(string_of_float(natural_end - fx)));
                }
                if sg.held {
                    ui.set_text_state(&bar, Some(string_of_float(max_w.min(24.0f64.max(sg.pointer.0 - 4.0 - fx)))));
                }
                match stored(ui) {
                    Some(w) => (fx + w, fx + w),
                    None => (natural_limit, natural_end),
                }
            };
            let labels_end = focus_end(ui, after_lead, kind.as_deref(), selection.as_deref());
            let mut pairs: Vec<(f64, String, String, f64)> = Vec::new();
            let mut tx = labels_end;
            for (key, label) in &keys {
                let kw = ui.text_width(Some(small), key);
                let lw = ui.text_width(None, label);
                if tx + kw + 8.0 + lw > limit - 8.0 {
                    tx = f64::INFINITY;
                } else {
                    pairs.push((tx, key.clone(), label.clone(), kw));
                    tx = tx + kw + 8.0 + lw + 8.0;
                }
            }
            let o2 = o.clone();
            let accent = o.accent;
            ui.draw(&bar, move |paint, b| {
                let (x, y, w, h) = ground(paint, b);
                trail(paint, (x, y, w, h), &o2.notes, &o2.readout, &o2.layout, o2.fps);
                // the file and its state, to the rule, then the labels
                if has_lead {
                    lead(paint, (x, y, h), &o2.file, o2.state, lead_limit, &o2.text);
                    focus_labels(paint, (y, h), true, accent, after_lead, kind.as_deref(), selection.as_deref());
                } else {
                    focus_labels(paint, (y, h), false, accent, x - 5.0, kind.as_deref(), selection.as_deref());
                }
                // each key in ink-3 at the label size before what it does in ink-2, 8 apart
                for (tx, key, label, kw) in &pairs {
                    let cy = kit::cap_y_paint(paint, y, h);
                    paint.text_s((*tx, cy), small, theme::ink_3(&theme), key);
                    let ty = paint.text_top(None, y, h);
                    paint.text_c((tx + kw + 8.0, ty), theme::ink_2(&theme), label);
                }
            });
            if ui.signal(&bar).hovered {
                let names: Vec<String> = keys.iter().map(|(k, _)| k.clone()).collect();
                ui.tooltip("guide-strip", &format!("{} \u{b7} {} \u{b7} / ?: all keys", title, names.join("  ")));
            }
        }
    }
}

/// Echo, the sheet's `08`: tips stacked 4 apart in the pane's bottom-left corner, the last at the
/// bottom. A tip is 24 high on the sheet fill with a line-2 edge and 13-point text 7 in;
/// information starts with a 6-point dot in the hint colour, a refusal reads in the error ink.
pub fn tips(ui: &mut Ui, bounds: (i64, i64, i64, i64), avoid: &[(f64, f64, f64, f64)], tips: &[(String, TipKind)]) {
    let (x, y, width, height) = bounds;
    if width > 0 && height > 0 && !tips.is_empty() {
        let theme = ui.theme();
        let count = tips.len();
        for (index, (text, kind)) in tips.iter().enumerate() {
            let extra = match kind {
                TipKind::Info => 32.0,
                TipKind::Refusal => 14.0,
            };
            let w = ((0.max(width - 24)) as f64).min(ui.text_width(None, text) + extra);
            let top = (y + 0.max(height - 12)) as f64 - ((count - index) as f64 * 28.0) + 4.0;
            // a tip that would sit on a rectangle of `avoid` (a graph card) moves up above it
            let left = (x + 12) as f64;
            let mut sorted: Vec<(f64, f64, f64, f64)> = avoid.to_vec();
            sorted.sort_by(|(_, a, _, _), (_, b, _, _)| b.partial_cmp(a).unwrap_or(std::cmp::Ordering::Equal));
            let mut top = top;
            for (ax, ay, aw, ah) in sorted {
                if left < ax + aw && ax < left + w && top < ay + ah && ay < top + 24.0 {
                    top = ay - 28.0;
                }
            }
            let boxed = ui.box_(
                &format!("echo-tip-{index}"),
                BoxOpts::default().flags(CLIP).w(Size::Px(w)).h(Size::Px(24.0)).at(((x + 12) as f64, top)),
            );
            let kind = *kind;
            let text = text.clone();
            ui.draw(&boxed, move |paint, (x, y, w, h)| {
                paint.fill(x, y, w, h, theme.input);
                paint.frame(x, y, w, h, theme::edge(&theme));
                let ty = paint.text_top(None, y, h);
                match kind {
                    TipKind::Info => {
                        paint.circle((x + 10.0, y + (h / 2.0)), 3.0, Some(theme::ports(&theme).hint), None);
                        paint.text_c((x + 19.0, ty), theme.foreground, &text);
                    }
                    TipKind::Refusal => paint.text_c((x + 7.0, ty), theme::invalid(), &text),
                }
            });
        }
    }
}
