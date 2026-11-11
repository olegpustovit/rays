//! the status strip, the guide strip and the echo tips.
use crate::kit;
use std::borrow::Cow;
use std::hash::Hash;
use editor_core::guide_context;
use pxui::theme;
use pxui::ui::*;

/// What the dot after the file says: checked and cooked, cooking, refused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
#[derive(Clone, Debug, Hash)]
pub struct Opts {
    pub file: String,
    pub state: State,
    pub layout: String,
    pub text: String,
    pub fps: Option<i64>,
    pub notes: Vec<String>,
    pub readout: String,
    pub accent: bool,
    pub extra: Vec<(Cow<'static, str>, Cow<'static, str>)>,
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

/// where the lead's parts go: the file, the dot, the status cut to `limit`, and the end
fn lead_plan<'a>(width: &mut dyn FnMut(&str) -> f32, x: f32, file: &str, limit: f32, text: &'a str) -> (f32, f32, f32, Cow<'a, str>, f32) {
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

/// What every strip starts with: the file and a dot for the state (checked, cooking, refused); the
/// status line after them is `line`'s. Where the next thing goes.
fn lead(paint: &mut Paint, (x, y, h): (f32, f32, f32), file: &str, state: State, limit: f32, text: &str) -> f32 {
    let theme = paint.theme;
    let ty = paint.text_top(None, y, h);
    let (file_x, dot_x, _, _, after) = lead_plan(&mut |s| paint.text_width(None, s), x, file, limit, text);
    if !file.is_empty() {
        paint.text_c((file_x, ty), theme.foreground, file);
    }
    let fill = match state {
        State::Ok => crate::kit::port_ink(&theme, crate::kit::Port::Int),
        State::Busy => theme.accent,
        State::Error => theme::invalid(),
    };
    paint.circle((dot_x + 3.0, y + (h / 2.0)), 3.0, Some(fill), None);
    after
}

/// The status line in ink-2, cut to `limit`, in a small view of its own over the strip's: a readout
/// that changes every frame (the frame cook) rebuilds only this. Returns where the lead ends, which
/// the strip's revision takes in place of the text.
fn line(ui: &mut Ui, label: &str, bounds: (i64, i64, i64, i64), file: &str, state: State, limit: f32, text: &str) -> f32 {
    let (x, y, w, h) = bounds;
    let (_, _, text_x, shown, after) = lead_plan(&mut |s| ui.text_width(None, s), x as f32, file, limit, text);
    let rev = revision(&|hs| (bounds, state, text_x.to_bits(), &*shown).hash(hs));
    let at = (text_x.floor() as i64).clamp(x, x + w);
    strip(ui, label, (at, y, x + w - at, h), rev, &mut |ui, boxed| {
        let shown = shown.to_string();
        ui.draw(boxed, move |paint, b| {
            let theme = paint.theme;
            let ty = paint.text_top(None, b.y + 1.0, b.h - 1.0);
            paint.text_c((text_x, ty), if state == State::Error { theme::invalid() } else { theme::ink_2(&theme) }, &shown);
        });
    });
    after
}

/// what a strip's revision takes from its `Opts`: all of it but the status line (`line`'s)
fn hash_sans_text(o: &Opts, h: &mut std::hash::DefaultHasher) {
    (&o.file, o.state, &o.layout, o.fps, &o.notes, &o.readout, o.accent, &o.extra, &o.leader, &o.kind, &o.selection).hash(h);
}

/// where the end's parts start (the layout, then the frame rate), 12 from the edge, 8 apart
fn trail_start(ui: &mut Ui, x: f32, w: f32, notes: &[String], readout: &str, layout: &str, fps: Option<i64>) -> f32 {
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
fn trail(paint: &mut Paint, (x, y, w, h): (f32, f32, f32, f32), notes: &[String], readout: &str, layout: &str, fps: Option<i64>) -> f32 {
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
fn ground(paint: &mut Paint, Rect { x, y, w, h }: Rect) -> Rect {
    let theme = paint.theme;
    paint.fill(x, y, w, h, theme.panel);
    paint.fill(x, y, w, 1.0, theme::edge(&theme));
    Rect::new(x, y + 1.0, w, h - 1.0)
}

/// the rule, then the focused pane's kind (ink) and what is selected (ink-2), both labels; where
/// the next thing goes
fn focus_labels(paint: &mut Paint, (y, h): (f32, f32), rule: bool, accent: bool, after: f32, kind: Option<&str>, selection: Option<&str>) -> f32 {
    let theme = paint.theme;
    let mut tx = after + 17.0;
    if rule {
        paint.fill(after + 4.0, y + ((h - 12.0) / 2.0), 1.0, 12.0, theme::border(&theme));
    }
    for (text, color) in [kind.map(|k| (k, if accent { theme.accent } else { theme.foreground })), selection.map(|s| (s, theme::ink_2(&theme)))].into_iter().flatten() {
        let cy = kit::cap_y_paint(paint, y, h);
        paint.cap((tx, cy), Some(color), text);
        tx = tx + paint.cap_width(text) + 8.0;
    }
    tx
}

/// where the labels of `focus_labels` end, without painting
fn focus_end(ui: &mut Ui, after: f32, kind: Option<&str>, selection: Option<&str>) -> f32 {
    let mut tx = after + 17.0;
    for text in [kind, selection].into_iter().flatten() {
        tx = tx + kit::cap_width(ui, text) + 8.0;
    }
    tx
}

/// The strip's paint, in a cached view of its own at `bounds`: `paint` builds the one painted box
/// (`CLIP`, filling the view) only when `revision` (the hash of what it shows, with the bounds) changes.
/// The strip's hit boxes stay at the root over it, so hovering or dragging them rebuilds nothing.
fn strip(ui: &mut Ui, label: &str, bounds: (i64, i64, i64, i64), revision: u64, paint: &mut dyn FnMut(&mut Ui, &UiBox)) {
    let (x, y, w, h) = bounds;
    let view = ui.view(label, revision, Size::Px(w as f32), Size::Px(h as f32), &mut |ui| {
        let boxed = ui.box_(label, BoxOpts::default().flags(CLIP).w(Size::Grow).h(Size::Grow));
        paint(ui, &boxed);
    });
    ui.set_at(&view, (x as f32, y as f32));
}

use crate::kit::revision;

/// The status strip without the keys: the file, its state, the whole status line, the layout in
/// use and the frame rate. `o.text` is the status line.
pub fn draw(ui: &mut Ui, bounds: (i64, i64, i64, i64), o: &Opts) {
    let (x, _, width, height) = bounds;
    if height > 0 {
        let right = trail_start(ui, x as f32, width as f32, &o.notes, &o.readout, &o.layout, o.fps);
        let limit = if o.kind.is_none() && o.selection.is_none() { right } else { right - 160.0 };
        let after = lead_plan(&mut |s| ui.text_width(None, s), x as f32, &o.file, limit, &o.text).4;
        let rev = revision(&|h| (bounds, after.to_bits(), hash_sans_text(o, h)).hash(h));
        strip(ui, "workspace-status", bounds, rev, &mut |ui, boxed| {
            let o = o.clone();
            ui.draw(boxed, move |paint, b| {
                let Rect { x, y, w, h } = ground(paint, b);
                trail(paint, (x, y, w, h), &o.notes, &o.readout, &o.layout, o.fps);
                lead(paint, (x, y, h), &o.file, o.state, limit, &o.text);
                if o.kind.is_some() || o.selection.is_some() {
                    focus_labels(paint, (y, h), true, false, after, o.kind.as_deref(), o.selection.as_deref());
                }
            });
        });
        line(ui, "workspace-status-line", bounds, &o.file, o.state, limit, &o.text);
    }
}

/// The strip of the workspace sheet: a hairline over a 24-point bar of the file, its state dot
/// and status line (a quarter of the strip at most), a rule, the focused pane's `kind` and the
/// `selection` as labels, each `extra` pair's key in ink-3 before what it does in ink-2, then the
/// `notes`, the layout in use and the frame rate at the right. With `leader` (the pending prefix)
/// the strip is the sheet's `09` one: the file, a rule, the prefix in the accent and `waiting for a
/// key`.
pub fn guide(ui: &mut Ui, bounds: (i64, i64, i64, i64), o: &Opts, context: guide_context::GuideContext) {
    let (x, y, width, height) = bounds;
    if height <= 0 {
        return;
    }
    let theme = ui.tokens();
    match &o.leader {
        Some(pending) => {
            // an open leader: the file and its state, a rule, the pending prefix in the accent and
            // `waiting for a key`, the frame rate at the end
            let limit = x as f32 + 320.0f32.min(width as f32 / 4.0);
            let after = lead_plan(&mut |s| ui.text_width(None, s), x as f32, &o.file, limit, &o.text).4;
            let rev = revision(&|h| (bounds, after.to_bits(), &pending, hash_sans_text(o, h)).hash(h));
            strip(ui, "workspace-guide-leader", bounds, rev, &mut |ui, boxed| {
            let o = o.clone();
            let pending = pending.clone();
            ui.draw(boxed, move |paint, b| {
                let Rect { x, y, w, h } = ground(paint, b);
                trail(paint, (x, y, w, h), &o.notes, &o.readout, &o.layout, o.fps);
                lead(paint, (x, y, h), &o.file, o.state, limit, &o.text);
                let tx = focus_labels(paint, (y, h), true, true, after, Some(&pending), None);
                let ty = paint.text_top(None, y, h);
                paint.text_c((tx, ty), theme::ink_2(&theme), "waiting for a key");
            });
            });
            line(ui, "workspace-guide-line", bounds, &o.file, o.state, limit, &o.text);
        }
        None => {
            let bar = ui.box_(
                "workspace-guide",
                BoxOpts::default()
                    .flags(CLICKABLE | CLIP)
                    .w(Size::Px(width as f32))
                    .h(Size::Px(height as f32))
                    .at((x as f32, y as f32)),
            );
            let keys = &o.extra;
            let title = guide_context::name(context);
            // the kind and what is selected; a context that is no node's (the leader, a search) names itself
            let kind: Option<&str> = Some(o.kind.as_deref().unwrap_or(title));
            let selection: Option<&str> = match context {
                guide_context::GuideContext::Canvas
                | guide_context::GuideContext::Node
                | guide_context::GuideContext::Multi
                | guide_context::GuideContext::List
                | guide_context::GuideContext::Text => o.selection.as_deref(),
                _ => Some(title),
            };
            let fx = x as f32;
            let fw = width as f32;
            let limit = trail_start(ui, fx, fw, &o.notes, &o.readout, &o.layout, o.fps);
            let has_lead = !(o.file.is_empty() && o.text.is_empty());
            // the lead (file, dot, status) takes what the labels and the key hints leave, at least a
            // quarter of the strip (320 at most); its rule drags it to a width kept in the strip's box
            // state, for the session
            let labels_w = focus_end(ui, 0.0, kind.as_deref(), selection.as_deref());
            let small = kit::cap_size(ui);
            let mut keys_w = 0.0;
            for (k, l) in keys {
                keys_w = keys_w + ui.text_width(Some(small), k) + ui.text_width(None, l) + 16.0;
            }
            let max_w = (limit - labels_w - 8.0 - fx).max(0.0);
            let natural_limit = (fx + 320.0f32.min(fw / 4.0)).max(limit - labels_w - keys_w - 8.0);
            let natural_end = if has_lead {
                let (_, _, _, _, after) = lead_plan(&mut |s| ui.text_width(None, s), fx, &o.file, natural_limit, &o.text);
                after
            } else {
                fx - 5.0
            };
            let stored = |ui: &Ui| -> Option<f32> {
                ui.text_state(&bar).and_then(|t| (&t).parse::<f32>().ok()).map(|w| w.min(max_w))
            };
            let split_x = (match stored(ui) {
                Some(w) => fx + w,
                None => natural_end,
            }) + 1.0;
            let split = ui.box_(
                "workspace-guide-split",
                BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(7.0)).h(Size::Px(height as f32)).at((split_x, y as f32)),
            );
            let (lead_limit, after_lead) = if !has_lead {
                (natural_limit, natural_end)
            } else {
                let sg = ui.signal(&split);
                let w = stored(ui);
                // the hit box sits on the rule: it follows the stored width once there is one
                if sg.hovered || sg.held {
                    ui.request_cursor(Cursor::HorizontalResize);
                }
                if sg.pressed && w.is_none() {
                    ui.set_text_state(&bar, Some((natural_end - fx).to_string()));
                }
                if sg.held {
                    ui.set_text_state(&bar, Some(max_w.min(24.0f32.max(sg.pointer.0 - 4.0 - fx)).to_string()));
                }
                match stored(ui) {
                    Some(w) => (fx + w, fx + w),
                    None => (natural_limit, natural_end),
                }
            };
            let rev = revision(&|h| (bounds, context as u8, lead_limit.to_bits(), after_lead.to_bits(), hash_sans_text(o, h)).hash(h));
            strip(ui, "workspace-guide-paint", bounds, rev, &mut |ui, boxed| {
            let labels_end = focus_end(ui, after_lead, kind.as_deref(), selection.as_deref());
            let mut pairs: Vec<(f32, String, String, f32)> = Vec::new();
            let mut tx = labels_end;
            for (key, label) in keys {
                let kw = ui.text_width(Some(small), key);
                let lw = ui.text_width(None, label);
                if tx + kw + 8.0 + lw > limit - 8.0 {
                    tx = f32::INFINITY;
                } else {
                    pairs.push((tx, key.to_string(), label.to_string(), kw));
                    tx = tx + kw + 8.0 + lw + 8.0;
                }
            }
            let o2 = o.clone();
            let accent = o.accent;
            let (kind, selection) = (kind.map(str::to_owned), selection.map(str::to_owned));
            ui.draw(boxed, move |paint, b| {
                let Rect { x, y, w, h } = ground(paint, b);
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
            });
            if has_lead {
                line(ui, "workspace-guide-line", bounds, &o.file, o.state, lead_limit, &o.text);
            }
            if ui.signal(&bar).hovered {
                let names: Vec<String> = keys.iter().map(|(k, _)| k.to_string()).collect();
                ui.tooltip("guide-strip", &format!("{} \u{b7} {} \u{b7} / ?: all keys", title, names.join("  ")));
            }
        }
    }
}

/// Echo, the sheet's `08`: tips stacked 4 apart in the pane's bottom-left corner, the last at the
/// bottom. A tip is 24 high on the sheet fill with a line-2 edge and 13-point text 7 in;
/// information starts with a 6-point dot in the hint colour, a refusal reads in the error ink.
pub fn tips(ui: &mut Ui, bounds: (i64, i64, i64, i64), avoid: &[(f32, f32, f32, f32)], tips: &[(String, TipKind)]) {
    let (x, y, width, height) = bounds;
    if width > 0 && height > 0 && !tips.is_empty() {
        let theme = ui.tokens();
        let count = tips.len();
        for (index, (text, kind)) in tips.iter().enumerate() {
            let extra = match kind {
                TipKind::Info => 32.0,
                TipKind::Refusal => 14.0,
            };
            let w = ((0.max(width - 24)) as f32).min(ui.text_width(None, text) + extra);
            let top = (y + 0.max(height - 12)) as f32 - ((count - index) as f32 * 28.0) + 4.0;
            // a tip that would sit on a rectangle of `avoid` (a graph card) moves up above it
            let left = (x + 12) as f32;
            let mut sorted: Vec<(f32, f32, f32, f32)> = avoid.to_vec();
            sorted.sort_by(|(_, a, _, _), (_, b, _, _)| b.total_cmp(a));
            let mut top = top;
            for (ax, ay, aw, ah) in sorted {
                if left < ax + aw && ax < left + w && top < ay + ah && ay < top + 24.0 {
                    top = ay - 28.0;
                }
            }
            let boxed = ui.box_(
                &format!("echo-tip-{index}"),
                BoxOpts::default().flags(CLIP).w(Size::Px(w)).h(Size::Px(24.0)).at(((x + 12) as f32, top)),
            );
            let kind = *kind;
            let text = text.clone();
            ui.draw(&boxed, move |paint, Rect { x, y, w, h }| {
                paint.fill(x, y, w, h, theme.input);
                paint.frame(x, y, w, h, theme::edge(&theme));
                let ty = paint.text_top(None, y, h);
                match kind {
                    TipKind::Info => {
                        paint.circle((x + 10.0, y + (h / 2.0)), 3.0, Some(crate::kit::port_ink(&theme, crate::kit::Port::Hint)), None);
                        paint.text_c((x + 19.0, ty), theme.foreground, &text);
                    }
                    TipKind::Refusal => paint.text_c((x + 7.0, ty), theme::invalid(), &text),
                }
            });
        }
    }
}
