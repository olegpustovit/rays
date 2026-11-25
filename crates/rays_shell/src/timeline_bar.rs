use crate::kit;
use input::MouseButton;
use pxui::ui::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Intent {
    PauseToggle,
    StopPlayback,
    ResetPlayback,
    SeekPlayback(i64),
    /// the last frame, typed in a tall timeline's End field
    SetEnd(i64),
}

/// The strip of the workspace sheet is one 24-point bar: Play, Stop, F, the frame field, the time
/// and the ruler to the edge. A taller panel has the bar, Reset, the rule, Frame, Time and End, a
/// hairline and the ruler under it. `edge` draws the line-2 hairline above a strip that has no
/// gutter over it.
pub fn draw(ui: &mut Ui, bounds: (i64, i64, i64, i64), edge: bool, playing: bool, frame: i64, time: f64, max_frame: i64) -> Vec<Intent> {
    use std::hash::Hash;
    let revision = kit::revision(&|h| (bounds, edge, playing, frame, time.to_bits(), max_frame).hash(h));
    let mut intents = Vec::new();
    kit::cached(ui, "timeline-view", revision, &mut |ui| {
        intents = build(ui, bounds, edge, playing, frame, time, max_frame);
    });
    intents
}

fn number_field(ui: &mut Ui, key: &str, at: (f32, f32), width: f32, current: &str, default: i64, valid: &dyn Fn(&str) -> bool) -> Option<i64> {
    // clickable: a right-click on the field reaches its menu, not the bar under it
    let owner = ui.node(pxui::hash_of(&("timeline-field-owner", key)), Node::DEFAULT.flags(Flags::CLICKABLE).at(at.0, at.1).w_px(width).h_px(20.0));
    let action = kit::context_menu(ui, &owner, &[
        ("Copy value", true), ("Paste value", true), ("Reset to default", true), ("Type value", true),
    ]).map(|(choice, _)| choice);
    let mut number = None;
    ui.within(&owner, &mut |ui| {
        let opts = pxui::ValueField::DEFAULT.edit(action == Some(3));
        number = pxui::ext::value_field(ui, pxui::hash_of(key), current, Rect::new(0.0, 0.0, width, 20.0), &opts, &pxui::ValueCallbacks::DEFAULT.valid(valid))
            .changed.and_then(|text| text.trim().parse().ok());
    });
    match action {
        Some(0) => { let _ = rays::clipboard::set_text(current); }
        Some(1) => {
            if let Ok(value) = rays::clipboard::get_text() && valid(value.trim()) { number = value.trim().parse().ok(); }
        }
        Some(2) => number = Some(default),
        _ => {}
    }
    number
}

fn build(ui: &mut Ui, bounds: (i64, i64, i64, i64), edge: bool, playing: bool, frame: i64, time: f64, max_frame: i64) -> Vec<Intent> {
    let (x, y, width, height) = bounds;
    let theme = ui.tokens();
    let (fx, fy, fw, fh) = (x as f32, y as f32, width as f32, height as f32);
    let bar = ui.node("workspace-timeline", pxui::Node::DEFAULT.flags(pxui::Flags::CLIP | pxui::Flags::CLICKABLE).w(Size::Px(fw)).h(Size::Px(fh)).at(fx, fy));
    let top = if edge { 1.0 } else { 0.0 };
    ui.canvas(&bar, move |paint, Rect { x, y, w, h }| {
        paint.fill(pxui::Rect::new(x, y, w, h), pxui::Fill::new(theme.panel));
        if edge {
            paint.fill(pxui::Rect::new(x, y, w, 1.0), pxui::Fill::new((theme).foreground.with_alpha(38)));
        }
    });
    let tall = fh - top >= 56.0;
    let bar_h = if tall { 24.0 } else { fh - top };
    let mut out: Vec<Intent> = Vec::new();
    ui.within(&bar, &mut |ui| {
        let cy = top + ((bar_h - 20.0) / 2.0);
        let mut cx = 12.0;
        let button = |ui: &mut Ui, cx: &mut f32, key: &str, icon: Option<kit::Icon>, hint: Option<&'static str>, active: bool, label: &str| -> bool {
            let w = kit::button_width(ui, hint, icon.is_some(), label);
            let opts = kit::ButtonOpts { icon, hint: hint.map(Into::into), active, ..kit::ButtonOpts::default() };
            let clicked = kit::button(ui, key, (*cx, cy), w, &opts, label);
            *cx = *cx + w + 8.0;
            clicked
        };
        let pause = button(ui, &mut cx, "timeline-play", Some(kit::Icon::Play), if tall { Some("Space") } else { None }, playing, "Play");
        let stop = button(ui, &mut cx, "timeline-stop", Some(kit::Icon::Stop), None, false, "Stop");
        let reset = tall && button(ui, &mut cx, "timeline-reset", None, None, false, "Reset");
        // the rule between the buttons and the fields: 4 points of margin each side
        let rule_x = cx + 4.0;
        if tall {
            cx = cx + 4.0 + 1.0 + 4.0 + 8.0;
        }
        let label = |ui: &mut Ui, cx: &mut f32, text: &str| -> f32 {
            let at = *cx;
            *cx = *cx + kit::cap_width(ui, text) + 8.0;
            at
        };
        let frame_label = label(ui, &mut cx, if tall { "Frame" } else { "F" });
        let field_w = if tall { 64.0 } else { 48.0 };
        let field_x = cx;
        let valid_frame = |text: &str| text.trim().parse::<i64>().is_ok();
        let mut typed = None;
        kit::with_label(format_args!("{frame}"), &mut |current| {
            typed = number_field(ui, "timeline-frame-field", (field_x, cy), field_w, current, 0, &valid_frame);
        });
        cx = cx + field_w + 8.0;
        let time_label = if tall { label(ui, &mut cx, "Time") } else { cx };
        let readout_x = cx;
        kit::with_label(format_args!("{time:.2} s"), &mut |text| {
            cx += ui.text_service().width(text, pxui::Label::BODY, None).unwrap_or(0.0) + 8.0;
        });
        // the last frame, typed, at the end of a tall panel's bar
        let end_x = fw - 12.0 - 64.0;
        let mut ended = None;
        if tall {
            let valid_end = |text: &str| text.trim().parse::<i64>().is_ok_and(|n| n >= 1);
            kit::with_label(format_args!("{max_frame}"), &mut |last| {
                ended = number_field(ui, "timeline-end-field", (end_x, cy), 64.0, last, 240, &valid_end);
            });
        }
        let ruler_x = if tall { 0.0 } else { cx };
        let ruler_y = if tall { top + bar_h + 1.0 } else { top };
        let ruler_w = (fw - ruler_x).max(0.0);
        let ruler_h = fh - ruler_y;
        // the ruler's own line-2 edge is its first column; the ticks divide what is inside it
        let inner_x = if tall { 0.0 } else { 1.0 };
        let span = 1.0f32.max(ruler_w - inner_x);
        let range = (frame as f32).max(max_frame as f32);
        let ruler = ui.node(
            "timeline-scrub",
            pxui::Node::DEFAULT.flags(pxui::Flags::CLICKABLE | pxui::Flags::BLOCKING).at(ruler_x, ruler_y).w(Size::Px(ruler_w)).h(Size::Px(ruler_h)),
        );
        // the ruler has no menu of its own: a right-click on it opens its panel's (or the strip's), and
        // the frame field beside it copies the frame
        let signal = ui.signal(&ruler);
        let scrub: Option<f32> = if signal.button == Some(MouseButton::LeftButton.code() as u8) && (signal.held || signal.released) && ruler_w > 1.0 {
            let px = (if signal.released { signal.release_point } else { signal.pointer }).0;
            let rx = ui.rect(&ruler).x;
            Some((0.0f32.max(1.0f32.min((px - rx - inner_x) / span)) * range).round())
        } else {
            None
        };
        ui.canvas(&bar, move |paint, Rect { x, y, w, h: _ }| {
            let y = y + top;
            let h = bar_h;
            let cy = kit::cap_y_paint(paint, y, h);
            { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_chars(pxui::Point::new(x + frame_label, cy), style, if tall { "Frame" } else { "F" }); };
            if tall {
                { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_chars(pxui::Point::new(x + time_label, cy), style, "Time"); };
                let end_style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08);
                let ew = paint.text_service().width("End", end_style, None).unwrap_or(0.0);
                { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_chars(pxui::Point::new(x + end_x - 8.0 - ew, cy), style, "End"); };
                paint.fill(pxui::Rect::new(x + rule_x, y + ((h - 12.0) / 2.0), 1.0, 12.0), pxui::Fill::new((theme).foreground.with_alpha(77)));
                paint.fill(pxui::Rect::new(x, y + bar_h, w, 1.0), pxui::Fill::new((theme).foreground.with_alpha(38)));
            }
            let ty = (y) + (((h) - (paint.tokens().font_size) as f32) / 2.0).floor();
            let ink = if tall { theme.foreground } else { theme.ink_2() };
            kit::with_label(format_args!("{time:.2} s"), &mut |text| {
                let span = pxui::TextSpan::new(0, text.len() as u32).color(ink);
                let _ = paint.text_spans(pxui::Point::new(x + readout_x, ty), pxui::Label::BODY, 0.0, text, &[span]);
            });
        });
        let cap = kit::cap_size(ui);
        let tick_labels: [String; 10] = if tall {
            std::array::from_fn(|tick| ((range * (tick * 10) as f32 / 100.0).round() as i64).to_string())
        } else {
            std::array::from_fn(|_| String::new())
        };
        ui.canvas(&ruler, move |paint, Rect { x, y, w, h }| {
            paint.fill(Rect::new(x, y, w, h), pxui::Fill::new(theme.input));
            if !tall {
                paint.fill(pxui::Rect::new(x, y, 1.0, h), pxui::Fill::new((theme).foreground.with_alpha(38)));
            }
            let left = x + inner_x;
            let at = |fraction: f32| left + (fraction * span).floor();
            let head = at(frame as f32 / 1.0f32.max(range));
            paint.fill(pxui::Rect::new(left, y, head - left, h), pxui::Fill::new((theme).accent.with_alpha(41)));
            // a strip has fifty minor ticks and a major one in five; a taller ruler a tick a
            // hundredth and a major one in ten, numbered
            let minors: i64 = if tall { 100 } else { 50 };
            let every: i64 = if tall { 10 } else { 5 };
            let minor_h = if tall { 6.0 } else { 5.0 };
            let major_h = if tall { 14.0 } else { 10.0 };
            for tick in 0..minors {
                let tx = at(tick as f32 / minors as f32);
                if tick % every == 0 {
                    paint.fill(pxui::Rect::new(tx, y + h - major_h, 1.0, major_h), pxui::Fill::new((theme).foreground.with_alpha(77)));
                    if tall {
                        let cy = kit::cap_y_paint(paint, y + 16.0, 24.0);
                        let text = &tick_labels[(tick / every) as usize];
                        let style = pxui::Label::BODY.size(cap as f32);
                        let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.ink_3());
                        let _ = paint.text_spans(pxui::Point::new(tx + 4.0, cy), style, 0.0, text, &[span]);
                    }
                } else {
                    paint.fill(pxui::Rect::new(tx, y + h - minor_h, 1.0, minor_h), pxui::Fill::new((theme).foreground.with_alpha(38)));
                }
            }
            // the 2-point accent playhead, and the frame as an accent label right of it
            let head = if tall { head - 1.0 } else { head };
            paint.fill(pxui::Rect::new(head, y, 2.0, h), pxui::Fill::new(theme.accent));
            let tag_h = if tall { 24.0 } else { 14.0 };
            let cy = kit::cap_y_paint(paint, y, tag_h);
            let style = pxui::Label::BODY.size((theme.font_size - 2.0).max(8.0)).caps(true).tracking(0.08)
                .color(pxui::theme::ColorToken::ACCENT);
            kit::with_label(format_args!("{frame}"), &mut |text| {
                paint.text_chars(pxui::Point::new(head + 2.0, cy), style, text);
            });
        });
        // a typed frame clamps to the timeline: 0 to the last frame
        if let Some(n) = typed.filter(|&n| n != frame) {
            out.push(Intent::SeekPlayback(0.max(max_frame.min(n))));
        }
        if let Some(n) = ended.filter(|&n| n != max_frame) {
            out.push(Intent::SetEnd(n));
        }
        if pause {
            out.push(Intent::PauseToggle);
        }
        if stop {
            out.push(Intent::StopPlayback);
        }
        if reset {
            out.push(Intent::ResetPlayback);
        }
        if let Some(target) = scrub {
            if target != frame as f32 {
                out.push(Intent::SeekPlayback(target as i64));
            }
        }
    });
    out
}
