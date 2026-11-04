//! Port of lib/pxui_shell/timeline_bar.ml.
use crate::kit;
use input::MouseButton;
use pxui::theme;
use pxui::ui::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Intent {
    Pause_toggle,
    Stop_playback,
    Reset_playback,
    Seek_playback(i64),
    /// the last frame, typed in a tall timeline's End field
    Set_end(i64),
}

fn int_of_string_opt(text: &str) -> Option<i64> {
    kit::int_of_string_opt(kit::ocaml_trim(text))
}

/// The strip of the workspace sheet is one 24-point bar: Play, Stop, F, the frame field, the time
/// and the ruler to the edge. A taller panel has the bar, Reset, the rule, Frame, Time and End, a
/// hairline and the ruler under it. `edge` draws the line-2 hairline above a strip that has no
/// gutter over it.
pub fn draw(ui: &mut Ui, bounds: (i64, i64, i64, i64), edge: bool, playing: bool, frame: i64, time: f64, max_frame: i64) -> Vec<Intent> {
    let (x, y, width, height) = bounds;
    let theme = ui.theme();
    let (fx, fy, fw, fh) = (x as f64, y as f64, width as f64, height as f64);
    let bar = ui.box_("workspace-timeline", BoxOpts::default().flags(CLIP | CLICKABLE).w(Size::Px(fw)).h(Size::Px(fh)).at((fx, fy)));
    let top = if edge { 1.0 } else { 0.0 };
    ui.draw(
        &bar,
        Box::new(move |paint, (x, y, w, h)| {
            paint.fill(x, y, w, h, theme.panel);
            if edge {
                paint.fill(x, y, w, 1.0, theme::edge(&theme));
            }
        }),
    );
    let tall = fh - top >= 56.0;
    let bar_h = if tall { 24.0 } else { fh - top };
    let mut out: Vec<Intent> = Vec::new();
    ui.within(&bar, &mut |ui| {
        let cy = top + ((bar_h - 20.0) / 2.0);
        let mut cx = 12.0;
        let button = |ui: &mut Ui, cx: &mut f64, key: &str, icon: Option<kit::Icon>, hint: Option<&str>, active: bool, label: &str| -> bool {
            let w = kit::button_width(ui, hint, icon.is_some(), label);
            let opts = kit::ButtonOpts { icon, hint: hint.map(|h| h.to_string()), active, ..kit::ButtonOpts::default() };
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
        let label = |ui: &mut Ui, cx: &mut f64, text: &str| -> f64 {
            let at = *cx;
            *cx = *cx + kit::cap_width(ui, text) + 8.0;
            at
        };
        let frame_label = label(ui, &mut cx, if tall { "Frame" } else { "F" });
        let field_w = if tall { 64.0 } else { 48.0 };
        let field_x = cx;
        let current = frame.to_string();
        let valid_frame = |text: &str| int_of_string_opt(text).is_some();
        let typed = ui
            .value_field((field_x, cy), field_w, 20.0, &ValueFieldOpts::default(), &valid_frame, "timeline-frame-field", &current)
            .0;
        cx = cx + field_w + 8.0;
        let time_label = if tall { label(ui, &mut cx, "Time") } else { cx };
        let readout = format!("{} s", cfmt::f(time, 2));
        let readout_x = cx;
        cx = cx + ui.text_width(None, &readout) + 8.0;
        // the last frame, typed, at the end of a tall panel's bar
        let last = max_frame.to_string();
        let end_x = fw - 12.0 - 64.0;
        let valid_end = |text: &str| int_of_string_opt(text).map(|n| n >= 1).unwrap_or(false);
        let ended = if !tall {
            last.clone()
        } else {
            ui.value_field((end_x, cy), 64.0, 20.0, &ValueFieldOpts::default(), &valid_end, "timeline-end-field", &last).0
        };
        let ruler_x = if tall { 0.0 } else { cx };
        let ruler_y = if tall { top + bar_h + 1.0 } else { top };
        let ruler_w = (fw - ruler_x).max(0.0);
        let ruler_h = fh - ruler_y;
        // the ruler's own line-2 edge is its first column; the ticks divide what is inside it
        let inner_x = if tall { 0.0 } else { 1.0 };
        let span = 1.0f64.max(ruler_w - inner_x);
        let range = (frame as f64).max(max_frame as f64);
        let ruler = ui.box_(
            "timeline-scrub",
            BoxOpts::default().flags(CLICKABLE | BLOCKING).at((ruler_x, ruler_y)).w(Size::Px(ruler_w)).h(Size::Px(ruler_h)),
        );
        let signal = ui.signal(&ruler);
        let scrub: Option<f64> = if signal.button == Some(MouseButton::LeftButton) && (signal.held || signal.released) && ruler_w > 1.0 {
            let px = (if signal.released { signal.release_point } else { signal.pointer }).0;
            let rx = ui.rect(&ruler).0;
            Some((0.0f64.max(1.0f64.min((px - rx - inner_x) / span)) * range).round())
        } else {
            None
        };
        ui.draw(
            &bar,
            Box::new(move |paint, (x, y, w, _)| {
                let y = y + top;
                let h = bar_h;
                let cy = kit::cap_y_paint(paint, y, h);
                paint.cap((x + frame_label, cy), None, if tall { "Frame" } else { "F" });
                if tall {
                    paint.cap((x + time_label, cy), None, "Time");
                    let ew = paint.cap_width("End");
                    paint.cap((x + end_x - 8.0 - ew, cy), None, "End");
                    paint.fill(x + rule_x, y + ((h - 12.0) / 2.0), 1.0, 12.0, theme::border(&theme));
                    paint.fill(x, y + bar_h, w, 1.0, theme::edge(&theme));
                }
                let ty = paint.text_top(None, y, h);
                paint.text_c((x + readout_x, ty), if tall { theme.foreground } else { theme::ink_2(&theme) }, &readout);
            }),
        );
        let cap = kit::cap_size(ui);
        ui.draw(
            &ruler,
            Box::new(move |paint, (x, y, w, h)| {
                paint.fill(x, y, w, h, theme.input);
                if !tall {
                    paint.fill(x, y, 1.0, h, theme::edge(&theme));
                }
                let left = x + inner_x;
                let at = |fraction: f64| left + (fraction * span).floor();
                let head = at(frame as f64 / 1.0f64.max(range));
                paint.fill(left, y, head - left, h, theme::tint(&theme));
                // a strip has fifty minor ticks and a major one in five; a taller ruler a tick a
                // hundredth and a major one in ten, numbered
                let minors: i64 = if tall { 100 } else { 50 };
                let every: i64 = if tall { 10 } else { 5 };
                let minor_h = if tall { 6.0 } else { 5.0 };
                let major_h = if tall { 14.0 } else { 10.0 };
                for tick in 0..minors {
                    let tx = at(tick as f64 / minors as f64);
                    if tick % every == 0 {
                        paint.fill(tx, y + h - major_h, 1.0, major_h, theme::border(&theme));
                        if tall {
                            let cy = kit::cap_y_paint(paint, y + 16.0, 24.0);
                            let number = (range * tick as f64 / minors as f64).round() as i64;
                            paint.text_s((tx + 4.0, cy), cap, theme::ink_3(&theme), &number.to_string());
                        }
                    } else {
                        paint.fill(tx, y + h - minor_h, 1.0, minor_h, theme::edge(&theme));
                    }
                }
                // the 2-point accent playhead, and the frame as an accent label right of it
                let head = if tall { head - 1.0 } else { head };
                paint.fill(head, y, 2.0, h, theme.accent);
                let tag_h = if tall { 24.0 } else { 14.0 };
                let cy = kit::cap_y_paint(paint, y, tag_h);
                paint.cap((head + 2.0, cy), Some(theme.accent), &frame.to_string());
            }),
        );
        // a typed frame clamps to the timeline: 0 to the last frame
        if typed != current {
            if let Some(n) = int_of_string_opt(&typed) {
                out.push(Intent::Seek_playback(0.max(max_frame.min(n))));
            }
        }
        if ended != last {
            if let Some(n) = int_of_string_opt(&ended) {
                out.push(Intent::Set_end(n));
            }
        }
        if pause {
            out.push(Intent::Pause_toggle);
        }
        if stop {
            out.push(Intent::Stop_playback);
        }
        if reset {
            out.push(Intent::Reset_playback);
        }
        if let Some(target) = scrub {
            if target != frame as f64 {
                out.push(Intent::Seek_playback(target as i64));
            }
        }
    });
    out
}
