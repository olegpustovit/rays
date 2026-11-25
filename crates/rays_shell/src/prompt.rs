//! standard name and searchable-picker modals; hosts interpret
//! the result.
use crate::kit;
use pxui::ui::*;

/// The sheet's button row: 20 points, `Cancel esc` and the primary `accept` with its key,
/// right-aligned 8 from the edge of a window `width` wide (its 1-point edges inside), 4 apart,
/// and 8 under it. A click is the same as the key.
fn buttons(ui: &mut Ui, key: &str, width: f32, accept: &str) -> Pick {
    let row = ui.node(pxui::hash_of(&format!("{key}-buttons")), pxui::Node::DEFAULT.w(Size::Grow).h(Size::Px(28.0)));
    let mut out = Pick::None;
    ui.within(&row, &mut |ui| {
        let primary_w = kit::button_width(ui, Some("\u{21b5}"), false, accept);
        let cancel_w = kit::button_width(ui, Some("esc"), false, "Cancel");
        let primary_x = width - 2.0 - 8.0 - primary_w;
        let cancel_opts = kit::ButtonOpts { hint: Some("esc".into()), ..kit::ButtonOpts::default() };
        let cancel = kit::button(ui, &format!("{key}-cancel"), (primary_x - 4.0 - cancel_w, 0.0), cancel_w, &cancel_opts, "Cancel");
        let submit_opts = kit::ButtonOpts { primary: true, hint: Some("\u{21b5}".into()), ..kit::ButtonOpts::default() };
        let submit = kit::button(ui, &format!("{key}-accept"), (primary_x, 0.0), primary_w, &submit_opts, accept);
        out = if cancel {
            Pick::Cancel
        } else if submit {
            Pick::Submit
        } else {
            Pick::None
        };
    });
    out
}

fn spacer(ui: &mut Ui, key: &str, height: f32) {
    ui.node(pxui::hash_of(key), pxui::Node::DEFAULT.w(Size::Grow).h(Size::Px(height)));
}

/// Confirmation with a safe default. Left and Right move between the choices; Tab switches.
pub fn confirm(ui: &mut Ui, key: &str, title: &str, message: &str, accept: &str, cancel: &str, mut accepting: bool) -> (bool, Pick) {
    let width = ui.view_size().width.min(440.0);
    let mut pick = Pick::None;
    let open = ui.modal(pxui::hash_of(key), &pxui::Overlay::DEFAULT.w_px(width), |ui| {
        if ui.key_pressed(pxui::KeyCode::ArrowLeft, pxui::Mods::NONE) {
            accepting = false;
        } else if ui.key_pressed(pxui::KeyCode::ArrowRight, pxui::Mods::NONE) {
            accepting = true;
        } else if ui.key_pressed(pxui::KeyCode::Tab, pxui::Mods::NONE) {
            accepting = !accepting;
        }
        ui.label(title, pxui::Label::BODY);
        pxui::ext::inspector_message(ui, "confirmation-message", message);
        spacer(ui, "confirmation-gap", 8.0);
        let enter = ui.key_pressed(pxui::KeyCode::Enter, pxui::Mods::NONE);
        ui.row(Node::row().w(Size::Grow).h_px(28.0).pad(4.0).gap(4.0).justify(Justify::End), |ui| {
            let canceled = ui.button("confirmation-safe", cancel, &pxui::Button::DEFAULT.selected(!accepting)).clicked;
            let accepted = ui.button("confirmation-action", accept, &pxui::Button::DEFAULT.selected(accepting)).clicked;
            pick = if canceled || (enter && !accepting) {
                Pick::Cancel
            } else if accepted || enter {
                Pick::Submit
            } else {
                Pick::None
            };
        });
        pxui::ext::footer(ui, None, &[("↵", if accepting { accept } else { cancel }), ("esc", cancel)]);
    });
    (accepting, if open.open { pick } else { Pick::Cancel })
}

/// A name prompt, the sheet's `06`: the title, what is asked in ink-2, the field and the buttons.
pub fn name(ui: &mut Ui, key: &str, title: &str, description: &str, label: &str, query: &str) -> Option<(String, Pick)> {
    let mut out: Option<(String, Pick)> = None;
    let open = ui.modal(pxui::hash_of(key), &pxui::Overlay::DEFAULT.w_px(320.0), |ui: &mut Ui| {
        ui.label(title, pxui::Label::BODY);
        spacer(ui, "prompt-above", 8.0);
        let line = ui.node("prompt-description", pxui::Node::DEFAULT.w(Size::Grow).h(Size::Px(24.0)));
        let theme = ui.tokens();
        let description = description.to_string();
        ui.canvas(&line, move |paint, Rect { x, y, w, h }| {
            let ty = (y) + (((h) - (paint.tokens().font_size) as f32) / 2.0).floor();
            let shown = ellipsis(&mut |s| paint.text_service().width(s, pxui::Label::BODY, None).unwrap_or(0.0), w - 16.0, &description);
            { let text = &shown; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.32)); let _ = paint.text_spans(pxui::Point::new(x + 8.0, ty), pxui::Label::BODY, 0.0, text, &[span]); };
        });
        let mut picked: (String, Pick) = (String::new(), Pick::None);
        ui.column(pxui::Node::column().pad(4.0), |ui| {
            let response = ui.picker("prompt-field", label, query, &pxui::Picker::DEFAULT.slash(false), &mut |_, _| {});
            picked = (response.query.to_owned(), response.pick);
        });
        let (query, result) = picked;
        let clicked = buttons(ui, key, 320.0, "Save");
        out = Some((query, if clicked == Pick::None { result } else { clicked }));
    });
    if open.open { out } else { None }
}

/// A searchable prompt, the sheet's `01` window: the field, the rows in the picker's style
/// (labels cut with an ellipsis), then the hairline and the hint bar with `N of M`; no buttons.
pub fn search(ui: &mut Ui, key: &str, title: &str, label: &str, query: &str, total: usize, fill: &mut dyn FnMut(&str, &mut pxui::PickRows) -> usize) -> Option<(String, Pick)> {
    let mut out: Option<(String, Pick)> = None;
    let open = ui.modal(pxui::hash_of(key), &pxui::Overlay::DEFAULT.w_px(320.0), |ui: &mut Ui| {
        ui.label(title, pxui::Label::BODY);
        let mut matches = 0;
        let response = ui.picker("prompt-search", label, query, &pxui::Picker::DEFAULT, &mut |query, out| {
            matches = fill(query, out);
        });
        let query = response.query.to_owned();
        let result = response.pick;
        let right = format!("{matches} of {total}");
        pxui::ext::footer(ui,
            Some(&right),
            &[("\u{2191}\u{2193}", "move"), ("\u{21b5}", "pick")],
        );
        out = Some((query, result));
    });
    if open.open { out } else { None }
}

/// The macro dialog's state, kept by the host: the macro's name and, for each literal, whether it
/// is a hole and its name.
#[derive(Clone, Debug, PartialEq)]
pub struct Macro {
    pub name: String,
    pub holes: Vec<(bool, String)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MacroResult {
    None,
    Submit,
}

/// A modal listing the literals (a checkbox and a hole name each; the first 12) and the names
/// that always become holes, with the macro name and a Create button. `None` when dismissed
/// (Escape, a press outside).
// ponytail: the first 12 literals only; a template with more is rare and the rest stay copied
// into it
pub fn macro_(ui: &mut Ui, key: &str, title: &str, literals: &[String], free: &[String], m: &Macro) -> Option<(Macro, MacroResult)> {
    let mut out: Option<(Macro, MacroResult)> = None;
    let open = ui.modal(pxui::hash_of(key), &pxui::Overlay::DEFAULT.w_px(480.0), |ui: &mut Ui| {
        // Enter in one of its fields creates the macro, like the button
        let enter = ui.text_input_focused() && ui.key_pressed(pxui::KeyCode::Enter, pxui::Mods::NONE);
        ui.label(title, pxui::Label::BODY);
        pxui::ext::inspector_message(ui,
            &format!("{key}-hint"),
            &if free.is_empty() {
                "Tick the literals that become holes.".to_string()
            } else {
                format!("Holes: ticked literals and {}", free.join(", "))
            },
        );
        let mut holes: Vec<(bool, String)> = Vec::new();
        for (i, (on, name)) in m.holes.iter().enumerate() {
            if i >= 12 || i >= literals.len() {
                holes.push((*on, name.clone()));
            } else {
                let on = *on ^ ui.toggle(i as u64, &format!("{}  {}", i + 1, literals[i]), *on, &pxui::Toggle::DEFAULT).clicked;
                let name = if on { ui.text_field(pxui::hash_of(&("hole", i)), name, &pxui::TextField::DEFAULT.name(&format!("hole {}", i + 1))).changed.unwrap_or(name).to_owned() } else { name.clone() };
                holes.push((on, name));
            }
        }
        let name = ui.text_field("Macro name", &m.name, &pxui::TextField::DEFAULT.name("Macro name")).changed.unwrap_or(&m.name).to_owned();
        let submit = ui.button("create-macro", "Create macro", &pxui::Button::DEFAULT).clicked;
        out = Some((Macro { name, holes }, if submit || enter { MacroResult::Submit } else { MacroResult::None }));
    });
    if open.open { out } else { None }
}
