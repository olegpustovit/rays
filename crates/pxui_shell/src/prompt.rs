//! standard name and searchable-picker modals; hosts interpret
//! the result.
use crate::kit;
use input::Key;
use pxui::theme;
use pxui::ui::*;

/// The sheet's button row: 20 points, `Cancel esc` and the primary `accept` with its key,
/// right-aligned 8 from the edge of a window `width` wide (its 1-point edges inside), 4 apart,
/// and 8 under it. A click is the same as the key.
fn buttons(ui: &mut Ui, key: &str, width: f64, accept: &str) -> Pick {
    let row = ui.box_(&format!("{key}-buttons"), BoxOpts::default().w(Size::Grow).h(Size::Px(28.0)));
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

fn spacer(ui: &mut Ui, key: &str, height: f64) {
    ui.box_(key, BoxOpts::default().w(Size::Grow).h(Size::Px(height)));
}

/// A name prompt, the sheet's `06`: the title, what is asked in ink-2, the field and the buttons.
pub fn name(ui: &mut Ui, key: &str, title: &str, description: &str, label: &str, query: &str) -> Option<(String, Pick)> {
    let mut out: Option<(String, Pick)> = None;
    let open = ui.modal(Some(320.0), key, &mut |ui| {
        ui.label(title);
        spacer(ui, "prompt-above", 8.0);
        let line = ui.box_("prompt-description", BoxOpts::default().w(Size::Grow).h(Size::Px(24.0)));
        let theme = ui.theme();
        let description = description.to_string();
        ui.draw(&line, move |paint, (x, y, w, h)| {
            let ty = paint.text_top(None, y, h);
            let shown = ellipsis(&mut |s| paint.text_width(None, s), w - 16.0, &description);
            paint.text_c((x + 8.0, ty), theme::ink_2(&theme), &shown);
        });
        let mut picked: (String, Pick) = (String::new(), Pick::None);
        ui.col(4.0, "prompt-field", &mut |ui| {
            let opts = PickerOpts { slash: false, ..PickerOpts::default() };
            picked = ui.picker(&opts, label, query, &|_| Vec::new());
        });
        let (query, result) = picked;
        let clicked = buttons(ui, key, 320.0, "Save");
        out = Some((query, if clicked == Pick::None { result } else { clicked }));
    });
    if open { out } else { None }
}

/// A searchable prompt, the sheet's `01` window: the field, the rows in the picker's style
/// (labels cut with an ellipsis), then the hairline and the hint bar with `N of M`; no buttons.
pub fn search(ui: &mut Ui, key: &str, title: &str, label: &str, query: &str, rows: &dyn Fn(&str) -> Vec<(String, String)>) -> Option<(String, Pick)> {
    let mut out: Option<(String, Pick)> = None;
    let open = ui.modal(Some(320.0), key, &mut |ui| {
        ui.label(title);
        let (query, result) = ui.picker(&PickerOpts::default(), label, query, rows);
        let right = format!("{} of {}", rows(&query).len(), rows("").len());
        ui.footer(
            Some(&right),
            &[("\u{2191}\u{2193}".to_string(), "move".to_string()), ("\u{21b5}".to_string(), "pick".to_string())],
        );
        out = Some((query, result));
    });
    if open { out } else { None }
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
    let open = ui.modal(Some(480.0), key, &mut |ui| {
        // Enter in one of its fields creates the macro, like the button
        let enter = ui.text_input_focused() && ui.key_pressed(Key::Enter);
        ui.label(title);
        ui.inspector_message(
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
                let on = ui.toggle(&format!("{}  {}", i + 1, literals[i]), *on);
                let name = if on { ui.text_field(&format!("hole {}", i + 1), name) } else { name.clone() };
                holes.push((on, name));
            }
        }
        let name = ui.text_field("Macro name", &m.name);
        let submit = ui.button("Create macro");
        out = Some((Macro { name, holes }, if submit || enter { MacroResult::Submit } else { MacroResult::None }));
    });
    if open { out } else { None }
}
