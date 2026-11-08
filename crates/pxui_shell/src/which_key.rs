//! Port of lib/pxui_shell/which_key.ml: the leader sheet and the key table.
//! Commands are `editor_core::command::Command`; their scope is a kind of panel, `Panel`.
use crate::kit;
use crate::layout;
use editor_core::command;
use editor_core::keymap::{self, Trigger};
use editor_core::panels::Panel;
use input::{Key, Platform};
use pxui::theme;
use pxui::ui::*;
use rays_math::color::T as Color;

pub type Rows = Vec<(String, String)>;
pub type Sections = Vec<(String, Rows)>;

fn first_word(label: &str) -> String {
    label.split(' ').next().unwrap_or("").to_string()
}

fn byte_at(sequence: &str, i: usize) -> String {
    String::from_utf8_lossy(&sequence.as_bytes()[i..i + 1]).to_string()
}

/// One page per typed prefix: a key that continues into longer sequences shows as a "+group" row
/// named by the first word of its first command.
fn page(platform: Platform, keymap: &[command::Command], prefix: &str, scope: Option<&Panel>) -> Rows {
    let mut rows: Rows = Vec::new();
    for command in keymap {
        match &command.trigger {
            Some(Trigger::Leader(sequence))
                if command.scope.as_ref() == scope && sequence.len() > prefix.len() && sequence.starts_with(prefix) =>
            {
                let key = byte_at(sequence, prefix.len());
                if rows.iter().any(|(k, _)| *k == key) {
                    continue;
                }
                let label = if sequence.len() == prefix.len() + 1 {
                    command.label.clone()
                } else {
                    format!("+{}", first_word(&command.label))
                };
                rows.push((key, label));
            }
            Some(Trigger::Chord(key, modifiers)) if prefix.is_empty() && command.scope.as_ref() == scope => {
                rows.push((keymap::label(platform, &Trigger::Chord(*key, modifiers.clone())), command.label.clone()));
            }
            _ => {}
        }
    }
    rows
}

/// The sheet's order of the leader's sections; a category it does not know follows them.
const CATEGORY_ORDER: [&str; 6] = ["Add", "Panel", "Layout", "Go", "Time", "File"];

fn ordered_titles(titles: &[String]) -> Vec<String> {
    let mut out: Vec<String> = CATEGORY_ORDER.iter().filter(|t| titles.iter().any(|x| x == *t)).map(|t| t.to_string()).collect();
    out.extend(titles.iter().filter(|t| !CATEGORY_ORDER.contains(&t.as_str())).cloned());
    out
}

/// The leader's page by category: one-letter continuations only, no chords. A key that continues
/// into longer sequences is "+" and a name from `describe`, else the first word of its first
/// command.
fn by_category(
    keymap: &[command::Command],
    prefix: &str,
    focus: &Panel,
    category: &dyn Fn(&command::Command) -> String,
    describe: &dyn Fn(&str) -> Option<String>,
    order: &dyn Fn(&str) -> Vec<String>,
) -> Sections {
    let mut rows: Vec<(String, String, String)> = Vec::new();
    for command in keymap {
        if let Some(Trigger::Leader(sequence)) = &command.trigger {
            if (command.scope.is_none() || command.scope.as_ref() == Some(focus))
                && sequence.len() > prefix.len()
                && sequence.starts_with(prefix)
            {
                let key = byte_at(sequence, prefix.len());
                let title = category(command);
                if rows.iter().any(|(t, k, _)| *t == title && *k == key) {
                    continue;
                }
                let label = if sequence.len() == prefix.len() + 1 {
                    command.label.clone()
                } else {
                    format!("+{}", describe(&format!("{prefix}{key}")).unwrap_or_else(|| first_word(&command.label)))
                };
                rows.push((title, key, label));
            }
        }
    }
    let mut titles: Vec<String> = Vec::new();
    for (t, _, _) in &rows {
        if !titles.contains(t) {
            titles.push(t.clone());
        }
    }
    let ordered = ordered_titles(&titles);
    // the host's order of a section's keys; keys it does not name keep the keymap's order after them
    let rank = |title: &str, key: &str| -> i64 {
        match order(title).iter().position(|k| k == key) {
            Some(i) => i as i64,
            None => i64::MAX,
        }
    };
    ordered
        .into_iter()
        .map(|title| {
            let mut section: Rows = rows.iter().filter(|(t, _, _)| *t == title).map(|(_, k, l)| (k.clone(), l.clone())).collect();
            section.sort_by_key(|(k, _)| rank(&title, k));
            (title, section)
        })
        .collect()
}

/// Sections of key rows, one to a column, painted by one box. A section is a label in ink-3
/// (`gap` points above it) over rows of 24: the key in ink-3 at the label size, 8 points on, what
/// it does, and a chevron when the key continues into more keys. `cols` columns share `w`.
fn columns(paint: &mut Paint, (x, y, w): (f64, f64, f64), cols: i64, gap: f64, label_ink: Color, sections: &[(String, Rows)]) {
    let theme = paint.theme;
    let row = paint.row_height as f64;
    let cap = 8.max(paint.font_size - 2);
    let col_w = w / (1.max(cols)) as f64;
    for (column, (title, rows)) in sections.iter().enumerate() {
        let cx = x + (column as f64 * col_w);
        let cy = kit::cap_y_paint(paint, y + gap, row);
        paint.cap((cx + 12.0, cy), Some(theme::ink_3(&theme)), title);
        for (k, (key, label)) in rows.iter().enumerate() {
            let rx = cx;
            let ry = y + gap + row + (k as f64 * row);
            let group = label.starts_with('+');
            let label = if group { &label[1..] } else { &label[..] };
            let key_width = paint.text_width(Some(cap), key);
            let ky = kit::cap_y_paint(paint, ry, row);
            paint.text_s((rx + 12.0, ky), cap, theme::ink_3(&theme), key);
            let lx = rx + 12.0 + key_width + 8.0;
            let limit = rx + col_w - lx - if group { 20.0 } else { 12.0 };
            let shown = ellipsis(&mut |s| paint.text_width(None, s), limit, label);
            let ty = paint.text_top(None, ry, row);
            paint.text_c((lx, ty), label_ink, &shown);
            // a key that continues into more keys
            if group {
                paint.chevron((rx + col_w - 17.0, ry + (row / 2.0)), Dir::Right, theme::ink_3(&theme));
            }
        }
    }
}

fn tallest(sections: &[(String, Rows)]) -> usize {
    sections.iter().fold(0, |most, (_, rows)| most.max(rows.len()))
}

fn no_describe(_: &str) -> Option<String> {
    None
}

fn no_order(_: &str) -> Vec<String> {
    Vec::new()
}

/// The leader: a sheet over the status strip, the `/` key and its name over six columns. Hosts
/// that give a `category` get the sections of the sheet (the keys of the leader only); without it
/// the sections are the commands everywhere and those of the focused pane, with the key chords.
pub fn panel(
    ui: &mut Ui,
    category: Option<&dyn Fn(&command::Command) -> String>,
    describe: Option<&dyn Fn(&str) -> Option<String>>,
    order: Option<&dyn Fn(&str) -> Vec<String>>,
    keymap: &[command::Command],
    prefix: &str,
    focus: &Panel,
    focus_name: &str,
) {
    let theme = ui.theme();
    let describe: &dyn Fn(&str) -> Option<String> = describe.unwrap_or(&no_describe);
    let order: &dyn Fn(&str) -> Vec<String> = order.unwrap_or(&no_order);
    let sections: Sections = match category {
        Some(category) => by_category(keymap, prefix, focus, category, describe, order),
        None => vec![
            ("Global".to_string(), page(ui.platform(), keymap, prefix, None)),
            (focus_name.to_string(), page(ui.platform(), keymap, prefix, Some(focus))),
        ]
        .into_iter()
        .filter(|(_, rows)| !rows.is_empty())
        .collect(),
    };
    let (view_w, view_h) = ui.view_size();
    let row = ui.row_height() as f64;
    let cols = 6.max(sections.len() as i64);
    // the edge, 8, the head row, a section label under 4, its rows, 8; never under the sheet's 183
    let height = 183.0f64.max(1.0 + 8.0 + row + 4.0 + row + (tallest(&sections) as f64 * row) + 8.0);
    let y = 0.0f64.max(view_h - layout::STATUS_HEIGHT as f64 - height);
    let leader = if prefix.is_empty() { "/".to_string() } else { format!("/ {prefix}") };
    // build before the body it shields; the host closes it on any key
    let cap = kit::cap_size(ui);
    ui.popup(None, None, false, false, (0.0, y), view_w, height, "leader", &mut |ui| {
        let boxed = ui.box_("leader-sheet", BoxOpts::default().w(Size::Grow).h(Size::Px(height)));
        let sections = sections.clone();
        let leader = leader.clone();
        ui.draw(&boxed, move |paint, (x, y, w, _)| {
            paint.fill(x, y, w, 1.0, theme::border(&theme));
            let y = y + 9.0;
            let ly = kit::cap_y_paint(paint, y, row);
            paint.text_s((x + 12.0, ly), cap, theme::ink_3(&theme), &leader);
            let lw = paint.text_width(Some(cap), &leader);
            paint.cap((x + 20.0 + lw, ly), None, "Leader");
            let hint = "a key continues \u{b7} esc closes";
            let hw = paint.text_width(Some(cap), hint);
            paint.text_s((x + w - 12.0 - hw, ly), cap, theme::ink_2(&theme), hint);
            columns(paint, (x + 16.0, y + row, w - 32.0), cols, 4.0, theme.foreground, &sections);
        });
    });
}

/// The key sheet's sections: every command a key reaches, once per id with all its keys (the Ctrl
/// twins of the Command chords left out), under the section `category` gives it, the leader
/// sheet's sections first.
pub fn sheet_sections(
    platform: Platform,
    category: Option<&dyn Fn(&command::Command) -> String>,
    text_keys: &[(String, Trigger)],
    keymap: &[command::Command],
) -> Sections {
    let keys_category = |_: &command::Command| "Keys".to_string();
    let category: &dyn Fn(&command::Command) -> String = category.unwrap_or(&keys_category);
    let mut commands: Vec<&command::Command> = Vec::new();
    for command in keymap {
        if command.trigger.is_none() || commands.iter().any(|previous| previous.id == command.id) {
            continue;
        }
        commands.push(command);
    }
    let mut titles: Vec<String> = Vec::new();
    for command in &commands {
        let title = category(command);
        if !titles.contains(&title) {
            titles.push(title);
        }
    }
    let mut sections: Sections = ordered_titles(&titles)
        .into_iter()
        .map(|title| {
            let mut rows: Rows = Vec::new();
            for command in &commands {
                if category(command) != title {
                    continue;
                }
                let mut keys: Vec<String> = Vec::new();
                for alias in keymap {
                    if alias.id != command.id {
                        continue;
                    }
                    match &alias.trigger {
                        Some(Trigger::Chord(_, modifiers)) if platform == Platform::Apple && modifiers.contains(&Key::Ctrl) => {}
                        Some(trigger) => keys.push(keymap::label(platform, trigger)),
                        None => {}
                    }
                }
                keys.sort();
                keys.dedup();
                rows.push((keys.join(" / "), command.label.clone()));
            }
            (title, rows)
        })
        .collect();
    if !text_keys.is_empty() {
        sections.push((
            "Text".to_string(),
            text_keys.iter().map(|(label, trigger)| (keymap::label(platform, trigger), label.clone())).collect(),
        ));
    }
    sections
}

/// The Flow key table, the sheet's `05`: a title row with `context` (what has the focus) and a
/// close button, the filter (at rest until typed in), and the sections of `sheet_sections` in
/// three columns. False on dismissal or the close button.
pub fn sheet(
    ui: &mut Ui,
    context: &str,
    category: Option<&dyn Fn(&command::Command) -> String>,
    text_keys: &[(String, Trigger)],
    keymap: &[command::Command],
) -> bool {
    let sections = sheet_sections(ui.platform(), category, text_keys, keymap);
    let mut result: Option<bool> = None;
    let cap = kit::cap_size(ui);
    let context = context.to_string();
    let open = ui.modal(Some(572.0), "guide-keys", &mut |ui| {
        let theme = ui.theme();
        let mut closed = false;
        // the title row: 4 above, then 24: the label, what has the focus (ink-2 at 70 percent, as
        // written) 8 before the close button, which ends 8 from the edge
        let title = ui.box_("keys-title", BoxOpts::default().w(Size::Grow).h(Size::Px(28.0)));
        let ctx = context.clone();
        ui.draw(&title, move |paint, (x, y, w, _)| {
            let ty = kit::cap_y_paint(paint, y + 4.0, 24.0);
            paint.cap((x + 12.0, ty), Some(theme::ink_2(&theme)), "Keys");
            if !ctx.is_empty() {
                let cw = paint.text_width(Some(cap), &ctx);
                paint.text_s((x + w - 36.0 - cw, ty), cap, rays_math::color::with_alpha(theme::ink_2(&theme), 179), &ctx);
            }
        });
        ui.within(&title, &mut |ui| {
            let holder = ui.box_("keys-close-holder", BoxOpts::default().w(Size::Px(40.0)).h(Size::Px(24.0)).at((530.0, 4.0)));
            ui.within(&holder, &mut |ui| {
                if ui.button_with("\u{d7}###guide-close", true, true, true, Some(theme::ink_2(&theme))) {
                    closed = true;
                }
            });
        });
        // the filter: what is typed narrows the rows to the commands and keys that match; the
        // query lives with its box, Escape closes the sheet
        let memory = ui.box_("keys-filter-memory", BoxOpts::default().w(Size::Grow).h(Size::Px(0.0)));
        let typed = ui.text_state(&memory).unwrap_or_default();
        let popts = PickerOpts { at_rest: true, ..PickerOpts::default() };
        let (query, pick) = ui.picker(&popts, "filter commands", &typed, &|_| Vec::new());
        ui.set_text_state(&memory, Some(query.clone()));
        let shown: Sections = if query.is_empty() {
            sections.clone()
        } else {
            sections
                .iter()
                .filter_map(|(title, rows)| {
                    let rows: Rows = rows.iter().filter(|(keys, label)| fuzzy_match(&query, label) || fuzzy_match(&query, keys)).cloned().collect();
                    if rows.is_empty() { None } else { Some((title.clone(), rows)) }
                })
                .collect()
        };
        // three columns: each section goes under the shortest column so far
        let rows_of = |s: &(String, Rows)| 1 + s.1.len();
        let height = |stack: &Vec<(String, Rows)>| stack.iter().fold(0, |n, s| n + rows_of(s));
        let mut stacks: Vec<Vec<(String, Rows)>> = vec![Vec::new(), Vec::new(), Vec::new()];
        for section in &shown {
            let mut shortest = 0;
            for i in [1usize, 2] {
                if height(&stacks[i]) < height(&stacks[shortest]) {
                    shortest = i;
                }
            }
            stacks[shortest].push(section.clone());
        }
        let tallest = stacks.iter().fold(0, |most, stack| most.max(height(stack)));
        let row = ui.row_height() as f64;
        let boxed = ui.box_("keys-sheet", BoxOpts::default().w(Size::Grow).h(Size::Px((tallest as f64 * row) + 8.0)));
        ui.draw(&boxed, move |paint, (x, y, w, _)| {
            for (column, stack) in stacks.iter().enumerate() {
                let mut top = y;
                for section in stack {
                    columns(
                        paint,
                        (x + (column as f64 * w / 3.0), top, w / 3.0),
                        1,
                        0.0,
                        theme::ink_2(&theme),
                        std::slice::from_ref(section),
                    );
                    top += rows_of(section) as f64 * row;
                }
            }
        });
        result = Some(pick != Pick::Cancel && !closed);
    });
    open && result.unwrap_or(false)
}
