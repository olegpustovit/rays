//! parameter rows generated from `Param` metadata, the one
//! inspector path for SOP nodes and plain sketch settings alike.
use crate::kit;
use param::{Effects, FieldView, KindView, Schema, Value};
use pxui::theme;
use pxui::ui::*;
use pxui::Rgba as Color;

#[derive(Clone, Debug, PartialEq)]
pub struct FlowRow {
    pub path: String,
    pub fields: Vec<FieldView>,
    pub shown: bool,
    pub locked: bool,
    pub drive: Option<String>,
    /// Drive readout, or “mixed” for an editable row shared by several nodes.
    pub live: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FlowChange {
    Edited(String, Value),
    EditedVector(String, [f64; 3]),
    Pinned(String, bool),
    Reset(String),
    Expression(String, String),
    Follow(String),
    Reveal(String),
}

/// The optional arguments of `flow_fields`. `pins` draws each row's pin dot (`shown`: filled when
/// the row is on its card, a ring when not), a click on it being `Pinned` under `pin_click`; so is
/// the s key over a row. A choice named in `chips` wears its colour as a small square, on the
/// closed control and in its menu. With `kind_label` the arguments that have no folder go in a
/// first, open section of that name.
#[derive(Clone, Debug)]
pub struct FlowOpts {
    pub expanded: Vec<String>,
    pub width: Option<f32>,
    pub pins: bool,
    pub pin_click: bool,
    pub chips: Vec<(String, Color)>,
    pub kind_label: Option<String>,
    /// the numbers are `kept_drag` fields: their owner follows a drag between builds (`pressed`,
    /// `dragging`, `dragged`), the rows' views kept
    pub kept_drag: bool,
    pub reveal_source: bool,
}

impl Default for FlowOpts {
    fn default() -> FlowOpts {
        FlowOpts { expanded: Vec::new(), width: None, pins: false, pin_click: false, chips: Vec::new(), kind_label: None, kept_drag: false, reveal_source: false }
    }
}

/// The `kept_drag` number the pointer holds, as its press built it: its field's key, its field's
/// name, how it scrubs and shows, the text the scrub starts from and the last text it wrote.
struct Held {
    field: i64,
    name: String,
    kind: param::number::Kind,
    range: Option<(f64, f64)>,
    trail: Option<(String, Color)>,
    origin: String,
    last: String,
    /// pressed in the build `pressed` has not yet been asked about
    fresh: bool,
}

thread_local! {
    // ponytail: one held number per thread (the pointer holds one field); keyed by its field, so
    // a second `Ui` on the thread cannot follow it
    static HELD: std::cell::RefCell<Option<Held>> = const { std::cell::RefCell::new(None) };
}

/// The name of the `kept_drag` number (`FlowChange::Edited`'s) pressed in the rows built since the
/// last call: its owner makes then what a drag of it edits (`dragged`).
pub fn pressed() -> Option<String> {
    HELD.with(|h| h.borrow_mut().as_mut().filter(|h| h.fresh).map(|h| {
        h.fresh = false;
        h.name.clone()
    }))
}

/// The held `kept_drag` number is dragged this frame (`Ui::field_drag`): its rows' views stay kept.
pub fn dragging(ui: &Ui) -> bool {
    HELD.with(|h| h.borrow().as_ref().is_some_and(|h| ui.box_of_key(pxui::Key::Resolved(h.field)).is_some_and(|b| pxui::ext::field_drag(ui, &b).is_some())))
}

/// After a frame of the held number's drag: its drag read as its build reads it, the field patched
/// (also after a build: a kept view's verifying build sees the patch, not a stale revision), and
/// the edit when the value moved past what a build or this wrote (`FlowChange::Edited`'s name and
/// value). `None` when it is not dragged; `Some((false, _))` when a patch cannot show it (build its
/// view next frame).
pub fn dragged(ui: &mut Ui) -> Option<(bool, Option<(String, Value)>)> {
    HELD.with(|h| {
        let mut h = h.borrow_mut();
        let h = h.as_mut()?;
        let field = ui.box_of_key(pxui::Key::Resolved(h.field))?;
        let (_, dx) = pxui::ext::field_drag(ui, &field)?;
        let fine = ui.press_keys(&field).contains(&input::Key::Shift.code());
        let text = param::number::scrub(h.kind, h.range, &h.origin, dx as f64, fine);
        let display = (h.kind == param::number::Kind::Float)
            .then(|| text.parse::<f64>().ok().map(param::number::show)).flatten();
        let style = pxui::ValueField::DEFAULT;
        let style = display.as_deref().map_or(style, |display| style.display(display));
        let style = h.range.and_then(|r| param::number::fraction(r, &text))
            .map_or(style, |fill| style.fraction(fill as f32));
        let opts = h.trail.as_ref().map_or(style, |(unit, ink)| style.trail(unit, *ink));
        let patched = ui.patch_field(&field, &opts, &text);
        if text == h.last {
            return Some((patched, None));
        }
        let value = match h.kind {
            param::number::Kind::Int => text.parse::<i64>().ok().map(Value::Int),
            param::number::Kind::Float => text.parse::<f64>().ok().map(Value::Float),
        };
        h.last = text;
        Some((patched, value.map(|v| (h.name.clone(), v))))
    })
}

/// Sections are one level deep: the top folder of a parameter, never the vector's own folder (its
/// row is one row, in the section it belongs to).
fn top_folder(row: &FlowRow) -> Option<&str> {
    let folder: &[String] = row.fields.first().map_or(&[], |f| &f.folder);
    fn squash(name: &str) -> impl Iterator<Item = char> + '_ { name.chars().filter(|c| *c != ' ').map(|c| c.to_ascii_lowercase()) }
    let folder = match folder.split_last() {
        Some((last, rest)) if squash(last).eq(squash(&row.path)) => rest,
        _ => folder,
    };
    folder.first().map(String::as_str)
}

fn expression(text: &str) -> bool {
    text.starts_with('=') && text.trim().len() > 1
}

fn text_of(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).to_string()
}

/// Everything the row builders share.
struct Ctx {
    width: f32,
    theme: theme::Tokens,
    ink_2: Color,
    ink_3: Color,
    pins: bool,
    pin_click: bool,
    chips: Vec<(String, Color)>,
    kept: bool,
    reveal_source: bool,
    cache_rows: bool,
}

impl Ctx {
    fn chip(&self, choice: &str) -> Option<Color> {
        self.chips.iter().find(|(c, _)| c == choice).map(|(_, color)| *color)
    }
}

fn pin_of(cx: &Ctx, shown: bool) -> Option<bool> {
    if cx.pins { Some(shown) } else { None }
}

/// a click on the dot, or the s key over the row, asks to flip the row's pin
fn pin_change(ui: &mut Ui, cx: &Ctx, boxed: &Handle, path: &str, shown: bool, clicked: bool, edits: Vec<FlowChange>) -> Vec<FlowChange> {
    let key = cx.pin_click && !ui.text_input_focused() && ui.key_pressed(pxui::KeyCode::Char('s'), pxui::Mods::NONE) && ui.hovered(boxed, true);
    let mut out = Vec::new();
    if clicked || key {
        out.push(FlowChange::Pinned(path.to_string(), !shown));
    }
    out.extend(edits);
    out
}

fn action(ui: &mut Ui, cx: &Ctx, key: &str, label: &str, x: f32, y: f32, enabled: bool) -> bool {
    let pin = label == "pin";
    let x = if pin { 6.0 } else { x };
    let w = if pin { 18.0 } else { 20.0 };
    let boxed = ui.node(
        pxui::hash_of(key),
        pxui::Node::DEFAULT.flags(if enabled { pxui::Flags::CLICKABLE | pxui::Flags::TAB_STOP } else { pxui::Flags::NONE }).at(x, y).w(Size::Px(w)).h(Size::Px(20.0)),
    );
    let signal = ui.signal(&boxed);
    let clicked = enabled && signal.clicked;
    if !pin {
        let (hovered, foreground, ink_3) = (signal.hovered, cx.theme.foreground, cx.ink_3);
        let label = label.to_string();
        let cap = kit::cap_size(ui);
        ui.canvas(&boxed, move |paint, Rect { x, y, w, h }| {
            let color = if enabled && hovered { foreground } else { ink_3 };
            if label == "\u{d7}" {
                let cx = x + (w / 2.0);
                let cy = y + (h / 2.0);
                paint.line((cx - 3.5, cy - 3.5), (cx + 3.5, cy + 3.5), pxui::Stroke::new(color));
                paint.line((cx - 3.5, cy + 3.5), (cx + 3.5, cy - 3.5), pxui::Stroke::new(color));
            } else {
                let ty = kit::cap_y_paint(paint, y, h);
                { let text = &label; let span = pxui::TextSpan::new(0, text.len() as u32).color(color); let _ = paint.text_spans(pxui::Point::new(x + 1.0, ty), pxui::Label::BODY.size((cap) as f32), 0.0, text, &[span]); };
            }
        });
    }
    clicked
}

#[allow(clippy::too_many_arguments)]
fn numeric(
    ui: &mut Ui,
    cx: &Ctx,
    kind: param::number::Kind,
    range: Option<(f64, f64)>,
    display: Option<String>,
    text: &str,
    convert: &dyn Fn(&str) -> Option<Value>,
    field: &FieldView,
    path: &str,
    (row, key): (&pxui::Handle, &str),
    ranged: bool,
    edit: bool,
    mixed: bool,
    (x, y, w): (f32, f32, f32),
) -> Vec<FlowChange> {
    // the kit's number field: a drag changes the value from the one it had, by the step of its
    // kind and soft range (a vector's cell has none), so a click without a drag writes nothing
    let range = if ranged { range } else { None };
    let kept = cx.kept && !mixed && !expression(text);
    // a held number scrubs by the range it had at the press: a literal's range follows its value
    // (`num_range`), and a drag measured from the press must not speed up as the value grows
    let held = pxui::ui::key_of(row.box_key, key);
    let scrub_range = if kept { HELD.with(|h| h.borrow().as_ref().filter(|h| h.field == held).map_or(range, |h| h.range)) } else { range };
    let field_text = if mixed { "" } else { text };
    let display = if mixed { Some("mixed") } else { display.as_deref() };
    let style = pxui::ValueField::DEFAULT.kept_drag(kept).edit(edit).left(expression(text));
    let style = display.map_or(style, |display| style.display(display));
    let style = field.unit.as_deref().map_or(style, |unit| style.trail(unit, cx.ink_3));
    let style = range.and_then(|r| param::number::fraction(r, field_text))
        .map_or(style, |fill| style.fraction(fill as f32));
    let scrub = |origin: &str, dx: f32, fine| {
        let base = if mixed { text } else { origin };
        let next = param::number::scrub(kind, scrub_range, base, f64::from(dx), fine);
        if origin.is_empty() && next == base { String::new() } else { next }
    };
    let valid = |t: &str| param::number::valid(kind, t) || expression(t);
    let callbacks = pxui::ValueCallbacks::DEFAULT.valid(&valid).scrub(&scrub);
    let changed = pxui::ext::value_field(ui, Key::Resolved(held), field_text, Rect::new(x, y, w, 20.0), &style, &callbacks)
        .changed.map(str::to_owned);
    if kept {
        // a press holds the number: its owner follows its drag while its view stays kept; the
        // release (or any build of it not held) lets it go
        let (pressed, holding) = ui.box_of_key(pxui::Key::Resolved(held)).map(|b| ui.signal(&b)).map_or((false, false), |s| (s.pressed, s.held));
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            match h.as_mut() {
                _ if pressed => {
                    let (origin, last) = (text.to_owned(), changed.as_deref().unwrap_or(field_text).to_owned());
                    *h = Some(Held { field: held, name: field.name.clone(), kind, range, trail: field.unit.clone().map(|unit| (unit, cx.ink_3)), origin, last, fresh: true });
                }
                Some(mine) if mine.field == held && holding => changed.as_deref().unwrap_or(field_text).clone_into(&mut mine.last),
                Some(mine) if mine.field == held => *h = None,
                _ => {}
            }
        });
    }
    let Some(changed) = changed else { return Vec::new() };
    // The field paints its scrub result immediately. Rebuild once so a host that declines the
    // edit can publish its authoritative value again, even when the row revision stays the same.
    ui.request_frame();
    if expression(&changed) {
        vec![FlowChange::Expression(path.to_string(), changed)]
    } else {
        match convert(&changed) {
            Some(value) => vec![FlowChange::Edited(field.name.clone(), value)],
            None => Vec::new(),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn input(
    ui: &mut Ui,
    cx: &Ctx,
    on_choice: &dyn Fn(&str, &pxui::Handle),
    ranged: bool,
    field: &FieldView,
    (row, path): (&pxui::Handle, &str),
    edit: bool,
    mixed: bool,
    (x, y, w): (f32, f32, f32),
) -> Vec<FlowChange> {
    let key = format!("flow-value-{path}");
    let field_key = Key::Resolved(key_of(row.box_key, &key));
    let theme = cx.theme;
    match (&field.kind, &field.current) {
        (KindView::Integer(range), Value::Int(value)) => numeric(
            ui,
            cx,
            param::number::Kind::Int,
            Some((range.soft_min as f64, range.soft_max as f64)),
            None,
            &value.to_string(),
            &|text| text.parse::<i64>().ok().map(Value::Int),
            field,
            path,
            (row, &key),
            ranged,
            edit,
            mixed,
            (x, y, w),
        ),
        (KindView::Floating(range), Value::Float(value)) => {
            // the text is the value in full, so a scrub starts from what is stored; it is shown short
            numeric(
                ui,
                cx,
                param::number::Kind::Float,
                Some((range.soft_min, range.soft_max)),
                Some(param::number::show(*value)),
                &param::float_text_with_decimal(*value, true),
                &|text| (text).parse::<f64>().ok().map(Value::Float),
                field,
                path,
                (row, &key),
                ranged,
                edit,
                mixed,
                (x, y, w),
            )
        }
        (KindView::Text, Value::Text(bytes)) => {
            let value = if mixed { String::new() } else { text_of(bytes) };
            // an empty group means every element of its owner
            let placeholder = match &field.default {
                Value::Text(d) if d.is_empty() && field.name == "group" => Some("all points".to_string()),
                Value::Text(d) if d.is_empty() && field.name.ends_with("_group") => Some("all".to_string()),
                _ => None,
            };
            let style = pxui::ValueField::DEFAULT.left(true).edit(edit);
            let style = placeholder.as_deref().map_or(style, |text| style.placeholder(text));
            let style = if mixed { style.display("mixed") } else { style };
            let explicit_empty = mixed && ui.key_pressed(pxui::KeyCode::Enter, pxui::Mods::NONE)
                && ui.box_of_key(field_key).is_some_and(|b| ui.text_state(&b).is_some());
            let changed = pxui::ext::value_field(ui, field_key, &value, Rect::new(x, y, w, 20.0), &style, &pxui::ValueCallbacks::DEFAULT).changed;
            changed.or(explicit_empty.then_some("")).map_or_else(Vec::new, |text| {
                vec![FlowChange::Edited(field.name.clone(), Value::Text(text.as_bytes().to_vec().into()))]
            })
        }
        (KindView::Choice(choices), current @ (Value::Choice(_) | Value::Int(_))) => {
            let boxed = ui.node(pxui::hash_of(&key), pxui::Node::DEFAULT.flags(pxui::Flags::CLICKABLE | pxui::Flags::TAB_STOP | pxui::Flags::CLIP).at(x, y).w(Size::Px(w)).h(Size::Px(20.0)));
            let index: usize = match current {
                Value::Int(index) => *index as usize,
                Value::Choice(label) => choices.iter().position(|c| c == label).unwrap_or(0),
                _ => 0,
            };
            on_choice(&field.name, &boxed);
            let just_opened = ui.signal(&boxed).clicked || edit;
            let open = just_opened || ui.state(&boxed, 0) == 1;
            ui.set_state(&boxed, if open { 1 } else { 0 });
            let chip = (!mixed).then(|| cx.chip(&choices[index])).flatten();
            let shown = if mixed { "mixed".to_owned() } else { choices[index].clone() };
            ui.canvas(&boxed, move |paint, Rect { x, y, w, h }| {
                paint.fill(Rect::new(x, y + h - 1.0, w, 1.0), pxui::Fill::new(if open { theme.accent } else { theme.edge() }));
                let pad = match chip {
                    Some(color) => {
                        let rect = Rect::new(x + 2.0, y + 6.0, 8.0, 8.0);
                        paint.fill(rect, pxui::Fill::new(color));
                        paint.stroke(rect, pxui::Stroke::new(theme.foreground.with_alpha(38)));
                        14.0
                    }
                    None => 0.0,
                };
                let ty = (y - 0.5) + (((h) - (paint.tokens().font_size) as f32) / 2.0).floor();
                { let text = &shown; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground); let _ = paint.text_spans(pxui::Point::new(x + 2.0 + pad, ty), pxui::Label::BODY, 0.0, text, &[span]); };
                paint.chevron((x + w - 5.0, y + (h / 2.0)), if open { Dir::Up } else { Dir::Down }, pxui::Stroke::new(theme.foreground));
            });
            if !open || just_opened {
                return Vec::new();
            }
            let Rect { x: bx, y: by, w: bw, h: bh } = ui.rect(&boxed);
            let rows: Vec<_> = choices.iter().enumerate().map(|(i, choice)| {
                let item = MenuItem::new(choice).selected(!mixed && i == index);
                cx.chip(choice).map_or(item, |ink| item.swatch(ink))
            }).collect();
            let choice = ui.menu(pxui::hash_of(&(key, "options")), Point::new(bx, by + bh + 1.0), &rows, &Menu::DEFAULT.w_px(bw));
            match (choice.chosen, choice.dismissed) {
                (None, false) => Vec::new(),
                (None, true) => {
                    ui.set_state(&boxed, 0);
                    Vec::new()
                }
                (Some(selected), _) => {
                    ui.set_state(&boxed, 0);
                    if !mixed && selected as usize == index {
                        Vec::new()
                    } else {
                        vec![FlowChange::Edited(
                            field.name.clone(),
                            match current {
                                Value::Int(_) => Value::Int(selected as i64),
                                _ => Value::Choice(choices[selected as usize].clone()),
                            },
                        )]
                    }
                }
            }
        }
        (KindView::Toggle, Value::Bool(value)) if mixed || edit || ui.box_of_key(field_key).is_some_and(|b| ui.text_state(&b).is_some()) => {
            let style = pxui::ValueField::DEFAULT.edit(edit);
            let style = if mixed { style.display("mixed") } else { style };
            let current = if mixed { "" } else if *value { "true" } else { "false" };
            let valid = |s: &str| s.parse::<bool>().is_ok();
            let changed = pxui::ext::value_field(ui, field_key, current, Rect::new(x, y, w, 20.0), &style, &pxui::ValueCallbacks::DEFAULT.valid(&valid)).changed;
            changed.and_then(|text| text.parse::<bool>().ok()).map_or_else(Vec::new, |v| vec![FlowChange::Edited(field.name.clone(), Value::Bool(v))])
        }
        (KindView::Toggle, Value::Bool(value)) => {
            let edited = *value ^ crate::kit::switch(ui, &key, (x, y), *value);
            if edited == *value { Vec::new() } else { vec![FlowChange::Edited(field.name.clone(), Value::Bool(edited))] }
        }
        _ => Vec::new(),
    }
}

fn number_from_drive(row: &FlowRow, text: &str) -> Option<Value> {
    match row.fields.as_slice() {
        [FieldView { kind: KindView::Integer(_), .. }] => text.parse::<i64>().ok().map(Value::Int),
        _ => text.parse::<f64>().ok().filter(|v| v.is_finite()).map(Value::Float),
    }
}

fn drive_edit(row: &FlowRow, text: String) -> Option<FlowChange> {
    if expression(&text) {
        Some(FlowChange::Expression(row.path.clone(), text))
    } else {
        let value = number_from_drive(row, &text)?;
        match row.fields.as_slice() {
            [field] if matches!(field.kind, KindView::Integer(_) | KindView::Floating(_)) => {
                Some(FlowChange::Edited(field.name.clone(), value))
            }
            _ => Some(FlowChange::Expression(row.path.clone(), format!("={text}"))),
        }
    }
}

fn field_text(field: &FieldView) -> String {
    match &field.current {
        Value::Bool(v) => v.to_string(),
        Value::Int(v) => match &field.kind {
            KindView::Choice(options) => options.get(*v as usize).cloned().unwrap_or_default(),
            _ => v.to_string(),
        },
        Value::Float(v) => param::float_text_with_decimal(*v, true),
        Value::Text(v) => text_of(v),
        Value::Choice(v) => v.clone(),
    }
}

fn pasted_value(field: &FieldView, text: String) -> Option<Value> {
    let literal = text.trim();
    match &field.kind {
        KindView::Integer(_) => literal.parse().ok().map(Value::Int),
        KindView::Floating(_) => literal.parse::<f64>().ok().filter(|v| v.is_finite()).map(Value::Float),
        KindView::Toggle => literal.parse().ok().map(Value::Bool),
        KindView::Text => Some(Value::Text(text.into_bytes().into())),
        KindView::Choice(options) => {
            let index = options.iter().position(|s| s == literal)?;
            Some(match field.current {
                Value::Int(_) => Value::Int(index as i64),
                _ => Value::Choice(options[index].clone()),
            })
        }
    }
}

fn vector_text(fields: &[FieldView]) -> String {
    let items: Vec<_> = fields.iter().map(field_text).collect();
    format!("[{}]", items.join(" "))
}

fn pasted_vector(row: &FlowRow, text: &str) -> Option<[f64; 3]> {
    if is_color_3(row) && let Ok(c) = Color::parse(text) {
        let (r, g, b) = (f64::from(c.r) / 255.0, f64::from(c.g) / 255.0, f64::from(c.b) / 255.0);
        return Some([r, g, b]);
    }
    let text = text.strip_prefix('[')?.strip_suffix(']')?;
    let values: Vec<f64> = text.split_whitespace().map(str::parse).collect::<Result<_, _>>().ok()?;
    values.iter().all(|v| v.is_finite()).then_some(())?;
    values.try_into().ok()
}

fn row_menu(ui: &mut Ui, cx: &Ctx, boxed: &Handle, row: &FlowRow, typable: bool) -> (bool, Vec<FlowChange>) {
    let mixed = row.live.as_deref() == Some("mixed");
    let Some((choice, _)) = kit::context_menu(ui, boxed, &[
        ("Copy value", !mixed),
        ("Paste value", true),
        ("Reset to default", true),
        ("Type value", typable),
        ("Reveal in Lisp", cx.reveal_source),
    ]) else { return (false, Vec::new()); };
    let edit = match choice {
        0 => {
            let text = if let Some(source) = &row.drive { source.clone() }
                else if let [field] = row.fields.as_slice() { field_text(field) }
                else { vector_text(&row.fields) };
            let _ = rays::clipboard::set_text(&text);
            None
        }
        1 => rays::clipboard::get_text().ok().and_then(|text| {
            if row.drive.is_some() {
                drive_edit(row, text.trim().to_owned())
            } else if let [field] = row.fields.as_slice() {
                pasted_value(field, text).map(|value| FlowChange::Edited(field.name.clone(), value))
            } else {
                pasted_vector(row, text.trim()).map(|values| FlowChange::EditedVector(row.path.clone(), values))
            }
        }),
        2 if row.drive.is_some() => Some(FlowChange::Reset(row.path.clone())),
        2 => match row.fields.as_slice() {
            [f] => Some(FlowChange::Edited(f.name.clone(), f.default.clone())),
            [a, b, c] => match (&a.default, &b.default, &c.default) {
                (Value::Float(a), Value::Float(b), Value::Float(c)) => Some(FlowChange::EditedVector(row.path.clone(), [*a, *b, *c])),
                _ => None,
            },
            _ => None,
        },
        3 => return (true, Vec::new()),
        4 => Some(FlowChange::Reveal(row.path.clone())),
        _ => None,
    };
    (false, edit.into_iter().collect())
}

fn driven(ui: &mut Ui, cx: &Ctx, row: &FlowRow, title: &str, source: &str) -> Vec<FlowChange> {
    let (path, live, shown) = (row.path.as_str(), row.live.as_deref(), row.shown);
    let (boxed, control_x, control_y, control_w) = pxui::ext::inspector_row(ui, Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    // the cross that removes a drive shows on the hovered row, in place of the live value
    let hovered = ui.hovered(&boxed, true);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        let live = if hovered { None } else { live };
        let key = format!("flow-expression-{path}");
        let key = Key::Resolved(key_of(boxed.box_key, &key));
        let editing = ui.box_of_key(key).is_some_and(|b| ui.text_state(&b).is_some());
        let edits: Vec<FlowChange> = if expression(source) || type_value || editing {
            let style = pxui::ValueField::DEFAULT.edit(type_value)
                .display(source.strip_prefix('=').unwrap_or(source))
                .lead("\u{192}", cx.ink_2)
                .line(kit::port_ink(&cx.theme, kit::Port::Float));
            let style = live.map_or(style, |value| style.trail(value, cx.ink_2));
            let valid = |text: &str| expression(text) || number_from_drive(row, text).is_some();
            pxui::ext::value_field(ui, key, source, Rect::new(control_x, control_y, control_w, 20.0), &style, &pxui::ValueCallbacks::DEFAULT.valid(&valid))
                .changed.and_then(|text| drive_edit(row, text.to_owned())).into_iter().collect()
        } else {
            let link = ui.node(pxui::hash_of(&format!("flow-source-{path}")),
                pxui::Node::DEFAULT
                    .flags(pxui::Flags::CLICKABLE | pxui::Flags::TAB_STOP_MARKED)
                    .at(control_x, control_y)
                    .w(Size::Px(1.0f32.max(control_w - 24.0)))
                    .h(Size::Px(20.0)),
            );
            let followed = ui.signal(&link).clicked;
            let theme = cx.theme;
            let ink_2 = cx.ink_2;
            let source = source.to_string();
            let live = live.map(str::to_owned);
            ui.canvas(&boxed, move |paint, Rect { x, y, w: _, h: _ }| {
                let ty = (y + control_y - 0.5) + (((20.0) - (paint.tokens().font_size) as f32) / 2.0).floor();
                let left = x + control_x + 2.0;
                let right = x + control_x + control_w - 2.0;
                { let text = "\u{2190}"; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.accent); let _ = paint.text_spans(pxui::Point::new(left, ty), pxui::Label::BODY, 0.0, text, &[span]); };
                let source_x = left + paint.text_service().width("\u{2190}", pxui::Label::BODY, None).unwrap_or(0.0) + 6.0;
                let live_w = match &live {
                    Some(v) => paint.text_service().width(v, pxui::Label::BODY, None).unwrap_or(0.0) + 6.0,
                    None => 0.0,
                };
                let size = paint.tokens().font_size;
                let shown = ellipsis(&mut |s| paint.text_service().width(s, pxui::Label::BODY.size((size) as f32), None).unwrap_or(0.0), right - source_x - live_w, &source);
                { let text = &shown; let span = pxui::TextSpan::new(0, text.len() as u32).color(ink_2); let _ = paint.text_spans(pxui::Point::new(source_x, ty), pxui::Label::BODY, 0.0, text, &[span]); };
                if let Some(value) = &live {
                    let vw = paint.text_service().width(value, pxui::Label::BODY, None).unwrap_or(0.0);
                    { let text = value; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground); let _ = paint.text_spans(pxui::Point::new(right - vw, ty), pxui::Label::BODY, 0.0, text, &[span]); };
                }
            });
            if followed { vec![FlowChange::Follow(path.to_string())] } else { Vec::new() }
        };
        let reset = hovered && action(ui, cx, &format!("reset-{path}"), "\u{d7}", cx.width - 32.0, control_y, true);
        let pin = cx.pin_click && action(ui, cx, &format!("pin-{path}"), "pin", 0.0, control_y, true);
        let mut all = Vec::new();
        if reset {
            all.push(FlowChange::Reset(path.to_string()));
        }
        all.extend(edits);
        out.extend(pin_change(ui, cx, &boxed, path, shown, pin, all));
    });
    out
}

fn scalar(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &Handle), row: &FlowRow, title: &str, mixed: bool) -> Vec<FlowChange> {
    let (path, field, shown) = (row.path.as_str(), &row.fields[0], row.shown);
    let (boxed, control_x, control_y, control_w) = pxui::ext::inspector_row(ui, Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        // the label's own extent: its column
        let label_x = pxui::ext::inspector_label_x(ui);
        let label = ui.node(
            "label-edit",
            pxui::Node::DEFAULT.flags(pxui::Flags::CLICKABLE).at(label_x, 2.0).w(Size::Px(1.0f32.max(control_x - label_x - 8.0))).h(Size::Px(20.0)),
        );
        let edit = type_value || ui.signal(&label).double_clicked;
        let edits = input(ui, cx, on_choice, true, field, (&boxed, path), edit, mixed, (control_x, control_y, control_w));
        let pinned = cx.pin_click && action(ui, cx, &format!("pin-{path}"), "pin", 0.0, control_y, true);
        out.extend(pin_change(ui, cx, &boxed, path, shown, pinned, edits));
    });
    out
}

fn has_substr(sub: &str, s: &str) -> bool {
    s.contains(sub)
}

fn is_color_3(row: &FlowRow) -> bool {
    row.fields.len() == 3 && (has_substr("color", &row.path) || row.fields.iter().all(|f| has_substr("color", &f.name)))
}

fn is_color_1(field: &FieldView) -> bool {
    match (&field.kind, &field.current) {
        (KindView::Text, Value::Text(bytes)) => {
            let text = text_of(bytes);
            (text.starts_with('#') || has_substr("color", &field.name)) && Color::parse(&text).is_ok()
        }
        _ => false,
    }
}

fn color_row_3(ui: &mut Ui, cx: &Ctx, row: &FlowRow, title: &str, shown: bool) -> Vec<FlowChange> {
    let path = row.path.clone();
    let to_f = |v: &Value| -> f64 {
        match v {
            Value::Float(x) => *x,
            Value::Int(x) => *x as f64,
            _ => 0.0,
        }
    };
    let fields = &row.fields;
    let (r, g, b) = if fields.len() == 3 { (to_f(&fields[0].current), to_f(&fields[1].current), to_f(&fields[2].current)) } else { (0.0, 0.0, 0.0) };
    let swatch_color = Color::rgb((r * 255.0).round() as u8, (g * 255.0).round() as u8, (b * 255.0).round() as u8);
    let hex_str = swatch_color.to_hex_string();
    let (boxed, control_x, control_y, control_w) = pxui::ext::inspector_row(ui, Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        let text = kit::colour_edit(ui, &path, (control_x, control_y), control_w, swatch_color, &hex_str, type_value);
        let hex_edits: Vec<FlowChange> = if text == hex_str {
            Vec::new()
        } else {
            match Color::parse(&text) {
                Ok(c) => {
                    let (nr, ng, nb) = (f64::from(c.r) / 255.0, f64::from(c.g) / 255.0, f64::from(c.b) / 255.0);
                    vec![FlowChange::EditedVector(path.clone(), [nr, ng, nb])]
                }
                Err(_) => Vec::new(),
            }
        };
        let pin = cx.pin_click && action(ui, cx, &format!("pin-{path}"), "pin", 0.0, control_y, !row.locked);
        let mut all = hex_edits;
        all.extend(pin_change(ui, cx, &boxed, &path, shown, pin, Vec::new()));
        out.extend(all);
    });
    out
}

fn color_row_1(ui: &mut Ui, cx: &Ctx, row: &FlowRow, title: &str) -> Vec<FlowChange> {
    let (path, field, shown) = (row.path.as_str(), &row.fields[0], row.shown);
    let text_val = match &field.current {
        Value::Text(t) => text_of(t),
        _ => "#ffffff".to_string(),
    };
    let swatch_color = Color::parse(&text_val).unwrap_or(Color::WHITE).with_alpha(255);
    let hex_str = swatch_color.to_hex_string();
    let (boxed, control_x, control_y, control_w) = pxui::ext::inspector_row(ui, Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        let text = kit::colour_edit(ui, path, (control_x, control_y), control_w, swatch_color, &hex_str, type_value);
        let hex_edits: Vec<FlowChange> = if text == hex_str {
            Vec::new()
        } else {
            match Color::parse(&text) {
                Ok(_) => vec![FlowChange::Edited(field.name.clone(), Value::Text(text.into_bytes().into()))],
                Err(_) => Vec::new(),
            }
        };
        let pin = cx.pin_click && action(ui, cx, &format!("pin-{path}"), "pin", 0.0, control_y, true);
        let mut all = hex_edits;
        all.extend(pin_change(ui, cx, &boxed, path, shown, pin, Vec::new()));
        out.extend(all);
    });
    out
}

fn row_widget(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &Handle), row: &FlowRow) -> Vec<FlowChange> {
    if !cx.cache_rows {
        return row_content(ui, cx, on_choice, row);
    }
    let revision = kit::debug_revision(&(row, cx.width, cx.pins, cx.pin_click, cx.kept, cx.reveal_source, &cx.chips));
    let mut out = Vec::new();
    kit::with_label(format_args!("flow-row-view-{}", row.path), &mut |label| {
        let key = pxui::hash_of(label);
        let host = ui.node(key, Node::DEFAULT.w(Size::Px(cx.width)).h(Size::Fit));
        ui.within(&host, &mut |ui| {
            ui.view_of(key, &revision, |ui| out = row_content(ui, cx, on_choice, row));
        });
    });
    out
}

fn row_content(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &Handle), row: &FlowRow) -> Vec<FlowChange> {
    let title = if let [field] = row.fields.as_slice() && row.path.starts_with('@') { &field.label } else { &row.path };
    if let Some(source) = &row.drive {
        return driven(ui, cx, row, title, source);
    }
    let mixed = row.live.as_deref() == Some("mixed");
    if row.fields.len() == 1 {
        let field = &row.fields[0];
        return if !mixed && is_color_1(field) {
            color_row_1(ui, cx, row, title)
        } else {
            scalar(ui, cx, on_choice, row, title, mixed)
        };
    }
    if !mixed && is_color_3(row) {
        return color_row_3(ui, cx, row, title, row.shown);
    }
    // a vector: three fields in the control column, 8 between, each with its axis letter
    let (boxed, control_x, control_y, control_w) = pxui::ext::inspector_row(ui, Some(cx.width), pin_of(cx, row.shown), &format!("flow-row-{}", row.path), &title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, row.fields.len() == 3);
    ui.within(&boxed, &mut |ui| {
        let mut edits: Vec<FlowChange> = Vec::new();
        let key = Key::Resolved(key_of(boxed.box_key, "flow-vector-entry"));
        let editing = ui.box_of_key(key).is_some_and(|b| ui.text_state(&b).is_some());
        if type_value || editing {
            let current = if mixed { String::new() } else { vector_text(&row.fields) };
            let style = pxui::ValueField::DEFAULT.edit(type_value).left(true);
            let valid = |text: &str| pasted_vector(row, text.trim()).is_some();
            let response = pxui::ext::value_field(ui, key, &current, Rect::new(control_x, control_y, control_w, 20.0), &style, &pxui::ValueCallbacks::DEFAULT.valid(&valid));
            if let Some(v) = response.changed.and_then(|text| pasted_vector(row, text.trim())) {
                edits.push(FlowChange::EditedVector(row.path.clone(), v));
            }
        } else {
            kit::vector(ui, &boxed, (control_x, control_y), control_w, 0.0, &["x", "y", "z"], &mut |ui, index, x, w| {
                if let Some(field) = row.fields.get(index) {
                    let axis = ["x", "y", "z"][index];
                    edits.extend(input(ui, cx, on_choice, false, field, (&boxed, &format!("{}.{}", row.path, axis)), false, mixed, (x, control_y, w)));
                }
            });
        }
        let pin = cx.pin_click && action(ui, cx, &format!("pin-{}", row.path), "pin", 0.0, control_y, !row.locked);
        let mut all = edits;
        all.extend(pin_change(ui, cx, &boxed, &row.path, row.shown, pin, Vec::new()));
        out.extend(all);
    });
    out
}

fn no_choice(_: &str, _: &pxui::Handle) {}

/// Builds the section header each frame while retaining its content by revision.
pub fn section(ui: &mut Ui, key: &str, revision: u64, expanded: bool, set_expanded: Option<bool>, label: &str, build: &mut dyn FnMut(&mut Ui)) -> bool {
    pxui::ext::inspector_section(ui, key, expanded, set_expanded, label, &mut |ui| {
        let name = if key.starts_with("flow-section-") { "flow-section-content" } else { "content" };
        let content = ui.node("section-body", pxui::Node::DEFAULT.w(pxui::Size::Grow).h(pxui::Size::Fit));
        ui.within(&content, &mut |ui| { ui.view_of(name, &revision, &mut *build); });
    })
}

#[allow(clippy::too_many_arguments)]
fn build(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &pxui::Handle), expanded: &[String], kind_label: Option<&str>, rows: &[FlowRow], folder: &str, out: &mut Vec<FlowChange>) {
    let open = expanded.iter().any(|key| key == folder) || kind_label == Some(folder);
    // the section's revision: its rows as they read (values, live readouts, drives, pins) and how
    // they are laid out
    let mine = |row: &&FlowRow| top_folder(row) == Some(folder) || (kind_label == Some(folder) && top_folder(row).is_none());
    let carry_frame = (!cx.cache_rows).then(|| ui.frame_index());
    let shown = kit::revision(&|h| {
        use std::hash::Hash;
        for row in rows.iter().filter(mine) {
            kit::debug_revision(row).hash(h);
        }
        (cx.width.to_bits(), cx.pins, cx.pin_click, cx.reveal_source, carry_frame, kit::debug_revision(&cx.chips)).hash(h);
    });
    section(ui, &format!("flow-section-{folder}"), shown, open, None, folder, &mut |ui| {
        // The kind's ungrouped rows precede rows explicitly in its folder, as in the old tree.
        if kind_label == Some(folder) {
            for row in rows.iter().filter(|row| top_folder(row).is_none()) {
                out.extend(row_widget(ui, cx, on_choice, row));
            }
        }
        for row in rows.iter().filter(|row| top_folder(row) == Some(folder)) {
            out.extend(row_widget(ui, cx, on_choice, row));
        }
    });
}

/// Responsive rows shared by Flow, scene, World, and compound interface inspectors, laid out as
/// the kit's inspector sheet. `on_choice` is called with the field's name and the box of each
/// choice control, built or not open: a host makes it a drop target. Sections are one level deep
/// (a parameter's top folder; a vector is one row, never a section of its own).
pub fn flow_fields(ui: &mut Ui, o: &FlowOpts, on_choice: Option<&dyn Fn(&str, &pxui::Handle)>, rows: &[FlowRow]) -> Vec<FlowChange> {
    // the rows fill the panel they are built in
    let width = o.width.unwrap_or_else(|| pxui::ext::inspector_width(ui));
    let theme = ui.tokens();
    let cx = Ctx {
        width,
        theme,
        ink_2: theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.32),
        ink_3: theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.521),
        pins: o.pins,
        pin_click: o.pin_click,
        chips: o.chips.clone(),
        kept: o.kept_drag,
        reveal_source: o.reveal_source,
        // A carried material re-reads its drop target even on a kept row.
        cache_rows: on_choice.is_none(),
    };
    let on_choice: &dyn Fn(&str, &pxui::Handle) = on_choice.unwrap_or(&no_choice);
    if rows.is_empty() {
        pxui::ext::inspector_message(ui, "no-parameters", "No parameters");
        return Vec::new();
    }
    let mut out = Vec::new();
    let own = rows.iter().any(|row| top_folder(row).is_none());
    if let Some(label) = o.kind_label.as_deref().filter(|_| own) {
        build(ui, &cx, on_choice, &o.expanded, o.kind_label.as_deref(), rows, label, &mut out);
    } else {
        for row in rows.iter().filter(|row| top_folder(row).is_none()) {
            out.extend(row_widget(ui, &cx, on_choice, row));
        }
    }
    // ponytail: folders are one level deep. Scan this small visible parameter list instead of
    // allocating a per-frame tree; cache folder indices if inspectors grow to thousands of rows.
    for (index, row) in rows.iter().enumerate() {
        let Some(folder) = top_folder(row) else { continue; };
        if (own && o.kind_label.as_deref() == Some(folder)) || rows[..index].iter().any(|row| top_folder(row) == Some(folder)) { continue; }
        build(ui, &cx, on_choice, &o.expanded, o.kind_label.as_deref(), rows, folder, &mut out);
    }
    out
}

/// One inspector row per field inside the current panel; folders become sections, open when their
/// "/"-joined path is in `expanded`; `kept_drag` as `FlowOpts`'. Returns this frame's edits, empty
/// when nothing changed.
pub fn fields(ui: &mut Ui, expanded: &[String], width: Option<f32>, kept_drag: bool, views: Vec<FieldView>) -> Vec<(String, Value)> {
    let rows: Vec<FlowRow> = views
        .into_iter()
        .map(|field| FlowRow { path: field.name.clone(), fields: vec![field], shown: false, locked: true, drive: None, live: None })
        .collect();
    let o = FlowOpts { expanded: expanded.to_vec(), width, kept_drag, ..FlowOpts::default() };
    flow_fields(ui, &o, None, &rows)
        .into_iter()
        .filter_map(|change| match change {
            FlowChange::Edited(name, value) => Some((name, value)),
            _ => None,
        })
        .collect()
}

/// `fields` over a typed record, edits applied through `Schema::apply_all`.
pub fn record(ui: &mut Ui, schema: &Schema, values: &[Value]) -> Result<(Vec<Value>, Effects), String> {
    let views = schema.view(values)?;
    let changes = fields(ui, &[], None, false, views);
    if changes.is_empty() { Ok((values.to_vec(), param::NO_EFFECTS)) } else { schema.apply_all(values, &changes) }
}

/// Builds a header and returns its context action. The header result is copied only as needed.
pub fn header_context<T>(ui: &mut Ui, title: &str, reset: bool, reveal: bool, build: &mut dyn FnMut(&mut Ui) -> T) -> (T, Option<usize>) {
    let owner = ui.node("inspector-header-context", Node::DEFAULT.flags(Flags::CLICKABLE).w(Size::Grow).h(Size::Fit));
    let mut head = None;
    ui.within(&owner, &mut |ui| head = Some(build(ui)));
    let action = kit::context_menu(ui, &owner, &[("Copy value", true), ("Reset to default", reset), ("Reveal in Lisp", reveal)])
        .map(|(choice, _)| choice);
    if action == Some(0) {
        let _ = rays::clipboard::set_text(title);
    }
    (head.expect("inspector header"), action)
}
