//! parameter rows generated from `Param` metadata, the one
//! inspector path for SOP nodes and plain sketch settings alike.
use crate::kit;
use param::{Effects, FieldView, KindView, Schema, Value};
use pxui::theme;
use pxui::ui::*;
use pxui::geom::color::{self, T as Color};
use std::rc::Rc;

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
    HELD.with(|h| h.borrow().as_ref().is_some_and(|h| ui.box_of_key(h.field).is_some_and(|b| ui.field_drag(&b).is_some())))
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
        let field = ui.box_of_key(h.field)?;
        let (_, dx) = ui.field_drag(&field)?;
        let fine = ui.press_keys(&field).contains(&input::Key::Shift.code());
        let text = param::number::scrub(h.kind, h.range, &h.origin, dx as f64, fine);
        let opts = ValueFieldOpts {
            display: (h.kind == param::number::Kind::Float)
                .then(|| text.parse::<f64>().ok().map(param::number::show)).flatten(),
            fraction: h.range.and_then(|r| param::number::fraction(r, &text).map(|f| f as f32)),
            trail: h.trail.clone(),
            ..ValueFieldOpts::default()
        };
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
fn pin_change(ui: &mut Ui, cx: &Ctx, boxed: &UiBox, path: &str, shown: bool, clicked: bool, edits: Vec<FlowChange>) -> Vec<FlowChange> {
    let key = cx.pin_click && !ui.text_input_focused() && ui.key_pressed(input::Key::KeyChar('s')) && ui.hovered_within(boxed);
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
    let boxed = ui.box_(
        key,
        BoxOpts::default().flags(if enabled { CLICKABLE | TAB_STOP } else { NO_FLAGS }).at((x, y)).w(Size::Px(w)).h(Size::Px(20.0)),
    );
    let signal = ui.signal(&boxed);
    let clicked = enabled && signal.clicked;
    if !pin {
        let (hovered, foreground, ink_3) = (signal.hovered, cx.theme.foreground, cx.ink_3);
        let label = label.to_string();
        let cap = kit::cap_size(ui);
        ui.draw(&boxed, move |paint, Rect { x, y, w, h }| {
            let color = if enabled && hovered { foreground } else { ink_3 };
            if label == "\u{d7}" {
                let cx = x + (w / 2.0);
                let cy = y + (h / 2.0);
                paint.line((cx - 3.5, cy - 3.5), (cx + 3.5, cy + 3.5), color);
                paint.line((cx - 3.5, cy + 3.5), (cx + 3.5, cy - 3.5), color);
            } else {
                let ty = kit::cap_y_paint(paint, y, h);
                paint.text_s((x + 1.0, ty), cap, color, &label);
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
    (row, key): (&UiBox, &str),
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
    let held = pxui::ui_core::key_of(row.box_key, key);
    let scrub_range = if kept { HELD.with(|h| h.borrow().as_ref().filter(|h| h.field == held).map_or(range, |h| h.range)) } else { range };
    let field_text = if mixed { "" } else { text };
    let mixed_origin = mixed.then(|| text.to_owned());
    let opts = ValueFieldOpts {
        kept_drag: kept,
        display: if mixed { Some("mixed".to_owned()) } else { display },
        edit,
        left: expression(text),
        trail: field.unit.clone().map(|unit| (unit, cx.ink_3)),
        fraction: range.and_then(|r| param::number::fraction(r, field_text).map(|f| f as f32)),
        scrub: Some(Rc::new(move |origin, dx, fine| {
            let base = mixed_origin.as_deref().unwrap_or(origin);
            let next = param::number::scrub(kind, scrub_range, base, dx as f64, fine);
            if origin.is_empty() && next == base { String::new() } else { next }
        })),
        ..ValueFieldOpts::default()
    };
    let valid = move |t: &str| param::number::valid(kind, t) || expression(t);
    let changed = ui.value_field((x, y), w, 20.0, &opts, &valid, key, field_text).0;
    if kept {
        // a press holds the number: its owner follows its drag while its view stays kept; the
        // release (or any build of it not held) lets it go
        let (pressed, holding) = ui.box_of_key(held).map(|b| ui.signal(&b)).map_or((false, false), |s| (s.pressed, s.held));
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            match h.as_mut() {
                _ if pressed => {
                    let (origin, last) = (text.to_owned(), changed.clone());
                    *h = Some(Held { field: held, name: field.name.clone(), kind, range, trail: opts.trail.clone(), origin, last, fresh: true });
                }
                Some(mine) if mine.field == held && holding => mine.last = changed.clone(),
                Some(mine) if mine.field == held => *h = None,
                _ => {}
            }
        });
    }
    if changed == field_text {
        Vec::new()
    } else if expression(&changed) {
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
    on_choice: &dyn Fn(&str, &UiBox),
    ranged: bool,
    field: &FieldView,
    (row, path): (&UiBox, &str),
    edit: bool,
    mixed: bool,
    (x, y, w): (f32, f32, f32),
) -> Vec<FlowChange> {
    let key = format!("flow-value-{path}");
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
            let opts = ValueFieldOpts { placeholder, display: mixed.then(|| "mixed".to_owned()), edit, left: true, ..ValueFieldOpts::default() };
            let focused = mixed && ui.box_of_key(pxui::ui_core::key_of(row.box_key, &key)).is_some_and(|b| ui.focused(&b));
            let (text, _) = ui.value_field((x, y), w, 20.0, &opts, &|_| true, &key, &value);
            let explicit_empty = focused && ui.key_pressed(input::Key::Enter);
            if text == value && !explicit_empty { Vec::new() } else { vec![FlowChange::Edited(field.name.clone(), Value::Text(text.into_bytes().into()))] }
        }
        (KindView::Choice(choices), current @ (Value::Choice(_) | Value::Int(_))) => {
            let boxed = ui.box_(&key, BoxOpts::default().flags(CLICKABLE | TAB_STOP | CLIP).at((x, y)).w(Size::Px(w)).h(Size::Px(20.0)));
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
            ui.draw(&boxed, move |paint, Rect { x, y, w, h }| {
                paint.fill(x, y + h - 1.0, w, 1.0, if open { theme.accent } else { theme::edge(&theme) });
                let pad = match chip {
                    Some(color) => {
                        paint.rect(x + 2.0, y + 6.0, 8.0, 8.0, Some(color), Some(theme::edge(&theme)));
                        14.0
                    }
                    None => 0.0,
                };
                let ty = paint.text_top(None, y - 0.5, h);
                paint.text_c((x + 2.0 + pad, ty), theme.foreground, &shown);
                paint.chevron((x + w - 5.0, y + (h / 2.0)), if open { Dir::Up } else { Dir::Down }, theme.foreground);
            });
            if !open || just_opened {
                return Vec::new();
            }
            let Rect { x: bx, y: by, w: bw, h: bh } = ui.rect(&boxed);
            let opts = MenuOpts {
                width: Some(bw),
                selected: (!mixed).then_some(index as i64),
                swatches: choices.iter().map(|choice| cx.chip(choice)).collect(),
                ..MenuOpts::default()
            };
            let items: Vec<(String, bool)> = choices.iter().map(|choice| (choice.clone(), true)).collect();
            match ui.context_menu((bx, by + bh + 1.0), &opts, &format!("{key}-options"), &items) {
                MenuResult::Open => Vec::new(),
                MenuResult::Dismiss => {
                    ui.set_state(&boxed, 0);
                    Vec::new()
                }
                MenuResult::Pick(selected) => {
                    ui.set_state(&boxed, 0);
                    if !mixed && selected as usize == index {
                        Vec::new()
                    } else {
                        vec![FlowChange::Edited(
                            field.name.clone(),
                            match current {
                                Value::Int(_) => Value::Int(selected),
                                _ => Value::Choice(choices[selected as usize].clone()),
                            },
                        )]
                    }
                }
            }
        }
        (KindView::Toggle, Value::Bool(value)) if mixed || edit || ui.box_of_key(pxui::ui_core::key_of(row.box_key, &key)).is_some_and(|b| ui.focused(&b)) => {
            let options = ValueFieldOpts { display: mixed.then(|| "mixed".to_owned()), edit, ..ValueFieldOpts::default() };
            let current = if mixed { "" } else if *value { "true" } else { "false" };
            let (text, _) = ui.value_field((x, y), w, 20.0, &options, &|s| s.parse::<bool>().is_ok(), &key, current);
            if text == current { Vec::new() } else {
                text.parse::<bool>().ok().map_or_else(Vec::new, |v| vec![FlowChange::Edited(field.name.clone(), Value::Bool(v))])
            }
        }
        (KindView::Toggle, Value::Bool(value)) => {
            let edited = ui.inspector_toggle_value(&key, (x, y), *value);
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
    if is_color_3(row) && let Ok(c) = color::hex(text) {
        let (r, g, b, _) = color::to_floats(c);
        return Some([r, g, b]);
    }
    let text = text.strip_prefix('[')?.strip_suffix(']')?;
    let values: Vec<f64> = text.split_whitespace().map(str::parse).collect::<Result<_, _>>().ok()?;
    values.iter().all(|v| v.is_finite()).then_some(())?;
    values.try_into().ok()
}

fn row_menu(ui: &mut Ui, cx: &Ctx, boxed: &UiBox, row: &FlowRow, typable: bool) -> (bool, Vec<FlowChange>) {
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
    let (boxed, control_x, control_y, control_w) = ui.inspector_row(Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    // the cross that removes a drive shows on the hovered row, in place of the live value
    let hovered = ui.hovered_within(&boxed);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        let live: Option<String> = if hovered { None } else { live.map(|l| l.to_string()) };
        let key = format!("flow-expression-{path}");
        let editing = ui.box_of_key(pxui::ui_core::key_of(boxed.box_key, &key)).is_some_and(|b| ui.focused(&b));
        let edits: Vec<FlowChange> = if expression(source) || type_value || editing {
            let opts = ValueFieldOpts {
                edit: type_value,
                display: Some(source.strip_prefix('=').unwrap_or(source).to_owned()),
                lead: Some(("\u{192}".to_string(), cx.ink_2)),
                trail: live.map(|value| (value, cx.ink_2)),
                line: Some(crate::kit::port_ink(&cx.theme, crate::kit::Port::Float)),
                ..ValueFieldOpts::default()
            };
            let valid = |text: &str| expression(text) || number_from_drive(row, text).is_some();
            let (text, _) = ui.value_field((control_x, control_y), control_w, 20.0, &opts, &valid, &key, source);
            if text == source { Vec::new() } else { drive_edit(row, text).into_iter().collect() }
        } else {
            let link = ui.box_(
                &format!("flow-source-{path}"),
                BoxOpts::default()
                    .flags(CLICKABLE | TAB_STOP_MARKED)
                    .at((control_x, control_y))
                    .w(Size::Px(1.0f32.max(control_w - 24.0)))
                    .h(Size::Px(20.0)),
            );
            let followed = ui.signal(&link).clicked;
            let theme = cx.theme;
            let ink_2 = cx.ink_2;
            let source = source.to_string();
            ui.draw(&boxed, move |paint, Rect { x, y, w: _, h: _ }| {
                let ty = paint.text_top(None, y + control_y - 0.5, 20.0);
                let left = x + control_x + 2.0;
                let right = x + control_x + control_w - 2.0;
                paint.text_c((left, ty), theme.accent, "\u{2190}");
                let source_x = left + paint.text_width(None, "\u{2190}") + 6.0;
                let live_w = match &live {
                    Some(v) => paint.text_width(None, v) + 6.0,
                    None => 0.0,
                };
                let size = paint.font_size;
                let shown = ellipsis(&mut |s| paint.text_width(Some(size), s), right - source_x - live_w, &source);
                paint.text_c((source_x, ty), ink_2, &shown);
                if let Some(value) = &live {
                    let vw = paint.text_width(None, value);
                    paint.text_c((right - vw, ty), theme.foreground, value);
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

fn scalar(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &UiBox), row: &FlowRow, title: &str, mixed: bool) -> Vec<FlowChange> {
    let (path, field, shown) = (row.path.as_str(), &row.fields[0], row.shown);
    let (boxed, control_x, control_y, control_w) = ui.inspector_row(Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        // the label's own extent: its column
        let label_x = ui.inspector_label_x();
        let label = ui.box_(
            "label-edit",
            BoxOpts::default().flags(CLICKABLE).at((label_x, 2.0)).w(Size::Px(1.0f32.max(control_x - label_x - 8.0))).h(Size::Px(20.0)),
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
            (text.starts_with('#') || has_substr("color", &field.name)) && color::hex(&text).is_ok()
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
    let swatch_color = color::of_unit_floats(None, r, g, b);
    let hex_str = color::to_hex_string(swatch_color);
    let (boxed, control_x, control_y, control_w) = ui.inspector_row(Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        let text = kit::colour_edit(ui, &path, (control_x, control_y), control_w, swatch_color, &hex_str, type_value);
        let hex_edits: Vec<FlowChange> = if text == hex_str {
            Vec::new()
        } else {
            match color::hex(&text) {
                Ok(c) => {
                    let (nr, ng, nb, _) = color::to_floats(c);
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
    let c = color::hex(&text_val).unwrap_or(color::white());
    let (r, g, b, _) = color::to_floats(c);
    let swatch_color = color::of_unit_floats(None, r, g, b);
    let hex_str = color::to_hex_string(swatch_color);
    let (boxed, control_x, control_y, control_w) = ui.inspector_row(Some(cx.width), pin_of(cx, shown), &format!("flow-row-{path}"), title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, true);
    ui.within(&boxed, &mut |ui| {
        let text = kit::colour_edit(ui, path, (control_x, control_y), control_w, swatch_color, &hex_str, type_value);
        let hex_edits: Vec<FlowChange> = if text == hex_str {
            Vec::new()
        } else {
            match color::hex(&text) {
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

fn row_widget(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &UiBox), row: &FlowRow) -> Vec<FlowChange> {
    // a row is named by its argument, as the sheet and the card name it
    let title: String = if row.fields.len() == 1 && row.path.starts_with('@') { row.fields[0].label.clone() } else { row.path.clone() };
    if let Some(source) = &row.drive {
        let revision = kit::debug_revision(&(row, cx.width, cx.pins, cx.pin_click, cx.reveal_source));
        let mut out = Vec::new();
        kit::with_label(format_args!("flow-driven-view-{}", row.path), &mut |label| {
            ui.view(label, revision, Size::Px(cx.width), Size::Px(ui.row_height() as f32), &mut |ui| {
                out = driven(ui, cx, row, &title, source);
            });
        });
        return out;
    }
    let mixed = row.live.as_deref() == Some("mixed");
    if row.fields.len() == 1 {
        let field = &row.fields[0];
        return if !mixed && is_color_1(field) {
            color_row_1(ui, cx, row, &title)
        } else {
            scalar(ui, cx, on_choice, row, &title, mixed)
        };
    }
    if !mixed && is_color_3(row) {
        return color_row_3(ui, cx, row, &title, row.shown);
    }
    // a vector: three fields in the control column, 8 between, each with its axis letter
    let (boxed, control_x, control_y, control_w) = ui.inspector_row(Some(cx.width), pin_of(cx, row.shown), &format!("flow-row-{}", row.path), &title);
    let (type_value, mut out) = row_menu(ui, cx, &boxed, row, row.fields.len() == 3);
    ui.within(&boxed, &mut |ui| {
        let mut edits: Vec<FlowChange> = Vec::new();
        let key = "flow-vector-entry";
        let editing = ui.box_of_key(pxui::ui_core::key_of(boxed.box_key, key)).is_some_and(|b| ui.focused(&b));
        if type_value || editing {
            let current = if mixed { String::new() } else { vector_text(&row.fields) };
            let options = ValueFieldOpts { edit: type_value, left: true, ..Default::default() };
            let valid = |text: &str| pasted_vector(row, text.trim()).is_some();
            let (text, _) = ui.value_field((control_x, control_y), control_w, 20.0, &options, &valid, key, &current);
            if text != current && let Some(v) = pasted_vector(row, text.trim()) {
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

fn no_choice(_: &str, _: &UiBox) {}

#[allow(clippy::too_many_arguments)]
fn build(ui: &mut Ui, cx: &Ctx, on_choice: &dyn Fn(&str, &UiBox), expanded: &[String], kind_label: Option<&str>, rows: &[FlowRow], folder: &str, out: &mut Vec<FlowChange>) {
    let open = expanded.iter().any(|key| key == folder) || kind_label == Some(folder);
    // the section's revision: its rows as they read (values, live readouts, drives, pins) and how
    // they are laid out
    let mine = |row: &&FlowRow| top_folder(row) == Some(folder) || (kind_label == Some(folder) && top_folder(row).is_none());
    let shown = kit::revision(&|h| {
        use std::hash::Hash;
        for row in rows.iter().filter(mine) {
            kit::debug_revision(row).hash(h);
        }
        (cx.width.to_bits(), cx.pins, cx.pin_click, cx.reveal_source, kit::debug_revision(&cx.chips)).hash(h);
    });
    ui.inspector_section_view(&format!("flow-section-{folder}"), shown, open, None, folder, &mut |ui| {
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
pub fn flow_fields(ui: &mut Ui, o: &FlowOpts, on_choice: Option<&dyn Fn(&str, &UiBox)>, rows: &[FlowRow]) -> Vec<FlowChange> {
    // the rows fill the panel they are built in
    let width = o.width.unwrap_or_else(|| ui.inspector_width());
    let theme = ui.tokens();
    let cx = Ctx {
        width,
        theme,
        ink_2: theme::ink_2(&theme),
        ink_3: theme::ink_3(&theme),
        pins: o.pins,
        pin_click: o.pin_click,
        chips: o.chips.clone(),
        kept: o.kept_drag,
        reveal_source: o.reveal_source,
    };
    let on_choice: &dyn Fn(&str, &UiBox) = on_choice.unwrap_or(&no_choice);
    if rows.is_empty() {
        ui.inspector_message("no-parameters", "No parameters");
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

/// The header's right-click actions use the same reset request as the button.
pub fn header_context(ui: &mut Ui, title: &str, reset: bool, reveal: bool, build: &mut dyn FnMut(&mut Ui) -> Head) -> (Head, bool) {
    let owner = ui.box_("inspector-header-context", BoxOpts::default().flags(CLICKABLE).w(Size::Grow).h(Size::Fit));
    let mut head = None;
    ui.within(&owner, &mut |ui| head = Some(build(ui)));
    let mut head = head.expect("inspector header");
    let mut reveal_source = false;
    if let Some((choice, _)) = kit::context_menu(ui, &owner, &[("Copy value", true), ("Reset to default", reset), ("Reveal in Lisp", reveal)]) {
        match choice {
            0 => { let _ = rays::clipboard::set_text(title); }
            1 => head.reset_pressed = true,
            2 => reveal_source = true,
            _ => {}
        }
    }
    (head, reveal_source)
}
