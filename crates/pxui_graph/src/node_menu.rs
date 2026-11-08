// Port of lib/pxui_graph/node_menu.ml(i): the node menu (leader `/ a`, the sheet's [01]): a search field
// over the kinds, the likeliest first, typed search over the whole catalog by name, key or category.
// Pure state plus one PXUI popup; the host decides what a picked key adds.
use flow_check::op::{self, Op};
use flow_syntax::context::{self, Context};
use flow_syntax::ty::{self, ColorKind, Ty};
use pxui::theme::{self, Theme};
use pxui::ui::{fuzzy_match, Axis, BoxOpts, Pick, PickerOpts, Size, Ui, BLOCKING, CLICKABLE};
use rays_math::color::T as Color;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Entry {
    pub key: String,
    pub label: String,
    pub category: Vec<String>,
    pub arity: i64,
    /// the graph the kind is for: "sop", "value", "scene" ...
    pub context: String,
    /// what the node makes: the colour of its square in the rows
    pub output: Ty,
    /// why the kind cannot be placed in this graph, if it cannot
    pub off: Option<String>,
}

#[derive(Clone)]
struct Item {
    entry: Entry,
    lower_key: String,
    lower_label: String,
    lower_category: String,
}

#[derive(Clone)]
pub struct Menu {
    items: Vec<Item>,
    x: i64,
    y: i64,
    query: String,
    /// the node the new one goes after: the title says so
    after: Option<String>,
}


const MENU_WIDTH: i64 = 320;
const MENU_LIMIT: i64 = 10;

/// Entries for the context's immutable operator declarations.
pub fn of_ops(extra: &[Op], context: &Context) -> Vec<Entry> {
    op::of_context(extra, context)
        .into_iter()
        .filter(|o| o.ctx == *context)
        .map(|o| {
            let tys: Vec<Ty> = o.signature.pos.iter().map(|(_, t)| t.clone()).collect();
            Entry {
                key: format!("={}", o.name),
                label: o.name.clone(),
                category: vec![o.category.clone()],
                arity: 0,
                context: context::name(*context).to_owned(),
                output: (o.out)(&tys).unwrap_or(Ty::Any),
                off: None,
            }
        })
        .collect()
}

pub fn position(menu: &Menu) -> (i64, i64) {
    (menu.x, menu.y)
}

/// A menu open at a screen point (clamped inside the bounds it is drawn in).
pub fn create(after: Option<&str>, x: i64, y: i64, entries: Vec<Entry>) -> Menu {
    let mut seen: HashSet<String> = HashSet::new();
    let mut items = Vec::new();
    for entry in entries {
        if entry.key.trim().is_empty()
            || entry.label.trim().is_empty()
            || entry.category.is_empty()
            || entry.category.iter().any(|p| p.trim().is_empty())
            || entry.arity < 0
            || seen.contains(&entry.key)
        {
            continue;
        }
        seen.insert(entry.key.clone());
        items.push(Item {
            lower_key: entry.key.to_ascii_lowercase(),
            lower_label: entry.label.to_ascii_lowercase(),
            lower_category: entry.category.join(" / ").to_ascii_lowercase(),
            entry,
        });
    }
    Menu { items, x, y, query: String::new(), after: after.map(|s| s.to_owned()) }
}

fn search_rank(query: &str, item: &Item) -> i64 {
    if item.lower_label.starts_with(query) {
        0
    } else if item.lower_label.split(' ').any(|w| w.starts_with(query)) {
        1
    } else if item.lower_label.contains(query) || item.lower_key.contains(query) {
        2
    } else if fuzzy_match(query, &item.lower_label) || fuzzy_match(query, &item.lower_key) {
        3
    } else {
        4
    }
}

/// The rows: every match of the query, or, before anything is typed, every kind that can be placed here.
fn rows(menu: &Menu, query: &str) -> Vec<Entry> {
    if !query.is_empty() {
        let q = query.to_ascii_lowercase();
        // the kinds that cannot be placed here follow the ones that can
        let mut found: Vec<&Item> = menu
            .items
            .iter()
            .filter(|i| fuzzy_match(&q, &i.lower_label) || fuzzy_match(&q, &i.lower_category) || fuzzy_match(&q, &i.lower_key))
            .collect();
        let off = |i: &Item| if i.entry.off.is_none() { 0 } else { 1 };
        found.sort_by(|l, r| {
            off(l)
                .cmp(&off(r))
                .then_with(|| search_rank(&q, l).cmp(&search_rank(&q, r)))
                .then_with(|| l.entry.label.as_bytes().cmp(r.entry.label.as_bytes()))
        });
        found.into_iter().map(|i| i.entry.clone()).collect()
    } else {
        let fits = |e: &Entry| if menu.after.is_some() && e.arity == 0 { 1 } else { 0 };
        // the host lists the graph's own kinds first: a graph's kinds keep that order, by name within
        let mut contexts: Vec<String> = Vec::new();
        for i in &menu.items {
            if !contexts.contains(&i.entry.context) {
                contexts.push(i.entry.context.clone());
            }
        }
        let context = |e: &Entry| contexts.iter().position(|c| *c == e.context).unwrap_or(contexts.len());
        let mut v: Vec<Entry> = menu.items.iter().filter(|i| i.entry.off.is_none()).map(|i| i.entry.clone()).collect();
        v.sort_by(|a, b| fits(a).cmp(&fits(b)).then_with(|| context(a).cmp(&context(b))).then_with(|| a.label.as_bytes().cmp(b.label.as_bytes())));
        v
    }
}

/// What a row says on the right: the graph the kind is for and its category, or why it cannot go here.
fn entry_detail(entry: &Entry) -> String {
    match &entry.off {
        Some(reason) => reason.clone(),
        None => match entry.category.last() {
            Some(last) => format!("{} / {}", entry.context, last.to_ascii_lowercase()),
            None => entry.context.clone(),
        },
    }
}

fn entry_label(entry: &Entry) -> String {
    match entry.off {
        Some(_) => format!("{}/{}", entry.context, entry.key),
        None => entry.label.clone(),
    }
}

/// Resolve a declaration's color role through the current kit palette.
pub fn color(theme: &Theme, role: ColorKind) -> Color {
    let ports = theme::ports(theme);
    match role {
        ColorKind::Geometry => ports.geometry,
        ColorKind::Float => ports.float,
        ColorKind::Int => ports.int,
        ColorKind::Bool => ports.bool,
        ColorKind::Vec3 => ports.vec3,
        ColorKind::Text => ports.text,
        ColorKind::Fn => ports.fn_,
        ColorKind::Record => ports.record,
        ColorKind::Output => ports.output,
        ColorKind::Compound => ports.compound,
    }
}

/// The colour of a type's port: a kind's square here, the ports, squares and wires of the graph pane.
/// A list is its elements' colour.
pub fn port_color(theme: &Theme, ty: &Ty) -> Color {
    let ports = theme::ports(theme);
    match ty {
        Ty::Named(_) => color(theme, ty::color(ty)),
        Ty::Float => ports.float,
        Ty::Int => ports.int,
        Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => ports.vec3,
        Ty::Bool => ports.bool,
        Ty::Text | Ty::Color => ports.text,
        Ty::Fn(_) => ports.fn_,
        Ty::Record(_) => ports.record,
        Ty::List(e) | Ty::Array(e) => port_color(theme, e),
        Ty::Any => ports.output,
    }
}

fn picker_rows(menu: &Menu, query: &str) -> Vec<(String, String)> {
    rows(menu, query).iter().map(|e| (entry_label(e), entry_detail(e))).collect()
}

/// The node menu: returns the menu while it stays open and the key of the entry picked this frame.
pub fn update(menu: Menu, ui: &mut Ui, bounds: (i64, i64, i64, i64)) -> (Option<Menu>, Option<String>) {
    let (bx, by, bw, bh) = bounds;
    let row = ui.row_height();
    let shown = rows(&menu, "").len() as i64;
    // the edge, the title (4 above its row), the search row, the rows and the foot (a hairline under 4,
    // then a bar)
    let height = 2 + (4 + row) + 28 + (MENU_LIMIT.min(shown) * row) + (5 + row);
    let width = MENU_WIDTH;
    let x = bx.max(menu.x.min(bx + bw - width));
    let y = by.max(menu.y.min(by + bh - height));
    let result: Rc<RefCell<Option<(String, Pick)>>> = Rc::new(RefCell::new(None));
    let out = result.clone();
    let m = menu.clone();
    let open = ui.popup(
        Some(theme::border(&ui.theme())),
        None,
        true,
        false,
        (x as f64, y as f64),
        width as f64,
        height as f64,
        "pxui-graph-menu",
        &mut |ui: &mut Ui| {
            let theme = ui.theme();
            // the title row: the label at 12 and, right-aligned 8 from the edge in ink-2 at the label
            // size, the node the new one goes after
            let title = ui.box_("pxui-graph-menu-title", BoxOpts::default().w(Size::Grow).h(Size::Px((4 + row) as f64)));
            let after = m.after.clone();
            ui.draw(&title, move |paint, (x, y, w, h)| {
                let size = paint.label_size();
                let top = paint.text_top(Some(size), y + 4.0, h - 4.0);
                paint.cap((x + 12.0, top), None, "Add node");
                if let Some(name) = &after {
                    let text = format!("after {}", name);
                    let tw = paint.text_width(Some(size), &text);
                    paint.text_s((x + w - 8.0 - tw, top), size, theme::ink_2(&theme), &text);
                }
            });
            // the rows, kept so that a row's square and unavailability are known
            let latest: Rc<RefCell<Vec<Entry>>> = Rc::new(RefCell::new(Vec::new()));
            let (l1, l2, l3) = (latest.clone(), latest.clone(), latest.clone());
            let mut o = PickerOpts::default();
            o.limit = MENU_LIMIT;
            o.mark = Some(Rc::new(move |index: i64| {
                let l = l1.borrow();
                if (index as usize) < l.len() { Some(port_color(&theme, &l[index as usize].output)) } else { None }
            }));
            o.off = Some(Rc::new(move |index: i64| {
                let l = l2.borrow();
                (index as usize) < l.len() && l[index as usize].off.is_some()
            }));
            let mq = m.clone();
            let (query, pick) = ui.picker(&o, "type to search", &m.query, &move |query: &str| {
                *l3.borrow_mut() = rows(&mq, query);
                picker_rows(&mq, query)
            });
            // a cross at the right of the search row clears it
            let clear = ui.box_(
                "pxui-graph-menu-clear",
                BoxOpts::default().flags(CLICKABLE | BLOCKING).w(Size::Px(20.0)).h(Size::Px(20.0)).at(((x + width - 25) as f64, (y + 1 + row + 4 + 4) as f64)),
            );
            if !query.is_empty() {
                let muted = theme::muted(&ui.theme());
                ui.draw(&clear, move |paint, (cx, cy, _, _)| {
                    let ty = paint.text_top(None, cy, 20.0);
                    paint.text_c((cx + 6.5, ty), muted, "\u{d7}");
                });
            }
            let query = if ui.signal(&clear).clicked { String::new() } else { query };
            let right = format!("{} of {}", rows(&m, &query).len(), m.items.len());
            ui.footer(
                Some(&right),
                &[("\u{2191}\u{2193}".to_owned(), "move".to_owned()), ("\u{21b5}".to_owned(), "place".to_owned())],
            );
            *out.borrow_mut() = Some((query, pick));
        },
    );
    let _ = Axis::Column;
    let (next, picked) = if !open {
        (None, None)
    } else {
        match result.borrow_mut().take() {
            None => (None, None),
            Some((query, pick)) => {
                let menu = Menu { query, ..menu };
                match pick {
                    Pick::Cancel => (None, None),
                    Pick::Pick(index) => {
                        let key = rows(&menu, &menu.query)[index as usize].key.clone();
                        (None, Some(key))
                    }
                    _ => (Some(menu), None),
                }
            }
        }
    };
    if next.is_none() {
        ui.dismiss_popup();
    }
    (next, picked)
}

/// The entry keys the query lists, in order.
pub fn keys(menu: &Menu, query: &str) -> Vec<String> {
    rows(menu, query).into_iter().map(|e| e.key).collect()
}
