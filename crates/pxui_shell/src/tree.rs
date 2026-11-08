//! Port of lib/pxui_shell/tree.ml: a keyboard-first tree list (WAI-ARIA treeview keys): focus is
//! separate from selection, folds and a filter (keeping ancestors) are view state, rows show
//! indent guides and sticky ancestors, toggle columns paint when dragged, and rows drag with a
//! 4-point dead zone onto before/inside/after drop zones. The host owns rows and selection and
//! applies the intents.
use crate::kit;
use crate::prompt;
use editor_core::command;
use editor_core::keymap::Trigger;
use input::{Key, MouseButton};
use pxui::theme;
use pxui::ui::*;
use rays::frame;
use rays_math::color::T as Color;
use std::collections::BTreeSet;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Row {
    pub id: i64,
    /// depth-first order; a child is one deeper than its parent
    pub depth: i64,
    pub label: String,
    /// muted text after the label, e.g. the kind
    pub detail: String,
    /// a kind letter on its colour
    pub badge: (String, Color),
    /// a muted `↳` row repeating `id` shown elsewhere
    pub link: bool,
    /// dimmed and locked: no rename, drag, or toggles
    pub ghost: bool,
    /// one per column
    pub flags: Vec<bool>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DropAt {
    Before,
    Inside,
    After,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Intent {
    /// the new selection, primary first
    Select(Vec<i64>),
    Flag { ids: Vec<i64>, column: i64, value: bool },
    Move { ids: Vec<i64>, target: i64, drop: DropAt },
    Indent(Vec<i64>),
    Outdent(Vec<i64>),
    Reorder { ids: Vec<i64>, delta: i64 },
    Rename(i64, String),
    /// double-click, or the host's enter key
    Activate(i64),
    /// from the row context menu
    Delete(Vec<i64>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    Up,
    Down,
    Extend_up,
    Extend_down,
    Collapse,
    Expand,
    First,
    Last,
    Indent_rows,
    Outdent_rows,
    Move_up,
    Move_down,
    Rename_row,
    Delete_rows,
    Filter,
    Hide,
    Activate_row,
}

#[derive(Clone, Debug, PartialEq)]
enum Drag {
    Rows { ids: Vec<i64>, moved: bool },
    Paint { column: i64, value: bool, painted: Vec<i64> },
}

/// The list's view state.
#[derive(Clone, Debug)]
pub struct Tree {
    focus: Option<i64>,
    anchor: Option<i64>,
    folded: BTreeSet<i64>,
    filter: Option<String>,
    renaming: Option<(i64, String)>,
    drag: Option<Drag>,
    reveal: bool,
    context: Option<(f64, f64, i64, Vec<i64>)>,
}

pub fn create() -> Tree {
    Tree { focus: None, anchor: None, folded: BTreeSet::new(), filter: None, renaming: None, drag: None, reveal: false, context: None }
}

/// Scroll the focused row into view on the next update.
pub fn reveal(mut t: Tree) -> Tree {
    t.reveal = true;
    t
}

pub fn focused(t: &Tree) -> Option<i64> {
    t.focus
}

/// The filter or rename prompt holds the keyboard.
pub fn editing(t: &Tree) -> bool {
    t.renaming.is_some() || t.filter.is_some()
}

/// One line for crash reports and tests: the focus row, the folds, the filter.
pub fn summary(t: &Tree) -> String {
    format!(
        "focus {}, {} folded, filter {}",
        t.focus.map(|f| f.to_string()).unwrap_or_else(|| "-".to_string()),
        t.folded.len(),
        t.filter.as_ref().map(|f| format!("{f:?}")).unwrap_or_else(|| "-".to_string())
    )
}

/// Arrows, Home/End, Tab/Shift-Tab, Alt-arrows, F2, `/`, `h`, Enter; the host scopes them to its
/// list pane.
pub fn bindings() -> Vec<(command::t, Command)> {
    let key = |id: &str, label: &str, key: Key, action: Command, modifiers: Vec<Key>| -> (command::t, Command) {
        (command::make(Some(Trigger::Chord(key, modifiers)), None, Vec::new(), &format!("list.{id}"), label, 0), action)
    };
    vec![
        key("up", "previous row", Key::ArrowUp, Command::Up, vec![]),
        key("down", "next row", Key::ArrowDown, Command::Down, vec![]),
        key("up", "previous row", Key::KeyChar('k'), Command::Up, vec![]),
        key("down", "next row", Key::KeyChar('j'), Command::Down, vec![]),
        key("extend-up", "extend selection up", Key::ArrowUp, Command::Extend_up, vec![Key::Shift]),
        key("extend-down", "extend selection down", Key::ArrowDown, Command::Extend_down, vec![Key::Shift]),
        key("collapse", "fold / parent", Key::ArrowLeft, Command::Collapse, vec![]),
        key("expand", "unfold / first child", Key::ArrowRight, Command::Expand, vec![]),
        key("first", "first row", Key::Home, Command::First, vec![]),
        key("last", "last row", Key::End, Command::Last, vec![]),
        key("indent", "reparent under previous", Key::Tab, Command::Indent_rows, vec![]),
        key("outdent", "reparent up a level", Key::Tab, Command::Outdent_rows, vec![Key::Shift]),
        key("move-up", "move up", Key::ArrowUp, Command::Move_up, vec![Key::Alt]),
        key("move-down", "move down", Key::ArrowDown, Command::Move_down, vec![Key::Alt]),
        key("rename", "rename", Key::F2, Command::Rename_row, vec![]),
        key("delete", "delete", Key::Delete, Command::Delete_rows, vec![]),
        key("delete", "delete", Key::Backspace, Command::Delete_rows, vec![]),
        key("filter", "filter", Key::KeyChar('s'), Command::Filter, vec![]),
        key("hide", "hide / show", Key::KeyChar('h'), Command::Hide, vec![]),
        key("activate", "open selected in graph", Key::Enter, Command::Activate_row, vec![]),
    ]
}

fn has_children(rows: &[Row], index: usize) -> bool {
    index + 1 < rows.len() && rows[index + 1].depth > rows[index].depth
}

/// Indices of the shown rows: a filter keeps matches and their ancestors (and ignores folds); a
/// folded row hides its descendants.
fn visible(t: &Tree, rows: &[Row]) -> Vec<usize> {
    let count = rows.len();
    let filtering = matches!(&t.filter, Some(f) if !f.is_empty());
    let keep: Vec<bool> = if !filtering {
        vec![true; count]
    } else {
        let query = t.filter.clone().unwrap_or_default();
        let mut keep = vec![false; count];
        let mut stack = vec![0usize; count + 1];
        for (index, row) in rows.iter().enumerate() {
            let depth = row.depth.min(count as i64).max(0) as usize;
            stack[depth] = index;
            if fuzzy_match(&query, &row.label) {
                for level in 0..=depth {
                    keep[stack[level]] = true;
                }
            }
        }
        keep
    };
    let mut shown = Vec::new();
    let mut hidden_below = i64::MAX;
    for (index, row) in rows.iter().enumerate() {
        if row.depth <= hidden_below {
            hidden_below = i64::MAX;
        }
        if hidden_below == i64::MAX && keep[index] {
            shown.push(index);
            if !filtering && t.folded.contains(&row.id) && has_children(rows, index) {
                hidden_below = row.depth;
            }
        }
    }
    shown
}

fn position(rows: &[Row], shown: &[usize], id: i64) -> Option<usize> {
    shown.iter().position(|index| rows[*index].id == id)
}

fn span(rows: &[Row], shown: &[usize], a: usize, b: usize) -> Vec<i64> {
    let lo = a.min(b);
    (0..=(a.max(b) - lo)).map(|offset| rows[shown[lo + offset]].id).collect()
}

fn without(ids: &[i64], id: i64) -> Vec<i64> {
    ids.iter().copied().filter(|x| *x != id).collect()
}

fn cons(id: i64, rest: Vec<i64>) -> Vec<i64> {
    let mut out = vec![id];
    out.extend(rest);
    out
}

pub fn run_command(t: Tree, rows: &[Row], selected: &[i64], command: Command) -> (Tree, Vec<Intent>) {
    let mut t = t;
    let shown = visible(&t, rows);
    let count = shown.len();
    if count == 0 {
        return (t, Vec::new());
    }
    let at = |k: usize| &rows[shown[k]];
    let current = t.focus.and_then(|f| position(rows, &shown, f));
    let clamp = |k: i64| -> usize { 0.max((count as i64 - 1).min(k)) as usize };
    let targets: Vec<i64> = if selected.is_empty() { t.focus.into_iter().collect() } else { selected.to_vec() };
    let mv = |t: &mut Tree, k: i64| -> Vec<Intent> {
        let id = at(clamp(k)).id;
        t.focus = Some(id);
        t.anchor = Some(id);
        t.reveal = true;
        vec![Intent::Select(vec![id])]
    };
    let extend = |t: &mut Tree, k: i64| -> Vec<Intent> {
        let k = clamp(k);
        let anchor = t.anchor.and_then(|a| position(rows, &shown, a)).unwrap_or(k);
        t.focus = Some(at(k).id);
        t.reveal = true;
        let id = at(k).id;
        vec![Intent::Select(cons(id, without(&span(rows, &shown, anchor, k), id)))]
    };
    let cur = current.map(|k| k as i64);
    let intents = match (command, cur) {
        (Command::Up, Some(k)) => mv(&mut t, k - 1),
        (Command::Down, Some(k)) => mv(&mut t, k + 1),
        (Command::Up | Command::Extend_up, None) => mv(&mut t, count as i64 - 1),
        (Command::Down | Command::Extend_down, None) => mv(&mut t, 0),
        (Command::First, _) => mv(&mut t, 0),
        (Command::Last, _) => mv(&mut t, count as i64 - 1),
        (Command::Extend_up, Some(k)) => extend(&mut t, k - 1),
        (Command::Extend_down, Some(k)) => extend(&mut t, k + 1),
        (Command::Collapse, Some(k)) => {
            let k = k as usize;
            let index = shown[k];
            if has_children(rows, index) && !t.folded.contains(&rows[index].id) {
                t.folded.insert(rows[index].id);
                Vec::new()
            } else {
                let mut parent: Option<usize> = None;
                for candidate in (0..k).rev() {
                    if parent.is_none() && at(candidate).depth < at(k).depth {
                        parent = Some(candidate);
                    }
                }
                match parent {
                    Some(p) => mv(&mut t, p as i64),
                    None => Vec::new(),
                }
            }
        }
        (Command::Expand, Some(k)) => {
            let k = k as usize;
            let index = shown[k];
            if t.folded.contains(&rows[index].id) {
                t.folded.remove(&rows[index].id);
                Vec::new()
            } else if has_children(rows, index) {
                mv(&mut t, k as i64 + 1)
            } else {
                Vec::new()
            }
        }
        (Command::Indent_rows, _) => vec![Intent::Indent(targets)],
        (Command::Outdent_rows, _) => vec![Intent::Outdent(targets)],
        (Command::Delete_rows, _) => vec![Intent::Delete(targets)],
        (Command::Move_up, _) => vec![Intent::Reorder { ids: targets, delta: -1 }],
        (Command::Move_down, _) => vec![Intent::Reorder { ids: targets, delta: 1 }],
        (Command::Filter, _) => {
            t.filter = Some(t.filter.clone().unwrap_or_default());
            Vec::new()
        }
        (Command::Rename_row, Some(k)) if !at(k as usize).ghost => {
            t.renaming = Some((at(k as usize).id, at(k as usize).label.clone()));
            Vec::new()
        }
        (Command::Hide, Some(k)) => {
            let value = match at(k as usize).flags.first() {
                Some(shown) => !shown,
                None => true,
            };
            vec![Intent::Flag { ids: targets, column: 0, value }]
        }
        (Command::Activate_row, Some(k)) => vec![Intent::Activate(at(k as usize).id)],
        (Command::Collapse | Command::Expand | Command::Rename_row | Command::Hide | Command::Activate_row, _) => Vec::new(),
    };
    (t, intents)
}

/// a 12-point flag and its 8-point gap
const FLAG_WIDTH: f64 = 20.0;
const INDENT_STEP: f64 = 12.0;

fn ancestors(rows: &[Row], shown: &[usize], k: usize) -> Vec<usize> {
    let mut chain: Vec<usize> = Vec::new();
    let mut depth = rows[shown[k]].depth;
    for candidate in (0..k).rev() {
        if rows[shown[candidate]].depth < depth {
            chain.insert(0, candidate);
            depth = rows[shown[candidate]].depth;
        }
    }
    chain
}

/// the ancestors of the first row in view stay at the top while the list is scrolled
fn sticky_of(rows: &[Row], shown: &[usize], height: f64, scroll: f64) -> Vec<usize> {
    let count = shown.len();
    if count == 0 || scroll <= 0.0 {
        return Vec::new();
    }
    let k = 0.max((count as i64 - 1).min((scroll / height).floor() as i64)) as usize;
    ancestors(rows, shown, k).into_iter().take(3).collect()
}

fn drop_of(offset: f64) -> DropAt {
    if offset < 0.25 {
        DropAt::Before
    } else if offset > 0.75 {
        DropAt::After
    } else {
        DropAt::Inside
    }
}

/// The row list, its toggle columns, and the filter and rename prompts, built inside `Ui::frame`.
/// One box takes every pointer gesture; rows are found from the pointer, so only the visible
/// slice is painted. `title` leads the header, e.g. the level's breadcrumb.
pub fn update(t: Tree, ui: &mut Ui, _frame: &frame::t, bounds: (i64, i64, i64, i64), title: &str, columns: &[String], rows: &[Row], selected: &[i64]) -> (Tree, Vec<Intent>) {
    let mut t = t;
    let theme = ui.theme();
    let height = ui.row_height() as f64;
    let (x, y, w, h) = (bounds.0 as f64, bounds.1 as f64, bounds.2 as f64, bounds.3 as f64);
    let shown = visible(&t, rows);
    let count = shown.len();
    let at = |k: usize| &rows[shown[k]];
    let top = y + height;
    let body = height.max(h - height);
    let mut opts = BoxOpts::default().flags(CLICKABLE | SCROLL | CLIP | BLOCKING).w(Size::Px(w)).h(Size::Px(h)).at((x, y));
    opts = opts.scroll_step(height);
    let boxed = ui.box_("tree", opts);
    let signal = ui.signal(&boxed);
    let mut scroll = ui.scroll_offset(&boxed);
    if t.reveal {
        if let Some(k) = t.focus.and_then(|f| position(rows, &shown, f)) {
            let row_top = k as f64 * height;
            if row_top < scroll {
                scroll = row_top;
            } else if row_top + height > scroll + body {
                scroll = row_top + height - body;
            }
        }
    }
    let scroll = 0.0f64.max(scroll.min(0.0f64.max((count as f64 * height) - body)));
    if t.reveal {
        ui.set_scroll_offset(&boxed, scroll);
    }
    ui.within(&boxed, &mut |ui| {
        ui.box_("tree-content", BoxOpts::default().w(Size::Px(w)).h(Size::Px((count + 1) as f64 * height)));
    });
    let row_at = |py: f64| -> Option<usize> {
        // a sticky row is the row under the pointer, not the one scrolled beneath it
        let slot = ((py - top) / height).floor() as i64;
        let sticky = sticky_of(rows, &shown, height, scroll);
        let hit = if py < top || slot < 0 { None } else { sticky.get(slot as usize).copied() };
        match hit {
            Some(k) => Some(k),
            None => {
                let k = ((py - top + scroll) / height).floor() as i64;
                if py < top || k < 0 || k >= count as i64 { None } else { Some(k as usize) }
            }
        }
    };
    // the last flag ends 12 from the edge, a flag column is a 12-point flag and an 8-point gap
    let columns_x = x + w - 24.0 - (FLAG_WIDTH * 0.max(columns.len() as i64 - 1) as f64);
    let column_at = |px: f64| -> Option<i64> {
        if px < columns_x - 4.0 { None } else { Some(((px - columns_x) / FLAG_WIDTH) as i64) }
    };
    let flag = |k: usize, column: i64| -> bool {
        if column < 0 { false } else { at(k).flags.get(column as usize).copied().unwrap_or(false) }
    };
    let is_selected = |id: i64| selected.contains(&id);
    let left = signal.button == Some(MouseButton::LeftButton);
    let press_keys = ui.press_keys(&boxed);
    let modifier = |key: Key| press_keys.contains(&key);
    let mut intents: Vec<Intent> = Vec::new();
    // Presses: select, start a row drag or a toggle paint.
    if signal.pressed && left {
        match row_at(signal.press_point.1) {
            None => {
                t.drag = None;
                intents = vec![Intent::Select(Vec::new())];
            }
            Some(k) => {
                let row = at(k);
                let px = signal.press_point.0;
                let chevron_x = x + 12.0 + (row.depth as f64 * INDENT_STEP);
                if has_children(rows, shown[k]) && px >= chevron_x - 6.0 && px < chevron_x + 10.0 {
                    if t.folded.contains(&row.id) {
                        t.folded.remove(&row.id);
                    } else {
                        t.folded.insert(row.id);
                    }
                    t.drag = None;
                } else {
                    match column_at(px) {
                        Some(column) if column < row.flags.len() as i64 && !row.ghost => {
                            let value = !flag(k, column);
                            let ids = if is_selected(row.id) { selected.to_vec() } else { vec![row.id] };
                            t.drag = Some(Drag::Paint { column, value, painted: ids.clone() });
                            intents = vec![Intent::Flag { ids, column, value }];
                        }
                        _ => {
                            let selection = if modifier(Key::Shift) {
                                let anchor = t.anchor.and_then(|a| position(rows, &shown, a)).unwrap_or(k);
                                cons(row.id, without(&span(rows, &shown, anchor, k), row.id))
                            } else if input::command_held(ui.platform(), &press_keys) {
                                if is_selected(row.id) { without(selected, row.id) } else { cons(row.id, selected.to_vec()) }
                            } else if is_selected(row.id) {
                                cons(row.id, without(selected, row.id))
                            } else {
                                vec![row.id]
                            };
                            t.focus = Some(row.id);
                            if !modifier(Key::Shift) {
                                t.anchor = Some(row.id);
                            }
                            t.drag = if row.ghost { None } else { Some(Drag::Rows { ids: selection.clone(), moved: false }) };
                            intents = vec![Intent::Select(selection)];
                        }
                    }
                }
            }
        }
    }
    // Right-click: a menu on the row, or on the selection when the row is in it; it never changes
    // the selection.
    if context_clicked(&signal) {
        if let Some(k) = row_at(signal.release_point.1) {
            if !at(k).ghost {
                let id = at(k).id;
                let (cx, cy) = signal.release_point;
                t.context = Some((cx, cy, id, if is_selected(id) { selected.to_vec() } else { vec![id] }));
            }
        }
    }
    if let Some((cx, cy, id, ids)) = t.context.clone() {
        let found = rows.iter().find(|row| row.id == id);
        let shown_flag: Option<bool> = found.and_then(|row| row.flags.first().copied());
        let items = vec![
            ("Enter".to_string(), true),
            ("Rename".to_string(), true),
            ((if shown_flag == Some(false) { "Show" } else { "Hide" }).to_string(), shown_flag.is_some()),
            ("Delete".to_string(), true),
        ];
        match ui.context_menu((cx, cy), &MenuOpts::default(), "tree-context", &items) {
            MenuResult::Open => {}
            MenuResult::Dismiss => t.context = None,
            MenuResult::Pick(0) => {
                t.context = None;
                intents.push(Intent::Activate(id));
            }
            MenuResult::Pick(1) => {
                t.context = None;
                t.renaming = Some((id, found.map(|row| row.label.clone()).unwrap_or_default()));
            }
            MenuResult::Pick(2) => {
                t.context = None;
                intents.push(Intent::Flag { ids, column: 0, value: shown_flag == Some(false) });
            }
            MenuResult::Pick(_) => {
                t.context = None;
                intents.push(Intent::Delete(ids));
            }
        }
    }
    if signal.double_clicked && left {
        if let Some(k) = row_at(signal.pointer.1) {
            intents.push(Intent::Activate(at(k).id));
        }
    }
    // Held gestures: a 4-point dead zone starts a row drag; paint applies the toggle value to each
    // row the pointer passes.
    match t.drag.clone() {
        Some(Drag::Rows { ids, moved }) if signal.held || signal.released => {
            let (px, py) = signal.pointer;
            let (sx, sy) = signal.press_point;
            let moved = moved || (px - sx).hypot(py - sy) >= 4.0;
            if !signal.released {
                t.drag = Some(Drag::Rows { ids, moved });
            } else {
                match row_at(signal.pointer.1) {
                    Some(k) if moved && !ids.contains(&at(k).id) && !at(k).ghost => {
                        let offset = ((py - top + scroll) / height) - k as f64;
                        intents.push(Intent::Move { ids, target: at(k).id, drop: drop_of(offset) });
                    }
                    _ => {}
                }
                t.drag = None;
            }
        }
        Some(Drag::Paint { column, value, painted }) if signal.held || signal.released => {
            let fresh: Vec<i64> = match row_at(signal.pointer.1) {
                Some(k) if !painted.contains(&at(k).id) && !at(k).ghost && column < at(k).flags.len() as i64 && flag(k, column) != value => {
                    vec![at(k).id]
                }
                _ => Vec::new(),
            };
            t.drag = if signal.released {
                None
            } else {
                let mut p = fresh.clone();
                p.extend(painted);
                Some(Drag::Paint { column, value, painted: p })
            };
            if !fresh.is_empty() {
                intents.push(Intent::Flag { ids: fresh, column, value });
            }
        }
        Some(_) => t.drag = None,
        None => {}
    }
    let drop_hint: Option<(usize, DropAt)> = match &t.drag {
        Some(Drag::Rows { moved: true, ids }) => match row_at(signal.pointer.1) {
            Some(k) if !ids.contains(&at(k).id) => {
                let offset = ((signal.pointer.1 - top + scroll) / height) - k as f64;
                Some((k, drop_of(offset)))
            }
            _ => None,
        },
        _ => None,
    };
    {
        let rows_rc: Rc<Vec<Row>> = Rc::new(rows.to_vec());
        let shown = shown.clone();
        let selected = selected.to_vec();
        let columns: Vec<String> = columns.to_vec();
        let title = title.to_string();
        let focus = t.focus;
        let folded = t.folded.clone();
        let filter = t.filter.clone();
        let dragging = t.drag.is_some();
        let signal = signal.clone();
        let slot = boxed.box_slot;
        let hover_pointer = signal.pointer.1;
        let build_scroll = scroll;
        let sticky_rows = rows_rc.clone();
        ui.draw(&boxed, move |paint, _| {
            let rows: &[Row] = &rows_rc;
            let at = |k: usize| &rows[shown[k]];
            let is_selected = |id: i64| selected.contains(&id);
            let scroll = paint.scroll_y[slot];
            let first = 0.max((scroll / height).floor() as i64);
            let last = (count as i64 - 1).min(((scroll + body) / height).ceil() as i64);
            let sticky = sticky_of(&sticky_rows, &shown, height, scroll);
            paint.fill(x, y, w, h, theme.panel);
            let ink_2 = theme::ink_2(&theme);
            let ink_3 = theme::ink_3(&theme);
            // the row under the pointer, sticky rows first (as `row_at` finds it)
            let hover: Option<usize> = if signal.hovered && !dragging {
                let py = hover_pointer;
                let slot_k = ((py - top) / height).floor() as i64;
                let hit = if py < top || slot_k < 0 { None } else { sticky_of(&sticky_rows, &shown, height, build_scroll).get(slot_k as usize).copied() };
                match hit {
                    Some(k) => Some(k),
                    None => {
                        let k = ((py - top + build_scroll) / height).floor() as i64;
                        if py < top || k < 0 || k >= count as i64 { None } else { Some(k as usize) }
                    }
                }
            } else {
                None
            };
            let flag_filter_none = filter.is_none();
            let draw_row = |paint: &mut Paint, k: usize, row_y: f64| {
                let row = at(k);
                let index = shown[k];
                let selected = is_selected(row.id);
                // current (the keyboard cursor) and selected are the control fill; selected adds the
                // accent brackets, drawn once the rows are down
                paint.fill(x, row_y, w, height, theme.panel);
                if selected {
                    paint.fill(x + 6.0, row_y, w - 12.0, height, theme.control);
                } else if focus == Some(row.id) {
                    paint.fill(x, row_y, w, height, theme.control);
                } else if hover == Some(k) {
                    paint.fill(x, row_y, w, height, theme::faint_border(&theme));
                }
                let indent = |level: i64| x + 12.0 + (level as f64 * INDENT_STEP);
                let text_y = paint.text_top(None, row_y, height);
                if has_children(rows, index) {
                    paint.chevron(
                        (indent(row.depth) + 3.0, row_y + (height / 2.0)),
                        if folded.contains(&row.id) && flag_filter_none { Dir::Right } else { Dir::Down },
                        ink_2,
                    );
                }
                // the kind: a letter in the label style, in ink once selected
                let badge_x = indent(row.depth) + 14.0;
                let letter: &str = if row.link { "\u{21b3}" } else { &row.badge.0 };
                let cy = kit::cap_y_paint(paint, row_y, height);
                paint.cap((badge_x, cy), Some(if selected || focus == Some(row.id) { theme.foreground } else { ink_2 }), letter);
                let label_x = badge_x + paint.cap_width(letter) + 8.0;
                let hidden = match row.flags.first() {
                    Some(shown) => !shown,
                    None => false,
                };
                let color = if row.ghost || row.link || hidden { ink_3 } else { theme.foreground };
                paint.text_c((label_x, text_y), color, &row.label);
                if !row.detail.is_empty() {
                    // the detail sits before the flags, where the label leaves room
                    let dw = paint.text_width(None, &row.detail);
                    let dx = columns_x - 8.0 - dw;
                    if dx > label_x + paint.text_width(None, &row.label) + 8.0 {
                        paint.text_c((dx, text_y), ink_2, &row.detail);
                    }
                }
                // Flags: a square for the first column (visible), a round one for the others.
                for (column, value) in row.flags.iter().enumerate() {
                    let cx = columns_x + (column as f64 * FLAG_WIDTH) + 6.0;
                    let cy = row_y + (height / 2.0);
                    paint.flag((cx, cy), column > 0, *value);
                }
            };
            for k in first..=last {
                draw_row(paint, k as usize, top + (k as f64 * height) - scroll);
            }
            // the sheet's brackets: 4 points out of the fill, which is 6 in from the list's edge; after
            // every row, since they reach into the rows above and below, whose ground would cover them
            let mark = |paint: &mut Paint, k: usize, row_y: f64| {
                if is_selected(at(k).id) {
                    paint.brackets(x + 6.0, row_y, w - 12.0, height, 4.0, 8.0, 2.0, theme.accent);
                }
            };
            for k in first..=last {
                mark(paint, k as usize, top + (k as f64 * height) - scroll);
            }
            for (slot, k) in sticky.iter().enumerate() {
                draw_row(paint, *k, top + (slot as f64 * height));
            }
            for (slot, k) in sticky.iter().enumerate() {
                mark(paint, *k, top + (slot as f64 * height));
            }
            if !sticky.is_empty() {
                paint.fill(x, top + (sticky.len() as f64 * height), w, 1.0, theme::edge(&theme));
            }
            if let Some((k, drop)) = drop_hint {
                let row_y = top + (k as f64 * height) - scroll;
                match drop {
                    DropAt::Inside => paint.frame_w(x, row_y, w, height, 2.0, theme.accent),
                    DropAt::Before | DropAt::After => {
                        let line_y = if drop == DropAt::Before { row_y } else { row_y + height };
                        paint.line_w((x, line_y), (x + w, line_y), 2.0, theme.accent);
                    }
                }
            }
            // Header: the level as a section label and the flag columns, or the filter under a prompt.
            paint.fill(x, y, w, height, theme.panel);
            if filter.is_none() {
                let cy = kit::cap_y_paint(paint, y, height);
                paint.cap((x + 12.0, cy), Some(ink_3), &title);
                for (column, name) in columns.iter().enumerate() {
                    let nw = paint.cap_width(name);
                    paint.cap((columns_x + (column as f64 * FLAG_WIDTH) + 6.0 - (nw / 2.0), cy), Some(ink_2), name);
                }
            }
        });
    }
    // The filter field sits in the header; Escape clears it.
    if let Some(query) = t.filter.clone() {
        let mut result: (String, Pick) = (query.clone(), Pick::None);
        let popts = PanelOpts::default().at(x, y).width(w).max_height(height + 16.0);
        ui.panel(popts, "tree-filter", &mut |ui| {
            result = ui.picker(&PickerOpts::default(), "Filter rows", &query, &|_| Vec::new());
        });
        match result {
            (_, Pick::Cancel) => {
                ui.unfocus();
                t.filter = None;
            }
            (q, Pick::Submit) | (q, Pick::Pick(_)) => {
                ui.unfocus();
                t.filter = Some(q);
            }
            (q, _) => t.filter = Some(q),
        }
    }
    if let Some((id, name)) = t.renaming.clone() {
        match prompt::name(ui, "tree-rename", "Rename", "A new name for the row", "Name", &name) {
            None | Some((_, Pick::Cancel)) => {
                ui.dismiss_popup();
                t.renaming = None;
            }
            Some((name, Pick::Submit)) => {
                ui.dismiss_popup();
                t.renaming = None;
                let trimmed = name.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c')).to_string();
                if !trimmed.is_empty() {
                    intents.push(Intent::Rename(id, trimmed));
                }
            }
            Some((name, _)) => t.renaming = Some((id, name)),
        }
    }
    t.reveal = false;
    (t, intents)
}
