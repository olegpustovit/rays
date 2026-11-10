//! Picker, context menu, submenu and tooltip interaction contracts.
use crate::support::*;
use input::{Event, Key, Platform};
use pxui::ui::*;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

fn field_ui() -> Ui {
    new_ui(Some(11))
}

fn with_clipboard(f: &mut dyn FnMut()) {
    crate::steps::with_clipboard(f);
}

pub fn window_pxui_picker() {
    // Picker: typing filters, arrows move the cursor, Enter picks an index of the filtered rows,
    // Delete arms then deletes, Backspace on an empty query goes back, Escape cancels.
    let items = [("alpha", "1"), ("beta", "2"), ("gamma", "3"), ("delta", "4")];
    let rows = |query: &str| -> Vec<(String, String)> {
        items
            .iter()
            .filter(|(label, _)| fuzzy_match(query, label))
            .map(|(a, b)| (a.to_string(), b.to_string()))
            .collect()
    };
    let mut ui = field_ui();
    let query = RefCell::new(String::new());
    let last = Cell::new(Pick::None);
    let off_index = Cell::new(-1i64);
    let all_off = Cell::new(false);
    let pick = |ui: &mut Ui, command: bool, shift: bool, events: Vec<Event>| {
        let keys = if command {
            vec![Key::Meta]
        } else if shift {
            vec![Key::Shift]
        } else {
            vec![]
        };
        host_frame(ui, &frame_at(keys, 1.0, 0.0, events), &mut |ui| {
            ui.panel(PanelOpts::default().at(0.0, 0.0).width(240.0), "p", &mut |ui| {
                let off: Rc<dyn Fn(i64) -> bool> = {
                    let (off_index, all_off) = (off_index.clone(), all_off.clone());
                    Rc::new(move |index| !all_off.get() && index == off_index.get())
                };
                let o = PickerOpts { off: Some(off), ..PickerOpts::default() };
                let q = query.borrow().clone();
                let (edited, result) = ui.picker(&o, "Search", &q, &rows);
                *query.borrow_mut() = edited;
                last.set(result);
            });
        });
    };
    pick(&mut ui, false, false, vec![]);
    if !ui.text_input_focused() {
        fail("picker did not take keyboard focus");
    }
    pick(&mut ui, false, false, vec![key_down(Key::ArrowDown), key_down(Key::ArrowDown), key_down(Key::Enter)]);
    if last.get() != Pick::Pick(2) {
        fail("picker arrows did not move the cursor");
    }
    pick(&mut ui, false, false, vec![Event::TextInput("dla".into()), key_down(Key::Enter)]);
    if *query.borrow() != "dla" || last.get() != Pick::Pick(0) || rows(&query.borrow())[0].0 != "delta" {
        fail("picker typing did not filter rows before Enter");
    }
    with_clipboard(&mut || {
        *query.borrow_mut() = String::new();
        clipboard::set_text("del").unwrap_or_else(|m| fail(&m));
        pick(&mut ui, true, false, vec![key_down(kc('v'))]);
        pick(&mut ui, false, false, vec![]);
        if *query.borrow() != "del" {
            fail("picker did not paste the search query");
        }
        pick(&mut ui, true, false, vec![key_down(kc('c'))]);
        if clipboard::get_text() != Ok("del".to_owned()) {
            fail("picker did not copy the search query");
        }
        pick(&mut ui, true, false, vec![key_down(kc('x'))]);
        if !query.borrow().is_empty() {
            fail("picker did not cut the search query");
        }
        *query.borrow_mut() = "delta".to_owned();
        pick(&mut ui, false, false, vec![]);
        pick(&mut ui, false, false, vec![key_down(Key::ArrowLeft)]);
        pick(&mut ui, false, true, vec![key_down(Key::ArrowLeft)]);
        pick(&mut ui, true, false, vec![key_down(kc('c'))]);
        if clipboard::get_text() != Ok("t".to_owned()) {
            fail("picker did not copy the selected query character");
        }
        pick(&mut ui, true, false, vec![key_down(kc('x'))]);
        if *query.borrow() != "dela" {
            fail("picker did not cut the selection");
        }
        pick(&mut ui, true, false, vec![key_down(kc('v'))]);
        pick(&mut ui, false, false, vec![]);
        if *query.borrow() != "delta" {
            fail("picker pasted outside the caret");
        }
    });
    *query.borrow_mut() = "dla".to_owned();
    pick(&mut ui, false, false, vec![key_down(Key::Delete)]);
    if last.get() != Pick::None {
        fail("picker deleted on the first Delete");
    }
    pick(&mut ui, false, false, vec![key_down(Key::Delete)]);
    if last.get() != Pick::Delete(0) {
        fail("picker did not delete on the second Delete");
    }
    pick(&mut ui, false, false, vec![Event::TextInput("zzz".into()), key_down(Key::Enter)]);
    if last.get() != Pick::Submit {
        fail("picker Enter with no rows did not submit");
    }
    *query.borrow_mut() = String::new();
    pick(&mut ui, false, false, vec![key_down(Key::Backspace)]);
    if last.get() != Pick::Back {
        fail("picker Backspace on an empty query did not go back");
    }
    pick(&mut ui, false, false, vec![key_down(Key::Escape)]);
    if last.get() != Pick::Cancel {
        fail("picker Escape did not cancel");
    }
    // The second row sits at y = 3 + 24 + 24 (search row, first row). A row that cannot be picked
    // leaves the keyboard with the search field.
    *query.borrow_mut() = String::new();
    pick(&mut ui, false, false, vec![]);
    off_index.set(1);
    pick(&mut ui, false, false, vec![]);
    pick(&mut ui, false, false, vec![press((60, 3 + 48 + 12)), release((60, 3 + 48 + 12))]);
    pick(&mut ui, false, false, vec![Event::TextInput("a".into())]);
    if last.get() != Pick::None || *query.borrow() != "a" {
        fail("a click on an unavailable picker row took the keyboard");
    }
    all_off.set(true);
    *query.borrow_mut() = String::new();
    pick(&mut ui, false, false, vec![]);
    pick(&mut ui, false, false, vec![press((60, 3 + 48 + 12))]);
    if last.get() != Pick::None {
        fail("picker row committed on press");
    }
    pick(&mut ui, false, false, vec![release((60, 3 + 48 + 12))]);
    if last.get() != Pick::Pick(1) {
        fail("picker row did not commit on release inside");
    }
    ui.destroy();
}

pub fn window_pxui_context_menu() {
    // Context menu: rows commit on release inside, disabled rows are inert, a press outside
    // dismisses; the host keeps it open on Open.
    let mut ui = field_ui();
    let result = Cell::new(MenuResult::Open);
    let menu = |ui: &mut Ui, events: Vec<Event>| {
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            result.set(ui.context_menu(
                (20.0, 20.0),
                &MenuOpts::default(),
                "ctx",
                &[("One".to_owned(), true), ("Two".to_owned(), false), ("Three".to_owned(), true)],
            ));
        });
    };
    let row = |index: i64| (60, 20 + 3 + index * 24 + 12);
    menu(&mut ui, vec![]);
    menu(&mut ui, vec![press(row(0))]);
    if result.get() != MenuResult::Open {
        fail("context menu committed on press");
    }
    menu(&mut ui, vec![release(row(0))]);
    if result.get() != MenuResult::Pick(0) {
        fail("context menu did not commit on release");
    }
    menu(&mut ui, vec![press(row(1)), release(row(1))]);
    if result.get() != MenuResult::Open {
        fail("disabled context menu row committed");
    }
    menu(&mut ui, vec![press(row(2)), release((300, 200))]);
    if result.get() != MenuResult::Open {
        fail("context menu committed after release outside");
    }
    menu(&mut ui, vec![press((300, 200))]);
    if result.get() != MenuResult::Dismiss {
        fail("press outside did not dismiss the context menu");
    }
    ui.destroy();
}

pub fn window_pxui_submenu() {
    let mut ui = field_ui();
    let submenu = |ui: &mut Ui, events: Vec<Event>| -> MenuResult {
        let mut out = MenuResult::Open;
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            let o = MenuOpts {
                width: Some(150.0),
                submenus: vec![Submenu {
                    row: 0,
                    rows: vec![("Draw".to_owned(), true), ("Value".to_owned(), true)],
                    keys: vec![],
                    current: None,
                }],
                ..MenuOpts::default()
            };
            out = ui.context_menu((20.0, 20.0), &o, "submenu", &[("New graph".to_owned(), true), ("Delete".to_owned(), true)]);
        });
        out
    };
    submenu(&mut ui, vec![]);
    submenu(&mut ui, vec![move_to((60, 38))]);
    submenu(&mut ui, vec![move_to((190, 62))]);
    if submenu(&mut ui, vec![press((190, 62))]) != MenuResult::Open {
        fail("submenu committed on press");
    }
    if submenu(&mut ui, vec![]) != MenuResult::Open {
        fail("submenu lost capture between frames");
    }
    if submenu(&mut ui, vec![release((190, 62))]) != MenuResult::Pick(3) {
        fail("submenu did not commit on release");
    }
    if submenu(&mut ui, vec![press((300, 200))]) != MenuResult::Dismiss {
        fail("submenu did not dismiss outside");
    }
    ui.destroy();
}

#[derive(Default, Clone)]
pub struct AreaStep {
    pub keys: Vec<Key>,
    pub reveal: Option<i64>,
    pub reveal_nonce: i64,
    pub select: Option<(i64, i64)>,
    pub splice: Vec<(i64, i64, String)>,
}

pub struct Rig {
    pub ui: Ui,
    pub area: String,
    pub errors: Vec<i64>,
    pub wrapped: bool,
    pub submitted: bool,
    pub language: Option<Language>,
    pub context: Rc<Cell<Option<(f64, f64)>>>,
    pub scrubs: Rc<RefCell<Vec<ScrubPhase>>>,
    pub scrub_edits: Rc<RefCell<Vec<((i64, i64), String)>>>,
    pub selected: Rc<Cell<(i64, i64)>>,
    pub time: f64,
    pub command_key: Key,
}

impl Rig {
    pub fn step(&mut self, o: AreaStep, events: Vec<Event>) {
        self.time += 0.5;
        let context = self.context.clone();
        let scrubs = self.scrubs.clone();
        let scrub_edits = self.scrub_edits.clone();
        let selected = self.selected.clone();
        let opts = TextAreaOpts {
            reveal: o.reveal,
            reveal_nonce: o.reveal_nonce,
            select: o.select,
            splice: o.splice,
            on_select: Some(Rc::new(move |range| selected.set(range))),
            wrap: self.wrapped,
            errors: self.errors.clone(),
            language: self.language.clone(),
            on_context: Some(Rc::new(move |at| context.set(Some((f64::from(at.0), f64::from(at.1)))))),
            on_scrub: Some(Rc::new(move |p| scrubs.borrow_mut().insert(0, p))),
            on_scrub_edit: Some(Rc::new(move |range, value| scrub_edits.borrow_mut().insert(0, (range, value.to_owned())))),
            ..TextAreaOpts::default()
        };
        let area = &mut self.area;
        let submitted = &mut self.submitted;
        host_frame(&mut self.ui, &frame_at(o.keys, 1.0, self.time, events), &mut |ui| {
            let (text, submit) = ui.text_area_submit((0.0, 0.0), 300.0, 96.0, &opts, "area", area);
            *area = text;
            *submitted = submit;
        });
    }

    pub fn plain(&mut self, events: Vec<Event>) {
        self.step(AreaStep::default(), events);
    }

    pub fn keyed(&mut self, keys: Vec<Key>, events: Vec<Event>) {
        self.step(AreaStep { keys, ..AreaStep::default() }, events);
    }

    pub fn region(&self) -> (i64, i64) {
        match self.ui.output().platform.ime {
            Some(((_, y, _, _), cursor)) => (y, cursor),
            None => fail("text area lost its IME region"),
        }
    }

    pub fn expect(&self, what: &str, want: &str) {
        if self.area != want {
            fail(&format!("text area {}: {:?}", what, self.area));
        }
    }
}

pub fn text_area_start(platform: Platform) -> Rig {
    let mut rig = Rig {
        ui: Ui::create(platform, Some(11), golden_text()),
        area: String::new(),
        errors: Vec::new(),
        wrapped: false,
        submitted: false,
        language: None,
        context: Rc::new(Cell::new(None)),
        scrubs: Rc::new(RefCell::new(Vec::new())),
        scrub_edits: Rc::new(RefCell::new(Vec::new())),
        selected: Rc::new(Cell::new((0, 0))),
        time: 0.0,
        command_key: input::primary(platform),
    };
    rig.plain(vec![]);
    rig.plain(vec![
        press((150, 12)),
        release((150, 12)),
        Event::TextInput("ab".into()),
        key_down(Key::Enter),
        Event::TextInput("cd".into()),
    ]);
    rig.expect("insert and Enter", "ab\ncd");
    rig.plain(vec![key_down(Key::ArrowUp), Event::TextInput("X".into())]);
    rig.expect("Up keeps the column", "abX\ncd");
    rig.plain(vec![key_down(Key::Home), Event::TextInput("_".into())]);
    rig.expect("Home is line-scoped", "_abX\ncd");
    rig.plain(vec![key_down(Key::ArrowDown), key_down(Key::End), key_down(Key::Backspace)]);
    rig.expect("Down then End then Backspace", "_abX\nc");
    rig.plain(vec![key_down(Key::Delete)]);
    rig.expect("Delete at the end", "_abX\nc");
    let cursor = rig.region().1;
    rig.plain(vec![Event::TextEditing { text: "e".into(), start: 0, length: 1 }]);
    if rig.region().1 != cursor {
        fail("composition moved the caret");
    }
    rig.plain(vec![Event::TextInput("\u{e9}".into())]);
    rig.expect("IME commit", "_abX\nc\u{e9}");
    let previous = clipboard::get_text();
    let command_key = rig.command_key;
    rig.keyed(vec![Key::Shift], vec![key_down(Key::ArrowLeft)]);
    rig.keyed(vec![command_key], vec![key_down(kc('c'))]);
    if clipboard::get_text() != Ok("\u{e9}".to_owned()) {
        fail("text area copy ignored the selection");
    }
    rig.keyed(vec![command_key], vec![key_down(kc('a'))]);
    rig.keyed(vec![command_key], vec![key_down(kc('x'))]);
    rig.expect("cut all", "");
    if clipboard::get_text() != Ok("_abX\nc\u{e9}".to_owned()) {
        fail("text area cut lost the text");
    }
    rig.keyed(vec![command_key], vec![key_down(kc('v'))]);
    rig.plain(vec![]);
    rig.expect("paste keeps the newline", "_abX\nc\u{e9}");
    // copy and cut with no selection take the caret line: of an empty area, nothing
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![command_key], vec![key_down(kc('c'))]);
    rig.keyed(vec![command_key], vec![key_down(kc('x'))]);
    rig.expect("copy and cut of an empty area", "");
    rig.area = "ab\ncd".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![command_key], vec![key_down(kc('x'))]);
    rig.expect("cut with no selection takes the caret's line", "cd");
    if let Ok(text) = previous {
        let _ = clipboard::set_text(&text);
    }
    rig
}

pub fn text_area_1(rig: &mut Rig) {
    // Tab keeps the focus and inserts two spaces, Shift-Tab takes them off, Command-Enter is the
    // host apply
    rig.area = "ab".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.plain(vec![key_down(Key::Tab)]);
    rig.expect("Tab", "ab  ");
    if !rig.ui.text_input_focused() {
        fail("Tab moved the focus out of the text area");
    }
    rig.plain(vec![key_down(Key::Home), key_down(Key::Tab)]);
    rig.expect("Tab at the start of the line", "  ab  ");
    rig.keyed(vec![Key::Shift], vec![key_down(Key::Tab)]);
    rig.expect("Shift-Tab takes two spaces", "ab  ");
    rig.plain(vec![Event::TextInput("x".into())]);
    rig.keyed(vec![Key::Shift], vec![key_down(Key::Tab)]);
    rig.expect("Shift-Tab on an unindented line", "xab  ");
    if rig.submitted {
        fail("an edit submitted the text area");
    }
    rig.keyed(vec![Key::Meta], vec![key_down(Key::Enter)]);
    if !rig.submitted {
        fail("Command-Enter did not report an apply");
    }
    rig.expect("Command-Enter inserts nothing", "xab  ");
    rig.keyed(vec![Key::Ctrl], vec![key_down(Key::Enter)]);
    if !rig.submitted {
        fail("Control-Enter did not report an apply");
    }
    // soft wrapping: one long line is several rows, the caret and Up/Down follow the rows, and
    // nothing scrolls sideways
    rig.area = "a".repeat(100);
    rig.wrapped = true;
    rig.plain(vec![press((150, 60)), release((150, 60))]);
    rig.plain(vec![Event::TextInput("X".into())]);
    let x_at = rig.area.find('X').unwrap() as i64;
    if x_at < 60 || x_at > 99 {
        fail(&format!("a click on the third row landed at column {}", x_at));
    }
    // a text line is one and a half times the 11-point text: 17 points
    let y_third = rig.region().0;
    if y_third < 2 * 17 {
        fail(&format!("a wrapped caret is on the first row (y {})", y_third));
    }
    rig.plain(vec![key_down(Key::ArrowUp), Event::TextInput("Y".into())]);
    let y_at = rig.area.find('Y').unwrap() as i64;
    if y_at >= x_at || x_at - y_at > 60 {
        fail(&format!("Up did not move to the row above ({} then {})", x_at, y_at));
    }
    rig.plain(vec![key_down(Key::Home), Event::TextInput("H".into())]);
    let h_at = rig.area.find('H').unwrap() as i64;
    if h_at == 0 || h_at > y_at {
        fail("Home on a wrapped row was not row-scoped");
    }
    rig.plain(vec![key_down(Key::ArrowUp), key_down(Key::ArrowUp), key_down(Key::ArrowUp)]);
    rig.plain(vec![Event::TextInput("T".into())]);
    if rig.area.find('T').unwrap() > 40 {
        fail("Up at the first row did not go to the start");
    }
    rig.wrapped = false;
    rig.plain(vec![]);
    // scrolling: the caret line stays inside the 96-point area
    rig.area = (0..40).map(|i| i.to_string()).collect::<Vec<_>>().join("\n");
    rig.errors = vec![3, 40];
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowDown)]);
    for _ in 0..39 {
        rig.plain(vec![key_down(Key::ArrowDown)]);
    }
    rig.plain(vec![]);
    let y = rig.region().0;
    if y < 0 || y > 96 {
        fail(&format!("caret line left the area (y {})", y));
    }
    rig.plain(vec![key_down(Key::Escape)]);
    if rig.ui.text_input_focused() {
        fail("Escape did not leave the text area");
    }
}

pub const CASES: &[(&str, fn())] = &[
    ("window_pxui_picker", window_pxui_picker),
    ("window_pxui_context_menu", window_pxui_context_menu),
    ("window_pxui_submenu", window_pxui_submenu),
];
