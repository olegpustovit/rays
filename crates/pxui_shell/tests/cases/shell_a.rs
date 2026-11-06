//! Ports of lib/pxui_shell/test_shell.ml, part one: the chrome (panes, splitters, header menu,
//! windows, breadcrumbs), the strips (status, timeline, guide, echo) and the prompts.
use input::{Event, Key, MouseButton};
use pxui::ui::*;
use pxui_shell::chrome::{self, Intent, Side};
use pxui_shell::layout::{self as L, Axis, Panel, Size as PSize, t};
use pxui_shell::prompt::{self, Macro, MacroResult};
use pxui_shell::status_bar::{self, TipKind};
use pxui_shell::timeline_bar;
use pxui_shell::which_key;
use rays::frame;
use rays::scene::Layer;

pub fn new_ui() -> Ui {
    let path = format!("{}/../../assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"));
    Ui::create(input::Platform::Apple, Some(11), Some(path))
}

/// The OCaml test's `frame`: 400 x 300 unless sized, at `pointer` with `events`.
pub fn fr(size: (i64, i64), pointer: (f64, f64), events: Vec<Event>) -> frame::t {
    let mut input = input::at_time(0.0);
    input.size = size;
    input.pointer = pointer;
    input.events = events;
    frame::t { input, pixel_scale: (1.0, 1.0), fps: 0.0, mouse_delta: (0.0, 0.0) }
}

pub fn base() -> frame::t {
    fr((400, 300), (0.0, 0.0), vec![])
}

fn left_press(p: (f64, f64)) -> Event {
    Event::Pointer_pressed(MouseButton::LeftButton, p)
}

fn left_release(p: (f64, f64)) -> Event {
    Event::Pointer_released(MouseButton::LeftButton, p)
}

fn right_press(p: (f64, f64)) -> Event {
    Event::Pointer_pressed(MouseButton::RightButton, p)
}

fn right_release(p: (f64, f64)) -> Event {
    Event::Pointer_released(MouseButton::RightButton, p)
}

fn moved(p: (f64, f64)) -> Event {
    Event::Pointer_moved(p)
}

fn instances(f: &mut dyn FnMut(&mut Ui)) -> i64 {
    let mut ui = new_ui();
    ui.frame(&base(), f);
    let staged = Layer::stage_native(400, 300, &ui.scene(None)).unwrap_or_else(|e| panic!("{e}"));
    let mut total = 0;
    for layer in &staged.layers {
        if let Layer::native_layer::Ui_layer(batch, _) = layer {
            total += batch.count();
        }
    }
    ui.destroy();
    total
}

pub fn window_shell_pane_root() {
    let mut ui = new_ui();
    let pane = |ui: &mut Ui, events: Vec<Event>| -> (Signal, Signal) {
        let mut out: Option<(Signal, Signal)> = None;
        let b = base();
        ui.frame(&fr((400, 300), (0.0, 0.0), events), &mut |ui| {
            let root = chrome::pane_root(ui, &b, (100, 20, 100, 80), "pane");
            let mut child: Option<UiBox> = None;
            ui.within(&root, &mut |ui| {
                child = Some(ui.box_("child", BoxOpts::default().flags(CLICKABLE).w(Size::Px(20.0)).h(Size::Px(20.0)).at((110.0, 30.0))));
            });
            out = Some((ui.signal(&root), ui.signal(&child.unwrap())));
        });
        out.unwrap()
    };
    pane(&mut ui, vec![]);
    let (root, child) = pane(&mut ui, vec![left_press((115.0, 35.0))]);
    assert!(root.subtree_press == Some(0) && child.pressed, "pane root missed its child control press");
    let (root, _) = pane(&mut ui, vec![left_release((115.0, 35.0)), left_press((5.0, 5.0))]);
    assert!(root.subtree_press.is_none(), "pane root accepted a press outside its hit bounds");
    ui.destroy();
}

pub fn window_shell_splitter_hit_area() {
    let layout = L::default();
    let wide = fr((1000, 300), (0.0, 0.0), vec![]);
    let panes = L::panes(&L::geometry(None, None, &layout, &wide));
    assert_eq!((panes.view.2, panes.graph.2, panes.inspector.2), (449, 349, 200), "shell layout defaults are not 45/35/20");
    let divider_x = panes.view.2;
    let mut ui = new_ui();
    let chrome_frame = |ui: &mut Ui, events: Vec<Event>| {
        let f = fr((1000, 300), (0.0, 0.0), events);
        ui.frame(&f, &mut |ui| {
            chrome::update(None, None, None, None, None, None, &layout, ui, &wide);
            chrome::splitters(None, None, &layout, ui, &wide);
        });
    };
    chrome_frame(&mut ui, vec![]);
    chrome_frame(&mut ui, vec![moved(((divider_x - 2) as f64, 100.0))]);
    assert!(ui.cursor() == Some(Cursor::Horizontal_resize), "thin splitter lost its wider resize hit area");
    ui.destroy();
}

pub fn window_shell_tools_start() {
    // where a header's tools begin: after the title, 8, the 1-point rule between 4-point margins, 8;
    // the focus square adds 12; a window sets its path in the label size
    let mut ui = new_ui();
    let from = |ui: &mut Ui, focused: bool, text: &str| chrome::tools_start(ui, focused, false, false, text);
    let focused = from(&mut ui, true, "Graph\tgarden / sop");
    let plain = from(&mut ui, false, "Graph\tgarden / sop");
    let title = from(&mut ui, false, "Graph");
    assert!((focused - plain - 12.0).abs() <= 1e-6 && plain > title + 25.0, "a header's tools do not start after its focus square, title and breadcrumb");
    let cap = pxui_shell::kit::cap_width(&mut ui, "Graph");
    assert!((title - (12.0 + cap + 25.0)).abs() <= 1e-6, "a title alone is not 12 in, its label and 25 to the tools");
    ui.destroy();
}

fn chrome_frame(ui: &mut Ui, tree: &t, mouse: (f64, f64), events: Vec<Event>) -> Vec<Intent> {
    let f = fr((1000, 600), mouse, events);
    let mut out: Vec<Intent> = Vec::new();
    ui.frame(&f, &mut |ui| {
        let mut a = chrome::update(None, None, None, None, None, None, tree, ui, &f);
        a.extend(chrome::splitters(None, None, tree, ui, &f));
        out = a;
    });
    out
}

pub fn window_shell_splitter_drag_and_menus() {
    // dragging a splitter reports a ratio, and the header menu reports its intents
    let mut ui = new_ui();
    let default = L::default();
    chrome_frame(&mut ui, &default, (0.0, 0.0), vec![]);
    let x = 449.0;
    chrome_frame(&mut ui, &default, (x, 100.0), vec![moved((x, 100.0))]);
    chrome_frame(&mut ui, &default, (x, 100.0), vec![left_press((x, 100.0))]);
    let drag = chrome_frame(&mut ui, &default, (300.0, 100.0), vec![moved((300.0, 100.0))]);
    match drag.as_slice() {
        [Intent::Resize { node, size: PSize::Ratio(ratio) }] if node.is_empty() => {
            assert!((ratio - 0.3).abs() <= 0.01, "splitter ratio does not follow the pointer")
        }
        _ => panic!("a drag did not report one resize of the outer split: {drag:?}"),
    }
    let released = chrome_frame(&mut ui, &default, (300.0, 100.0), vec![left_release((300.0, 100.0))]);
    assert!(released.contains(&Intent::Settled), "a drag did not settle");
    // the gutter of a fixed split reports whole points of its fixed side
    let sized = t::Split { axis: Axis::H, size: PSize::Second(320), a: Box::new(t::Leaf(Panel::Graph)), b: Box::new(t::Leaf(Panel::Inspector)) };
    chrome_frame(&mut ui, &sized, (679.0, 100.0), vec![moved((679.0, 100.0))]);
    chrome_frame(&mut ui, &sized, (679.0, 100.0), vec![left_press((679.0, 100.0))]);
    let fixed = chrome_frame(&mut ui, &sized, (599.4, 100.0), vec![moved((599.4, 100.0))]);
    assert!(
        matches!(fixed.as_slice(), [Intent::Resize { node, size: PSize::Second(400) }] if node.is_empty()),
        "a fixed split's gutter did not report points: {fixed:?}"
    );
    chrome_frame(&mut ui, &sized, (599.4, 100.0), vec![left_release((599.4, 100.0))]);
    // a right-click on a gutter sizes its split another way, keeping the sizes it shows
    let menu_pick = |ui: &mut Ui, row: i64| -> Vec<Intent> {
        chrome_frame(ui, &sized, (679.0, 100.0), vec![moved((679.0, 100.0))]);
        chrome_frame(ui, &sized, (679.0, 100.0), vec![right_press((679.0, 100.0))]);
        chrome_frame(ui, &sized, (679.0, 100.0), vec![right_release((679.0, 100.0))]);
        let at = (700.0, 112.0 + (24.0 * row as f64));
        chrome_frame(ui, &sized, at, vec![moved(at)]);
        chrome_frame(ui, &sized, at, vec![left_press(at), left_release(at)])
    };
    let first = menu_pick(&mut ui, 1);
    assert!(
        matches!(first.as_slice(), [Intent::Resize { node, size: PSize::First(679) }, Intent::Settled] if node.is_empty()),
        "the gutter menu did not fix the first side ({} intents): {:?}",
        first.len(), first
    );
    let ratio = menu_pick(&mut ui, 0);
    assert!(
        matches!(ratio.as_slice(), [Intent::Resize { node, size: PSize::Ratio(r) }, Intent::Settled] if node.is_empty() && (r - 679.5 / 999.0).abs() < 1e-6),
        "the gutter menu did not size the split by its ratio"
    );
    // the header menu no longer holds the size rows: the gutter's right-click above does
    chrome_frame(&mut ui, &default, (600.0, 10.0), vec![moved((600.0, 10.0))]);
    chrome_frame(&mut ui, &default, (600.0, 10.0), vec![right_press((600.0, 10.0))]);
    chrome_frame(&mut ui, &default, (600.0, 10.0), vec![right_release((600.0, 10.0))]);
    chrome_frame(&mut ui, &default, (620.0, 28.0), vec![moved((620.0, 28.0))]);
    let pick = chrome_frame(&mut ui, &default, (620.0, 28.0), vec![left_press((620.0, 28.0)), left_release((620.0, 28.0))]);
    assert!(
        matches!(pick.as_slice(), [Intent::Split_panel(path, Axis::H)] if path == &vec![1usize, 0]),
        "the header menu did not split the graph panel ({} intents)",
        pick.len()
    );
    chrome_frame(&mut ui, &default, (985.0, 10.0), vec![moved((985.0, 10.0))]);
    let collapse = chrome_frame(&mut ui, &default, (985.0, 10.0), vec![left_press((985.0, 10.0)), left_release((985.0, 10.0))]);
    assert!(collapse.contains(&Intent::Toggle(vec![1, 1])), "the collapse button did not toggle the inspector");
    ui.destroy();
}

pub fn window_shell_window_past_the_edge() {
    // a window saved past the right edge is drawn inside the frame, and a drag moves it from where
    // it is drawn: the pointer is followed at once
    let mut ui = new_ui();
    let past = editor_core::panels::State { collapsed: false, window: Some((900, 50, 300, 200)) };
    let state = move |path: &[usize]| -> editor_core::panels::State { if path.is_empty() { past } else { editor_core::panels::default_state() } };
    let tree = t::Leaf(Panel::Graph);
    let windowed = |ui: &mut Ui, mouse: (f64, f64), events: Vec<Event>| -> Vec<Intent> {
        let f = fr((1000, 600), mouse, events);
        let mut out = Vec::new();
        ui.frame(&f, &mut |ui| {
            out = chrome::update(Some(&state), None, None, None, None, None, &tree, ui, &f);
        });
        out
    };
    windowed(&mut ui, (0.0, 0.0), vec![]);
    windowed(&mut ui, (750.0, 67.0), vec![moved((750.0, 67.0))]);
    windowed(&mut ui, (750.0, 67.0), vec![left_press((750.0, 67.0))]);
    let drags = windowed(&mut ui, (650.0, 67.0), vec![moved((650.0, 67.0))]);
    let got: Vec<(i64, i64)> = drags
        .iter()
        .filter_map(|i| match i {
            Intent::Window_drag(_, (x, _, w, _), _) => Some((*x, *w)),
            _ => None,
        })
        .collect();
    assert!(got == vec![(600, 300)], "a window saved past the edge did not follow the pointer: {got:?}");
    ui.destroy();
}

pub fn window_shell_breadcrumb_clicks() {
    // the parts of a docked header's breadcrumb before its last are buttons
    let mut ui = new_ui();
    let tree = t::Leaf(Panel::Graph);
    let title = |_: &L::Leaf| "Graph\tfirst / second / third / sop".to_string();
    let crumbed = |ui: &mut Ui, mouse: (f64, f64), events: Vec<Event>| -> Vec<Intent> {
        let f = fr((1000, 600), mouse, events);
        let mut out = Vec::new();
        ui.frame(&f, &mut |ui| {
            out = chrome::update(None, None, Some(&title), None, None, None, &tree, ui, &f);
        });
        out
    };
    crumbed(&mut ui, (0.0, 0.0), vec![]);
    let mut hit: Vec<i64> = Vec::new();
    for i in 0..40 {
        let p = ((20 + i * 6) as f64, 10.0);
        crumbed(&mut ui, p, vec![moved(p)]);
        crumbed(&mut ui, p, vec![left_press(p)]);
        for intent in crumbed(&mut ui, p, vec![left_release(p)]) {
            if let Intent::Crumb(_, index) = intent {
                if !hit.contains(&index) {
                    hit.push(index);
                }
            }
        }
    }
    hit.sort();
    assert!(hit == vec![0, 1, 2], "breadcrumb clicks reported {hit:?}, not the first three parts");
    ui.destroy();
}

pub fn window_shell_drop_targets() {
    // dock targets exist only while a panel is dragged, and a release over an edge docks there
    let mut ui = new_ui();
    let tree = L::default();
    let f = fr((1000, 600), (0.0, 0.0), vec![]);
    let geometry = L::geometry(None, None, &tree, &f);
    let state = |_: &[usize]| editor_core::panels::default_state();
    let run = |ui: &mut Ui, mouse: (f64, f64), events: Vec<Event>, dragging: Option<(Vec<usize>, bool)>| -> Vec<Intent> {
        let f = fr((1000, 600), mouse, events);
        let mut out = Vec::new();
        ui.frame(&f, &mut |ui| {
            out = chrome::drop_targets(ui, dragging.clone(), &geometry, &state);
        });
        out
    };
    assert!(run(&mut ui, (0.0, 0.0), vec![], None).is_empty());
    // the left edge of the viewport body
    let p = (4.0, 300.0);
    run(&mut ui, p, vec![moved(p)], Some((vec![1, 0], false)));
    let docked = run(&mut ui, p, vec![moved(p)], Some((vec![1, 0], true)));
    assert!(docked.contains(&Intent::Dock_panel(vec![1, 0], vec![0], Side::Left)), "a release over an edge did not dock there: {docked:?}");
    ui.destroy();
}

pub fn window_shell_status_and_guide_draw() {
    let bindings: Vec<editor_core::command::t> = vec![
        editor_core::command::make(Some(editor_core::keymap::Trigger::Leader("s".into())), None, vec![], "save", "Save", 0),
        editor_core::command::make(Some(editor_core::keymap::Trigger::Leader("v".into())), Some(editor_core::panels::Panel::View(String::new())), vec![], "view", "View", 0),
    ];
    let leader = |focus: editor_core::panels::Panel| -> i64 {
        instances(&mut |ui| {
            which_key::panel(ui, None, None, None, &bindings, "", &focus, "View");
        })
    };
    assert!(leader(editor_core::panels::Panel::View(String::new())) > leader(editor_core::panels::Panel::Graph), "focused leader bindings were not drawn");
    let status = |height: i64| -> i64 {
        instances(&mut |ui| {
            let o = status_bar::Opts { text: "Cook complete".into(), fps: Some(60), ..status_bar::Opts::default() };
            status_bar::draw(ui, (0, 272, 400, height), &o);
        })
    };
    assert!(status(24) > 0 && status(0) == 0, "status strip visibility or drawing failed");
    let mut ui = new_ui();
    ui.frame(&base(), &mut |ui| {
        let o = status_bar::Opts { text: "Overlay".into(), ..status_bar::Opts::default() };
        status_bar::draw(ui, (0, 272, 400, 28), &o);
    });
    assert!(!ui.scene(None).is_empty(), "PXUI did not draw the status bar");
    ui.destroy();
}

pub fn window_shell_timeline_frame_field() {
    let mut ui = new_ui();
    let bar = |ui: &mut Ui, mouse: (f64, f64), events: Vec<Event>| -> Vec<timeline_bar::Intent> {
        let mut out = Vec::new();
        ui.frame(&fr((400, 300), mouse, events), &mut |ui| {
            out = timeline_bar::draw(ui, (0, 270, 400, 30), false, false, 12, 0.2, 240);
        });
        out
    };
    assert!(bar(&mut ui, (0.0, 0.0), vec![]).is_empty(), "idle timeline emitted a playback request");
    // the frame field: found by pressing along the strip until a control takes the keyboard
    let press = |ui: &mut Ui, p: (f64, f64)| {
        bar(ui, p, vec![moved(p)]);
        bar(ui, p, vec![left_press(p), left_release(p)]);
        bar(ui, p, vec![]);
    };
    let mut x = 3.0;
    loop {
        assert!(x <= 400.0, "the timeline has no frame field");
        press(&mut ui, (x, 285.0));
        if ui.text_input_focused() {
            break;
        }
        x += 6.0;
    }
    let enter = |ui: &mut Ui, text: &str, key: Key| -> Vec<i64> {
        if !ui.text_input_focused() {
            press(ui, (x, 285.0));
        }
        bar(ui, (x, 285.0), vec![Event::Text_input(text.to_string())]);
        let mut sought = bar(ui, (x, 285.0), vec![Event::Key_pressed(key)]);
        sought.extend(bar(ui, (x, 285.0), vec![]));
        sought
            .into_iter()
            .filter_map(|i| match i {
                timeline_bar::Intent::Seek_playback(n) => Some(n),
                _ => None,
            })
            .collect()
    };
    assert!(enter(&mut ui, "7", Key::Enter) == vec![7], "a typed frame did not seek");
    assert!(enter(&mut ui, "9", Key::Escape).is_empty(), "Escape did not cancel the typed frame");
    assert!(!ui.text_input_focused(), "Escape left the frame field open");
    assert!(enter(&mut ui, "99999", Key::Enter) == vec![240], "a frame past the end did not clamp to the last frame");
    assert!(enter(&mut ui, "-5", Key::Enter) == vec![0], "a negative frame did not clamp to 0");
    assert!(enter(&mut ui, "x", Key::Enter).is_empty(), "text that is not a frame was committed");
    bar(&mut ui, (0.0, 0.0), vec![Event::Key_pressed(Key::Escape)]);
    ui.destroy();
}

pub fn window_shell_prompts() {
    let mut ui = new_ui();
    let mut query = None;
    ui.frame(&base(), &mut |ui| {
        query = prompt::name(ui, "name", "Save", "Name it", "Name", "draft");
    });
    assert!(query == Some(("draft".to_string(), Pick::None)), "name prompt lost its initial query");
    // the make-macro dialog keeps the host's state until something is pressed
    let m = Macro { name: "shifted".into(), holes: vec![(true, "p1".into()), (false, "p2".into()), (true, "p3".into())] };
    let literals = vec!["0.5".to_string(), "12".to_string(), "\"a\"".to_string()];
    let mut answer = None;
    ui.frame(&base(), &mut |ui| {
        answer = prompt::macro_(ui, "macro", "Make a macro", &literals, &["seed".to_string()], &m);
    });
    assert!(answer == Some((m.clone(), MacroResult::None)), "macro prompt changed its state or submitted on its own");
    ui.destroy();
}

pub fn window_shell_echo_tips() {
    // Echo: a refusal is a tip in the error ink (no dot), information has its dot; neither is drawn
    // for no tips, and a card under a tip does not change what is drawn.
    let tip_instances = |avoid: &[(f64, f64, f64, f64)], tips: &[(String, TipKind)]| -> i64 {
        instances(&mut |ui| {
            status_bar::tips(ui, (0, 0, 400, 300), avoid, tips);
        })
    };
    let refusal = vec![("Cannot delete the root".to_string(), TipKind::Refusal)];
    let info = vec![("Saved".to_string(), TipKind::Info)];
    assert!(tip_instances(&[], &[]) == 0, "no tips drew something");
    assert!(tip_instances(&[], &refusal) != 0, "a refusal tip drew nothing");
    assert!(tip_instances(&[], &refusal) != tip_instances(&[], &info), "a refusal tip is drawn like information (it has no dot)");
    assert!(
        tip_instances(&[(12.0, 250.0, 100.0, 40.0)], &refusal) == tip_instances(&[], &refusal),
        "a tip over a card changed what it draws"
    );
}

pub fn window_shell_status_rule_drags() {
    // the status strip's rule between the lead and the labels: the lead takes the room the labels
    // and hints leave (not a quarter of the strip), and the rule drags to resize it
    let mut ui = new_ui();
    let strip = |ui: &mut Ui, mouse: (f64, f64), events: Vec<Event>| {
        ui.frame(&fr((1000, 300), mouse, events), &mut |ui| {
            let o = status_bar::Opts {
                file: "a.rays".into(),
                text: "t 10 live, 4 cached, cook 380 ms and a status line that is long".into(),
                kind: Some("Graph".into()),
                selection: Some("x".into()),
                fps: Some(60),
                ..status_bar::Opts::default()
            };
            status_bar::guide(ui, (0, 272, 1000, 28), &o, editor_core::guide_context::t::Canvas);
        });
    };
    let rule_x = |ui: &mut Ui| -> i64 {
        let mut x = 0;
        loop {
            assert!(x <= 999, "the status strip has no rule to drag");
            let p = (x as f64, 285.0);
            strip(ui, p, vec![moved(p)]);
            strip(ui, p, vec![]);
            if ui.cursor() == Some(Cursor::Horizontal_resize) {
                return x;
            }
            x += 1;
        }
    };
    let x0 = rule_x(&mut ui);
    assert!(x0 >= 300, "the status lead is cut at a quarter of the strip with room left");
    let a = (x0 as f64, 285.0);
    let b = ((x0 - 120) as f64, 285.0);
    strip(&mut ui, a, vec![left_press(a)]);
    strip(&mut ui, b, vec![moved(b)]);
    strip(&mut ui, b, vec![left_release(b)]);
    let x1 = rule_x(&mut ui);
    assert!((x1 - (x0 - 120)).abs() <= 8, "the status rule did not follow the drag: {x0} to {x1}");
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("window_shell_pane_root", window_shell_pane_root),
    ("window_shell_splitter_hit_area", window_shell_splitter_hit_area),
    ("window_shell_tools_start", window_shell_tools_start),
    ("window_shell_splitter_drag_and_menus", window_shell_splitter_drag_and_menus),
    ("window_shell_window_past_the_edge", window_shell_window_past_the_edge),
    ("window_shell_breadcrumb_clicks", window_shell_breadcrumb_clicks),
    ("window_shell_drop_targets", window_shell_drop_targets),
    ("window_shell_status_and_guide_draw", window_shell_status_and_guide_draw),
    ("window_shell_timeline_frame_field", window_shell_timeline_frame_field),
    ("window_shell_prompts", window_shell_prompts),
    ("window_shell_echo_tips", window_shell_echo_tips),
    ("window_shell_status_rule_drags", window_shell_status_rule_drags),
];
