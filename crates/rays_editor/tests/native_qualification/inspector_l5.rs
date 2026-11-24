use input::{Event, Key, MouseButton, Platform};
use param::{FieldView, Value};
use rays_shell::inspector::{self, FlowChange, FlowOpts, FlowRow};

fn field(name: &str, kind: param::Kind, value: Value) -> FieldView {
    let schema = param::schema(
        "test".into(),
        vec![param::field(name.into(), kind, value).unwrap()],
    ).unwrap();
    schema.view(schema.default()).unwrap().remove(0)
}

fn type_in_row(row: &FlowRow, text: &str) -> Vec<FlowChange> {
    let mut ui = rays_shell::host::create(Platform::Apple, Some(11), None);
    let point = (250., 12.);
    let mut next = |frame, keys, events| {
        let f = rays::frame::Frame {
            input: input::Frame { size: (320, 240), frame, tick: frame, keys, events, pointer: point, ..input::at_time(frame as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.),
        };
        let mut out = Vec::new();
        rays_shell::host::build(&mut ui, &f, &mut |ui| {
            out = inspector::flow_fields(ui, &FlowOpts::default(), None, std::slice::from_ref(row));
        });
        out
    };
    next(1, vec![], vec![]);
    next(2, vec![], vec![Event::PointerPressed(MouseButton::LeftButton, point), Event::PointerReleased(MouseButton::LeftButton, point)]);
    next(3, vec![Key::Meta], vec![Event::KeyPressed(Key::KeyChar('a'))]);
    let changes = next(4, vec![], vec![Event::TextInput(text.into()), Event::KeyPressed(Key::Enter)]);
    rays_shell::host::destroy(&mut ui);
    changes
}

fn b21_hex_color_is_one_edit() {
    let row = FlowRow {
        path: "color".into(), shown: true, locked: false, drive: None, live: None,
        fields: ["color_r", "color_g", "color_b"].map(|name| {
            field(name, param::floating(0., 1., None, None).unwrap(), Value::Float(1.))
        }).to_vec(),
    };
    let edits = type_in_row(&row, "#ff0000");
    assert_eq!(edits, vec![FlowChange::EditedVector("color".into(), [1., 0., 0.])]);
}

fn b22_driven_number_can_be_replaced() {
    let row = FlowRow {
        path: "radius".into(), shown: true, locked: false, drive: Some("=(* 2 t)".into()), live: None,
        fields: vec![field("radius", param::floating(0., 10., None, None).unwrap(), Value::Float(2.))],
    };
    assert_eq!(type_in_row(&row, "3"), vec![FlowChange::Edited("radius".into(), Value::Float(3.))]);
}

fn click_budget_inspector_hover() {
    let rows: Vec<FlowRow> = (0..12).map(|i| FlowRow {
        path: format!("argument-{i}"), shown: true, locked: false, drive: Some("=(* 2 t)".into()), live: Some("2.0".into()),
        fields: vec![field("radius", param::floating(0., 10., None, None).unwrap(), Value::Float(2.))],
    }).collect();
    let opts = FlowOpts { kind_label: Some("Geometry".into()), pin_click: true, ..Default::default() };
    let mut ui = rays_shell::host::create(Platform::Apple, Some(11), None);
    ui.verify_views(false);
    let mut counts = Vec::new();
    for tick in 1..=6 {
        let point = (235., match tick { 1 | 2 => 5., 3 | 4 => 38., _ => 62. });
        let frame = rays::frame::Frame {
            input: input::Frame { size: (320, 360), frame: tick, tick, pointer: point, events: vec![Event::PointerMoved(point)], ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.),
        };
        let before = mem_sys::alloc_count();
        rays_shell::host::build(&mut ui, &frame, &mut |ui| {
            rays_shell::kit::cached(ui, "inspector-hover-parent", 1, &mut |ui| {
                inspector::flow_fields(ui, &opts, None, &rows);
            });
        });
        counts.push(mem_sys::alloc_count() - before);
    }
    eprintln!("CLICK inspector hover enter/hold/next/hold: {:?}", &counts[2..]);
    assert!(counts[2] <= 26 && counts[3] == 0 && counts[4] <= 30 && counts[5] == 0, "{counts:?}");
    rays_shell::host::destroy(&mut ui);
}

fn b29_shared_scrub_undoes_both_nodes_at_once() {
    use rays_editor::{editor, editor_commands::Action, reduce, workspace};
    let source = r#"(workspace shared
      (graph materials :context material
        (let* [a (material/standard :roughness 0.2) b (material/standard :roughness 0.7)] b))
      (graph editor :context editor
        (let* [g (ui/graph "materials" :focus true) i (ui/inspector)]
          (ui/workspace (ui/split "horizontal" g i :second_size 350)))))"#;
    let doc = workspace::load(None, Vec::new(), None, source).unwrap();
    let mut e = editor::create(editor::Create { await_: Some(true), platform: Some(Platform::Apple), ..Default::default() },
        &doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    let paths = ["a", "b"].map(|name| vec!["materials".into(), name.into()]);
    e = reduce::select_path(e, &paths[0]);
    std::rc::Rc::make_mut(&mut e.core).select_later = paths.to_vec();
    let mut tick = 0;
    let mut frame = |point, buttons, events| {
        tick += 1;
        rays::frame::Frame {
            input: input::Frame { size: (1000, 700), frame: tick, tick, pointer: point, buttons, events, ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.),
        }
    };
    e.core.ui.borrow_mut().set_a11y(true);
    let first = frame((0., 0.), vec![], vec![]);
    e = editor::update(e, &first);
    let before = editor_document::workspace_doc::to_text(&editor::workspace(&e)).unwrap();
    let (x, y, w, _) = editor::panes(&e, &first).inspector;
    let point = {
        let ui = e.core.ui.borrow();
        let row = ui.a11y_nodes().iter().find(|n| ui.a11y_name_of(n) == "roughness" && n.rect.x >= x as f32)
            .unwrap_or_else(|| panic!("shared roughness row was absent: {:?}", ui.a11y_nodes().iter().map(|n| ui.a11y_name_of(n)).collect::<Vec<_>>()));
        ((row.rect.x + row.rect.w * 0.65) as f64, (row.rect.y + row.rect.h / 2.0) as f64)
    };
    let left = MouseButton::LeftButton;
    e = editor::update(e, &frame(point, vec![left], vec![Event::PointerPressed(left, point)]));
    for dx in [20., 40.] {
        let moved = (point.0 + dx, point.1);
        e = editor::update(e, &frame(moved, vec![left], vec![Event::PointerMoved(moved)]));
    }
    let moved = (point.0 + 40., point.1);
    e = editor::update(e, &frame(moved, vec![], vec![Event::PointerReleased(left, moved)]));
    let after = editor::workspace(&e);
    let values = paths.each_ref().map(|path| {
        let expr = flow_graph::flow_edit::arg_text(&after.source, path, &flow_graph::flow_edit::ArgKey::Kw("roughness".into())).unwrap();
        flow_syntax::lisp::flat(&expr)
    });
    assert_eq!(values[0], values[1], "the scrub must land on both nodes: {values:?}, inspector at {:?}", (x, y, w));
    assert_ne!(values[0], "0.2");
    let e = reduce::step(e, None, None, &[Action::Undo], &frame(point, vec![], vec![])).unwrap();
    assert_eq!(editor_document::workspace_doc::to_text(&editor::workspace(&e)).unwrap(), before);
    assert!(!editor::can_undo(&e), "the scrub created more than one undo entry");
    editor::close(&e);
}

fn b27_inspector_row_context_menu() {
    let row = FlowRow {
        path: "count".into(), shown: true, locked: false, drive: None, live: None,
        fields: vec![field("count", param::integer(0, 10, None, None).unwrap(), Value::Int(2))],
    };
    let mut row = row;
    row.fields[0].current = Value::Int(9);
    let mut changes = Vec::new();
    choose_menu((320, 240), (80., 12.), "Reset to default", &mut |ui| {
        changes = inspector::flow_fields(ui, &FlowOpts::default(), None, std::slice::from_ref(&row));
    });
    assert_eq!(changes, vec![FlowChange::Edited("count".into(), Value::Int(2))]);
}

fn choose_menu(size: (i64, i64), point: (f64, f64), item: &str, build: &mut dyn FnMut(&mut pxui::ui::Ui)) {
    let mut ui = rays_shell::host::create(Platform::Apple, Some(11), None);
    ui.set_a11y(true);
    let mut step = |ui: &mut pxui::ui::Ui, tick, point, events| {
        let f = rays::frame::Frame {
            input: input::Frame { size, frame: tick, tick, pointer: point, events, ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.),
        };
        rays_shell::host::build(ui, &f, build);
    };
    step(&mut ui, 1, point, vec![]);
    let right = MouseButton::RightButton;
    step(&mut ui, 2, point, vec![Event::PointerPressed(right, point), Event::PointerReleased(right, point)]);
    step(&mut ui, 3, point, vec![]);
    let row = ui.a11y_nodes().iter().find(|n| ui.a11y_name_of(n) == item).unwrap_or_else(|| panic!("no menu item {item} at {point:?}"));
    let at = ((row.rect.x + row.rect.w / 2.) as f64, (row.rect.y + row.rect.h / 2.) as f64);
    let left = MouseButton::LeftButton;
    step(&mut ui, 4, at, vec![Event::PointerPressed(left, at), Event::PointerReleased(left, at)]);
    rays_shell::host::destroy(&mut ui);
}

fn b27_readouts_have_context_menus() {
    let was = rays::clipboard::get_text().ok();
    let size = (480, 240);
    let opts = rays_shell::status_bar::Opts { text: "cook complete".into(), ..Default::default() };
    choose_menu(size, (100., 212.), "Copy value", &mut |ui| {
        rays_shell::status_bar::draw(ui, (0, 200, size.0, 24), &opts);
    });
    assert_eq!(rays::clipboard::get_text().unwrap(), opts.text);

    choose_menu(size, (330., 12.), "Copy value", &mut |ui| {
        rays_shell::timeline_bar::draw(ui, (0, 0, size.0, 24), false, false, 42, 0.7, 240);
    });
    assert_eq!(rays::clipboard::get_text().unwrap(), "42");

    let geometry = rdk_ops::line_geometry::points(&[(3., 4., 5.)]).unwrap();
    choose_menu(size, (100., 60.), "Copy value", &mut |ui| {
        rays_editor::spreadsheet::view(ui, (0, 0, size.0, size.1), 0, Some(&geometry));
    });
    assert_eq!(rays::clipboard::get_text().unwrap(), "3.0");

    let mut reveal = false;
    choose_menu(size, (140., 22.), "Reveal in Lisp", &mut |ui| {
        reveal = inspector::header_context(ui, "Example", false, true, &mut |ui| {
            ui.inspector_header("sample-header", None, None, None, None, &[], None, false, "Example", "")
        }).1;
    });
    assert!(reveal);
    if let Some(text) = was { let _ = rays::clipboard::set_text(&text); }
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("inspector_b21_hex_color_is_one_edit", b21_hex_color_is_one_edit),
        ("inspector_b22_driven_number_can_be_replaced", b22_driven_number_can_be_replaced),
        ("click_budget_inspector_hover", click_budget_inspector_hover),
        ("inspector_b29_shared_scrub_undoes_both_nodes_at_once", b29_shared_scrub_undoes_both_nodes_at_once),
        ("inspector_b27_row_context_menu", b27_inspector_row_context_menu),
        ("inspector_b27_readouts_have_context_menus", b27_readouts_have_context_menus),
    ]
}
