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
    assert!(counts[2] <= 39 && counts[3] == 0 && counts[4] <= 42 && counts[5] == 0, "{counts:?}");
    rays_shell::host::destroy(&mut ui);
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("inspector_b21_hex_color_is_one_edit", b21_hex_color_is_one_edit),
        ("inspector_b22_driven_number_can_be_replaced", b22_driven_number_can_be_replaced),
        ("click_budget_inspector_hover", click_budget_inspector_hover),
    ]
}
