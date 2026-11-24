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

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("inspector_b21_hex_color_is_one_edit", b21_hex_color_is_one_edit)]
}
