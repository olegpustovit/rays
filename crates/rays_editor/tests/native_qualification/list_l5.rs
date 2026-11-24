use input::{Event, Key, MouseButton::LeftButton, Platform};
use rays_shell::tree::{self, Command, Intent, Row};

fn row(id: i64, label: &str) -> Row {
    Row {
        id, label: label.into(), depth: 0, detail: String::new(), ghost: false, link: false,
        badge: ("G".into(), pxui::geom::color::white()), flags: Vec::new(),
    }
}

fn with_tree(run: impl FnOnce(&mut tree::Tree, &[Row], &mut dyn FnMut(&mut tree::Tree, Vec<Key>, Vec<Event>) -> Vec<Intent>)) {
    let mut ui = rays_shell::host::create(Platform::Apple, Some(11), None);
    let rows = [row(1, "first"), row(2, "second")];
    let mut tick = 0;
    let mut step = |tree: &mut tree::Tree, keys, events| {
        tick += 1;
        let frame = rays::frame::Frame {
            input: input::Frame { size: (320, 240), frame: tick, tick, keys, events, ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.),
        };
        let mut edits = Vec::new();
        rays_shell::host::build(&mut ui, &frame, &mut |ui| {
            tree::update(tree, ui, &frame, (0, 0, 320, 240), "Scene", &[], &rows, &[1], &mut edits);
        });
        edits
    };
    let mut tree = tree::create();
    step(&mut tree, vec![], vec![]);
    run(&mut tree, &rows, &mut step);
    rays_shell::host::destroy(&mut ui);
}

fn b28_return_renames_inline() {
    if cfg!(target_os = "macos") {
        let commands = tree::bindings();
        assert!(commands.iter().any(|(binding, cmd)| *cmd == Command::RenameRow
            && matches!(&binding.trigger, Some(editor_core::keymap::Trigger::Chord(Key::Enter, keys)) if keys.is_empty())));
    }
    with_tree(|tree, rows, step| {
        step(tree, vec![], vec![Event::PointerPressed(LeftButton, (110., 36.)), Event::PointerReleased(LeftButton, (110., 36.))]);
        tree::run_command(tree, rows, &[1], Command::RenameRow, &mut Vec::new());
        step(tree, vec![], vec![]);
        step(tree, vec![Key::Meta], vec![Event::KeyPressed(Key::KeyChar('a'))]);
        assert_eq!(
            step(tree, vec![], vec![Event::TextInput(" renamed ".into()), Event::KeyPressed(Key::Enter)]),
            vec![Intent::Rename(1, "renamed".into())],
        );
    });
}

fn b28_escape_cancels_row_drag() {
    with_tree(|tree, _, step| {
        step(tree, vec![], vec![Event::PointerPressed(LeftButton, (90., 36.))]);
        step(tree, vec![], vec![Event::PointerMoved((90., 61.))]);
        step(tree, vec![], vec![Event::KeyPressed(Key::Escape)]);
        let landed = step(tree, vec![], vec![Event::PointerReleased(LeftButton, (90., 61.))]);
        assert!(!landed.iter().any(|e| matches!(e, Intent::Move { .. })), "{landed:?}");
    });
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("list_b28_return_renames_inline", b28_return_renames_inline),
        ("list_b28_escape_cancels_row_drag", b28_escape_cancels_row_drag),
    ]
}
