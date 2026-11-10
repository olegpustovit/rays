//! the Router and the platform-modifier labels (the History half lives in rays_editor/src/test_editor_history.rs).
use crate::command;
use crate::keymap::{label, Trigger};
use crate::panels::Panel;
use crate::router::{self, State};
use crate::frame;
use input::{Event, Key, Platform};

/// The scopes the tests use, as the panels they are.
struct Scope;

#[allow(non_upper_case_globals)]
impl Scope {
    const View: Panel = Panel::View(String::new());
    const Graph: Panel = Panel::Graph;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Act {
    Toggle,
    Layout,
    Undo,
    Redo,
    Delete,
    Frame,
    Palette,
    AddLight,
    Add,
}

type Cmd = command::Command;

// These event-order fixtures own their input and inspect the complete routed result.
fn route_frame(previous: &[Key], platform: Platform, keymap: &[&Cmd], focus: &Panel, text_focus: bool,
    frame: frame::Frame, state: State) -> (State, Vec<Cmd>, frame::Frame) {
    let (state, commands, routed) = router::step(previous, platform, keymap, focus, text_focus, &frame, state);
    (state, commands, routed.into_owned())
}
fn fly_frame(frame: frame::Frame) -> (bool, frame::Frame) {
    let (ended, routed) = router::fly(&frame);
    (ended, routed.into_owned())
}

const ACTS: [Act; 9] = [Act::Toggle, Act::Layout, Act::Undo, Act::Redo, Act::Delete, Act::Frame, Act::Palette, Act::AddLight, Act::Add];

/// The command's action code is the act's index in `ACTS`.
fn unact(code: usize) -> Act {
    ACTS[code]
}

fn make(trigger: Option<Trigger>, scope: Option<Panel>, guide: Vec<crate::guide_context::GuideContext>, id: &str, label: &str, action: Act) -> Cmd {
    command::make(trigger, scope, guide, id, label, action as usize)
}

fn cmd(id: &str, label: &str, trigger: Option<Trigger>, scope: Option<Panel>, action: Act) -> Cmd {
    make(trigger, scope, Vec::new(), id, label, action)
}

fn leader(sequence: &str) -> Option<Trigger> {
    Some(Trigger::Leader(sequence.into()))
}

fn chord(key: Key, modifiers: Vec<Key>) -> Option<Trigger> {
    Some(Trigger::Chord(key, modifiers))
}

fn bindings() -> Vec<Cmd> {
    vec![
        cmd("toggle", "toggle graph", leader("g"), None, Act::Toggle),
        cmd("layout", "layout", leader("l"), Some(Scope::Graph), Act::Layout),
        cmd("undo", "undo", chord(Key::KeyChar('z'), vec![Key::Meta]), None, Act::Undo),
        cmd(
            "redo",
            "redo",
            chord(Key::KeyChar('z'), vec![Key::Meta, Key::Shift]),
            None,
            Act::Redo,
        ),
        cmd("delete", "delete", chord(Key::Delete, vec![]), Some(Scope::Graph), Act::Delete),
        cmd("frame", "frame", chord(Key::KeyChar('f'), vec![]), Some(Scope::Graph), Act::Frame),
        cmd("add-light", "add light", leader("al"), None, Act::AddLight),
        // No trigger: palette only, never routed from a key.
        cmd("palette", "palette only", None, None, Act::Palette),
    ]
}

fn frame_with(events: Vec<Event>) -> frame::Frame {
    let mut input = input::at_time(0.);
    input.frame = 0;
    input.tick = 0;
    input.size = (10, 10);
    input.events = events;
    frame::Frame { input, pixel_scale: (1., 1.), fps: 0., mouse_delta: (0., 0.) }
}

fn with_keys(mut frame: frame::Frame, keys: Vec<Key>) -> frame::Frame {
    frame.input.keys = keys;
    frame
}

fn pressed(key: Key) -> Event {
    Event::Key_pressed(key)
}

fn refs(commands: &[Cmd]) -> Vec<&Cmd> {
    commands.iter().collect()
}

/// One router step with no previous keys; returns (state, matched actions, passed events).
fn route(
    platform: Platform,
    commands: &[Cmd],
    focus: Panel,
    text_focus: bool,
    frame: frame::Frame,
    state: State,
) -> (State, Vec<Act>, Vec<Event>) {
    let (state, matched, frame) =
        route_frame(&[], platform, &refs(commands), &focus, text_focus, frame, state);
    (state, matched.iter().map(|c| unact(c.action)).collect(), frame.input.events)
}

fn actions_of(matched: &[Cmd]) -> Vec<Act> {
    matched.iter().map(|c| unact(c.action)).collect()
}

#[test]
fn router_leader_chord_scope_text_focus_and_event_consumption() {
    let b = bindings();
    let alias = |scope: Option<Panel>, trigger: Option<Trigger>, label: &str| {
        make(trigger, scope, Vec::new(), "alias", label, Act::Frame)
    };
    let meta = Some(Trigger::Chord(Key::KeyChar('c'), vec![Key::Meta]));
    let view = alias(Some(Scope::View), meta.clone(), "view alias");
    let graph = alias(Some(Scope::Graph), meta, "graph alias");
    let ctrl = alias(None, chord(Key::KeyChar('c'), vec![Key::Ctrl]), "Ctrl alias");
    let qj = alias(None, leader("qj"), "first leader alias");
    let qq = alias(None, leader("qq"), "second leader alias");
    let aliases = vec![view, graph.clone(), ctrl.clone(), qj, qq.clone()];
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(&aliases),
        &Scope::Graph,
        false,
        with_keys(frame_with(vec![pressed(Key::KeyChar('c'))]), vec![Key::Meta]),
        State::Idle,
    );
    assert!(matched == vec![graph]);
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(&aliases),
        &Scope::Graph,
        false,
        with_keys(frame_with(vec![pressed(Key::KeyChar('c'))]), vec![Key::Ctrl]),
        State::Idle,
    );
    assert!(matched == vec![ctrl]);
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(&aliases),
        &Scope::View,
        false,
        frame_with(vec![
            pressed(Key::KeyChar('/')),
            pressed(Key::KeyChar('q')),
            pressed(Key::KeyChar('q')),
        ]),
        State::Idle,
    );
    assert!(matched == vec![qq]);
    let (traversed, activated, remaining) = route_frame(
        &[],
        Platform::Apple,
        &refs(&b),
        &Scope::View,
        false,
        frame_with(vec![pressed(Key::Tab), pressed(Key::KeyChar('/'))]),
        State::Idle,
    );
    assert!(
        traversed == State::Idle
            && activated.is_empty()
            && remaining.input.events == vec![pressed(Key::Tab), pressed(Key::KeyChar('/'))]
    );
    let add = make(
        chord(Key::Tab, vec![]),
        Some(Scope::View),
        Vec::new(),
        "graph.add",
        "add",
        Act::Add,
    );
    let (state, matched, remaining) = route_frame(
        &[],
        Platform::Apple,
        &refs(std::slice::from_ref(&add)),
        &Scope::View,
        false,
        frame_with(vec![
            pressed(Key::Tab),
            pressed(Key::KeyChar('/')),
            Event::Text_input(" ".into()),
        ]),
        State::Pending(String::new()),
    );
    assert!(
        state == State::Idle
            && actions_of(&matched) == vec![Act::Add]
            && remaining.input.events
                == vec![pressed(Key::KeyChar('/')), Event::Text_input(" ".into())]
    );
    let (state, matched, remaining) = route_frame(
        &[],
        Platform::Apple,
        &refs(std::slice::from_ref(&add)),
        &Scope::View,
        false,
        frame_with(vec![pressed(Key::Shift), pressed(Key::Tab), pressed(Key::KeyChar('/'))]),
        State::Idle,
    );
    assert!(
        state == State::Idle
            && matched.is_empty()
            && remaining.input.events.contains(&pressed(Key::Tab))
            && remaining.input.events.contains(&pressed(Key::KeyChar('/')))
    );
    let step = |focus: Panel, text_focus: bool, state: State, keys: Vec<Key>| {
        route(
            Platform::Apple,
            &bindings(),
            focus,
            text_focus,
            frame_with(keys.into_iter().map(pressed).collect()),
            state,
        )
    };
    let (state, actions, passed) =
        step(Scope::View, false, State::Idle, vec![Key::KeyChar('/'), Key::KeyChar('g')]);
    assert!(state == State::Idle && actions == vec![Act::Toggle] && passed.is_empty());
    let (state, actions, _) = route(
        Platform::Apple,
        &bindings(),
        Scope::View,
        false,
        frame_with(vec![pressed(Key::KeyChar('/')), Event::Text_input(" ".into())]),
        State::Idle,
    );
    assert!(state == State::Pending(String::new()) && actions.is_empty());
    let (state, actions, passed) = route(
        Platform::Apple,
        &bindings(),
        Scope::View,
        false,
        frame_with(vec![pressed(Key::KeyChar('g')), Event::Text_input("g".into())]),
        State::Pending(String::new()),
    );
    assert!(state == State::Idle && actions == vec![Act::Toggle] && passed.is_empty());
    let (state, _, _) = step(Scope::View, false, State::Idle, vec![Key::KeyChar('/')]);
    assert!(state == State::Pending(String::new()));
    let (state, actions, passed) = step(Scope::View, false, state, vec![Key::Escape]);
    assert!(state == State::Idle && actions.is_empty() && passed.is_empty());
    let (_, actions, _) = step(Scope::View, false, State::Idle, vec![Key::KeyChar('/'), Key::KeyChar('l')]);
    assert!(actions.is_empty());
    let (_, actions, _) = step(Scope::Graph, false, State::Idle, vec![Key::KeyChar('/'), Key::KeyChar('l')]);
    assert!(actions == vec![Act::Layout]);
    // Sequences: a proper prefix opens the next which-key page.
    let (state, actions, _) = step(Scope::View, false, State::Idle, vec![Key::KeyChar('/'), Key::KeyChar('a')]);
    assert!(state == State::Pending("a".into()) && actions.is_empty());
    let (state, actions, _) = step(Scope::View, false, state, vec![Key::KeyChar('L')]);
    assert!(state == State::Idle && actions == vec![Act::AddLight]);
    let (state, actions, _) = step(
        Scope::View,
        false,
        State::Idle,
        vec![Key::KeyChar('/'), Key::KeyChar('a'), Key::KeyChar('q')],
    );
    assert!(state == State::Idle && actions.is_empty());
    let (state, actions, passed) = step(Scope::View, true, State::Idle, vec![Key::KeyChar('/')]);
    assert!(
        state == State::Idle && actions.is_empty() && passed.len() == 1
            && passed == vec![pressed(Key::KeyChar('/'))]
    );
    let (state, _, _) = step(Scope::View, false, State::Idle, vec![Key::KeyChar('/')]);
    let (state, actions, passed) = step(Scope::View, true, state, vec![Key::KeyChar('g')]);
    assert!(
        state == State::Idle && actions.is_empty() && passed == vec![pressed(Key::KeyChar('g'))]
    );
    let chord_route = |focus: Panel, text_focus: bool, keys: Vec<Key>, events: Vec<Event>| {
        route_frame(
            &[],
            Platform::Apple,
            &refs(&b),
            &focus,
            text_focus,
            with_keys(frame_with(events), keys),
            State::Idle,
        )
    };
    let (_, matched, passed) = chord_route(
        Scope::View,
        false,
        vec![Key::Meta],
        vec![Event::Key_pressed(Key::KeyChar('Z')), Event::Text_input("z".into())],
    );
    assert!(actions_of(&matched) == vec![Act::Undo] && passed.input.events.is_empty());
    let (_, matched, _) = chord_route(
        Scope::View,
        false,
        vec![Key::Meta, Key::Shift],
        vec![pressed(Key::KeyChar('z'))],
    );
    assert!(actions_of(&matched) == vec![Act::Redo]);
    let (_, matched, _) = chord_route(
        Scope::View,
        false,
        vec![],
        vec![
            pressed(Key::Meta),
            pressed(Key::KeyChar('z')),
            pressed(Key::Shift),
            pressed(Key::KeyChar('z')),
            Event::Key_released(Key::Shift),
            pressed(Key::KeyChar('z')),
            Event::Key_released(Key::Meta),
        ],
    );
    assert!(actions_of(&matched) == vec![Act::Undo, Act::Redo, Act::Undo]);
    let (_, matched, _) = chord_route(
        Scope::View,
        false,
        vec![Key::Meta],
        vec![pressed(Key::KeyChar('z')), pressed(Key::Meta)],
    );
    assert!(matched.is_empty());
    let (_, matched, _) = route_frame(
        &[Key::Meta],
        Platform::Apple,
        &refs(&b),
        &Scope::View,
        false,
        frame_with(vec![
            pressed(Key::KeyChar('z')),
            Event::Focus_lost,
            pressed(Key::KeyChar('z')),
        ]),
        State::Idle,
    );
    assert!(actions_of(&matched) == vec![Act::Undo]);
    let (_, matched, _) = route_frame(
        &[Key::Meta],
        Platform::Apple,
        &refs(&b),
        &Scope::View,
        false,
        with_keys(
            frame_with(vec![
                pressed(Key::KeyChar('z')),
                pressed(Key::Meta),
                pressed(Key::KeyChar('z')),
            ]),
            vec![Key::Meta],
        ),
        State::Idle,
    );
    assert!(actions_of(&matched) == vec![Act::Undo, Act::Undo]);
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(&b),
        &Scope::View,
        false,
        frame_with(vec![pressed(Key::KeyChar('z')), Event::Key_released(Key::Meta)]),
        State::Idle,
    );
    assert!(matched.is_empty());
    let (_, matched, _) = chord_route(Scope::Graph, false, vec![], vec![pressed(Key::Delete)]);
    assert!(actions_of(&matched) == vec![Act::Delete]);
    let (_, matched, _) = chord_route(Scope::Graph, false, vec![], vec![pressed(Key::KeyChar('F'))]);
    assert!(actions_of(&matched) == vec![Act::Frame]);
    let (_, matched, passed) = chord_route(
        Scope::Graph,
        false,
        vec![Key::Meta],
        vec![pressed(Key::KeyChar('f'))],
    );
    assert!(matched.is_empty() && passed.input.events == vec![pressed(Key::KeyChar('f'))]);
    let (_, matched, passed) = chord_route(Scope::View, false, vec![], vec![pressed(Key::Delete)]);
    assert!(matched.is_empty() && passed.input.events == vec![pressed(Key::Delete)]);
    let (_, matched, passed) = chord_route(
        Scope::View,
        true,
        vec![Key::Meta],
        vec![pressed(Key::KeyChar('z'))],
    );
    assert!(matched.is_empty() && passed.input.events == vec![pressed(Key::KeyChar('z'))]);
    let pointer = Event::Pointer_moved((4., 5.));
    let (ended, passed) = fly_frame(frame_with(vec![pressed(Key::KeyChar('w')), pointer.clone()]));
    assert!(!ended && passed.input.events == vec![pointer]);
    let (ended, passed) = fly_frame(frame_with(vec![
        pressed(Key::KeyChar('/')),
        pressed(Key::KeyChar('g')),
        Event::Text_input("g".into()),
    ]));
    assert!(ended && passed.input.events == vec![pressed(Key::KeyChar('/'))]);
    let (state, actions, passed) = route_frame(
        &[],
        Platform::Apple,
        &refs(&b),
        &Scope::View,
        false,
        passed,
        State::Idle,
    );
    assert!(
        state == State::Pending(String::new()) && actions.is_empty() && passed.input.events.is_empty()
    );
    let (ended, passed) = fly_frame(frame_with(vec![pressed(Key::Space)]));
    assert!(ended && passed.input.events.is_empty());
    let (ended, passed) = fly_frame(frame_with(vec![pressed(Key::Escape)]));
    assert!(ended && passed.input.events.is_empty());
    let (ended, passed) = fly_frame(frame_with(vec![Event::Focus_lost]));
    assert!(ended && passed.input.events == vec![Event::Focus_lost]);
    // E10: a focused text field keeps its own Command chords (undo, above) and lets the others
    // run, but only commands of every pane
    let save = make(
        chord(Key::KeyChar('s'), vec![Key::Meta]),
        None,
        Vec::new(),
        "save",
        "save",
        Act::Palette,
    );
    let scoped = make(
        chord(Key::KeyChar('d'), vec![Key::Meta]),
        Some(Scope::View),
        Vec::new(),
        "dup",
        "duplicate",
        Act::Frame,
    );
    let mut typing_commands = vec![save.clone(), scoped];
    typing_commands.extend(bindings());
    let typing = |events: Vec<Event>| {
        route_frame(
            &[],
            Platform::Apple,
            &refs(&typing_commands),
            &Scope::View,
            true,
            with_keys(frame_with(events), vec![Key::Meta]),
            State::Idle,
        )
    };
    let (_, matched, passed) = typing(vec![pressed(Key::KeyChar('s'))]);
    assert!(matched == vec![save] && passed.input.events.is_empty());
    let (_, matched, passed) = typing(vec![pressed(Key::KeyChar('d'))]);
    assert!(matched.is_empty() && passed.input.events == vec![pressed(Key::KeyChar('d'))]);
    // the leader is /: Space plays and pauses, Shift P resets, a bare / waits for the sequence
    let play = make(chord(Key::Space, vec![]), None, Vec::new(), "play", "play", Act::Frame);
    let reset = make(
        chord(Key::KeyChar('p'), vec![Key::Shift]),
        None,
        Vec::new(),
        "reset",
        "reset",
        Act::Layout,
    );
    let run_cmds = vec![play, reset];
    let run = |events: Vec<Event>| {
        let (state, matched, _) = route_frame(
            &[],
            Platform::Apple,
            &refs(&run_cmds),
            &Scope::View,
            false,
            frame_with(events),
            State::Idle,
        );
        (state, matched.iter().map(|c| c.id.clone()).collect::<Vec<String>>())
    };
    assert!(run(vec![pressed(Key::Space)]) == (State::Idle, vec!["play".to_string()]));
    assert!(
        run(vec![pressed(Key::Shift), pressed(Key::KeyChar('p'))])
            == (State::Idle, vec!["reset".to_string()])
    );
    assert!(run(vec![pressed(Key::KeyChar('/'))]) == (State::Pending(String::new()), vec![]));
    // E17: Shift and a symbol key is the symbol it types: / ? is not / /, and a chord bound to +
    // is reached by Shift =
    let keys = make(leader("?"), None, Vec::new(), "?", "?", Act::Layout);
    let palette = make(leader("/"), None, Vec::new(), "/", "/", Act::Palette);
    let symbols = vec![palette.clone(), keys.clone()];
    let plus = make(
        chord(Key::KeyChar('+'), vec![Key::Meta]),
        None,
        Vec::new(),
        "plus",
        "larger",
        Act::Frame,
    );
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(&symbols),
        &Scope::View,
        false,
        frame_with(vec![
            pressed(Key::KeyChar('/')),
            pressed(Key::Shift),
            pressed(Key::KeyChar('/')),
        ]),
        State::Idle,
    );
    assert!(matched == vec![keys]);
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(&symbols),
        &Scope::View,
        false,
        frame_with(vec![pressed(Key::KeyChar('/')), pressed(Key::KeyChar('/'))]),
        State::Idle,
    );
    assert!(matched == vec![palette]);
    let (_, matched, _) = route_frame(
        &[],
        Platform::Apple,
        &refs(std::slice::from_ref(&plus)),
        &Scope::View,
        false,
        with_keys(frame_with(vec![pressed(Key::KeyChar('='))]), vec![Key::Meta, Key::Shift]),
        State::Idle,
    );
    assert!(matched == vec![plus]);
    println!("editor router: leader/chord scope, text focus, event consumption ok");
}


// The platform is an argument: Command is Meta on Apple and Ctrl elsewhere, in triggers and labels.
#[test]
fn router_platform_modifier_and_labels() {
    let undo = make(
        chord(Key::KeyChar('z'), vec![Key::Meta]),
        None,
        Vec::new(),
        "undo",
        "undo",
        Act::Undo,
    );
    let redo = make(
        chord(Key::KeyChar('z'), vec![Key::Meta, Key::Shift]),
        None,
        Vec::new(),
        "redo",
        "redo",
        Act::Redo,
    );
    let press_cmds = vec![undo, redo];
    let press = |platform: Platform, held: Vec<Key>, key: Key| -> Vec<Act> {
        let frame = with_keys(frame_with(vec![pressed(key)]), held);
        let (_, matched, _) = route_frame(
            &[],
            platform,
            &refs(&press_cmds),
            &Scope::View,
            false,
            frame,
            State::Idle,
        );
        actions_of(&matched)
    };
    assert!(press(Platform::Apple, vec![Key::Meta], Key::KeyChar('z')) == vec![Act::Undo]);
    assert!(press(Platform::Apple, vec![Key::Meta, Key::Shift], Key::KeyChar('z')) == vec![Act::Redo]);
    assert!(press(Platform::Other, vec![Key::Ctrl], Key::KeyChar('z')) == vec![Act::Undo]);
    assert!(press(Platform::Other, vec![Key::Ctrl, Key::Shift], Key::KeyChar('z')) == vec![Act::Redo]);
    assert!(press(Platform::Other, vec![Key::Meta], Key::KeyChar('z')).is_empty());
    assert!(label(Platform::Apple, &Trigger::Chord(Key::KeyChar('s'), vec![Key::Meta])) == "⌘S");
    assert!(label(Platform::Other, &Trigger::Chord(Key::KeyChar('s'), vec![Key::Meta])) == "Ctrl-S");
    assert!(
        label(Platform::Other, &Trigger::Chord(Key::KeyChar('d'), vec![Key::Meta, Key::Shift]))
            == "Ctrl-Shift-D"
    );
    assert!(
        label(Platform::Other, &Trigger::Chord(Key::KeyChar('x'), vec![Key::Meta, Key::Ctrl]))
            == "Ctrl-X"
    );
    println!("editor router: command modifier follows the platform, labels say Ctrl elsewhere ok");
}

#[test]
fn routing_borrows_unchanged_frames_and_owns_only_filtered_events() {
    use std::borrow::Cow;
    let mut frame = frame_with(vec![Event::Pointer_moved((4., 5.))]);
    frame.pixel_scale = (2., 3.);
    frame.fps = 120.;
    frame.mouse_delta = (1., 2.);
    let (_, actions, routed) = router::step(&[], Platform::Apple, &[], &Panel::Graph, false, &frame, State::Idle);
    assert!(actions.is_empty() && matches!(routed, Cow::Borrowed(_)));
    assert!(std::ptr::eq(routed.as_ref(), &frame));
    assert!(matches!(router::fly(&frame).1, Cow::Borrowed(_)));
    frame.input.events.insert(0, pressed(Key::KeyChar('/')));
    let before = input::identity(&frame.input);
    let (state, _, routed) = router::step(&[], Platform::Apple, &[], &Panel::Graph, false, &frame, State::Idle);
    assert!(matches!(state, State::Pending(_)) && matches!(routed, Cow::Owned(_)));
    assert_eq!(routed.input.events, [Event::Pointer_moved((4., 5.))]);
    assert_eq!((routed.pixel_scale, routed.fps, routed.mouse_delta), (frame.pixel_scale, frame.fps, frame.mouse_delta));
    assert_eq!(input::identity(&frame.input), before);
}
