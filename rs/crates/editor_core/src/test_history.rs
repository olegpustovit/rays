//! Port of lib/editor_core/test_history.ml: History, the Router and the platform-modifier labels.
use crate::command;
use crate::history::{self as hist, Merge};
use crate::keymap::{label, Trigger};
use crate::router::{self, State};
use input::{Event, Key, Platform};
use rays::frame;
use std::sync::Arc;

type H = hist::t<i64, i64>;

fn restore_ok(entry: &i64) -> Result<i64, String> {
    Ok(*entry)
}

fn new_h(capacity: Option<i64>, present: i64) -> H {
    let keep: Arc<dyn Fn(&i64) -> i64> = Arc::new(|a: &i64| *a);
    hist::create(capacity, None, None, keep, Arc::new(present)).unwrap()
}

fn record(merge: Merge, label: &str, value: i64, h: H) -> H {
    hist::record(merge, label, Arc::new(value), h)
}

fn step(value: i64, h: H) -> H {
    record(Merge::Step, "Edit", value, h)
}

fn undo(h: &H) -> Option<H> {
    hist::undo(&restore_ok, h).map(|r| r.unwrap())
}

fn redo(h: &H) -> Option<H> {
    hist::redo(&restore_ok, h).map(|r| r.unwrap())
}

fn present(h: &H) -> i64 {
    *hist::present(h)
}

fn depth(h: &H) -> i64 {
    hist::depth(h)
}

#[test]
fn history_merge_seal_undo_redo_capacity_labels() {
    let h = new_h(Some(3), 0);
    let h = step(1, h);
    let h = step(2, h);
    let h = record(Merge::Repair, "Edit", 3, h);
    assert!(present(&h) == 3 && depth(&h) == 2);
    let h = undo(&h).unwrap();
    assert!(present(&h) == 1 && hist::can_redo(&h));
    let h = redo(&h).unwrap();
    assert!(present(&h) == 3 && !hist::can_redo(&h));
    let h = step(4, h);
    let h = step(5, h);
    let h = step(6, h);
    assert!(depth(&h) == 3);
    // the OCaml test walks to the bottom but continues from the original h
    let mut bottom = h.clone();
    while let Some(next) = undo(&bottom) {
        bottom = next;
    }
    assert!(present(&bottom) == 3 && !hist::can_undo(&bottom));
    let h = record(Merge::Gesture("move/7".into()), "Edit", 7, h);
    let h = record(Merge::Gesture("move/7".into()), "Edit", 8, h);
    assert!(present(&h) == 8 && depth(&h) == 3);
    let h = hist::seal(false, h);
    let h = record(Merge::Gesture("move/7".into()), "Edit", 9, h);
    assert!(present(&undo(&h).unwrap()) == 8);
    let burst = |at: f64, value: i64, h: H| {
        record(Merge::Burst { key: "view".into(), at, window: 0.25 }, "Edit", value, h)
    };
    let h = burst(1.2, 2, burst(1., 1, new_h(None, 0)));
    assert!(depth(&h) == 1 && present(&undo(&h).unwrap()) == 0);
    let h = burst(1.5, 3, h);
    assert!(depth(&h) == 2 && present(&undo(&h).unwrap()) == 2);
    let h = undo(&h).unwrap();
    // a repair is no edit: it leaves what redo restores (E12)
    let h = record(Merge::Repair, "Edit", 10, h);
    assert!(present(&h) == 10 && hist::can_redo(&h) && present(&redo(&h).unwrap()) == 3);
    // Labels name what undo reverts and what redo reapplies.
    let h = record(Merge::Step, "Move", 2, record(Merge::Step, "Connect", 1, new_h(None, 0)));
    assert!(hist::label(&h) == "Move");
    let h = undo(&h).unwrap();
    assert!(hist::label(&h) == "Connect" && hist::redo_label(&h) == Some("Move"));
    // The cap counts exactly the entries to undo and to redo, at every step of any sequence.
    let bytes_fn: Arc<dyn Fn(&i64) -> i64> = Arc::new(|n: &i64| 10 + *n);
    let keep: Arc<dyn Fn(&i64) -> i64> = Arc::new(|a: &i64| *a);
    let check = |h: &H| {
        let (entries, _) = hist::retained(h);
        assert!(entries.iter().map(|n| 10 + **n).sum::<i64>() == hist::byte_depth(h));
        assert!(hist::byte_depth(h) >= 0);
    };
    let h = hist::create(None, Some(200), Some(bytes_fn.clone()), keep.clone(), Arc::new(0)).unwrap();
    assert!(hist::byte_depth(&h) == 0);
    let h = step(1, h);
    check(&h);
    assert!(hist::byte_depth(&h) == 10);
    let h = step(2, h);
    check(&h);
    let h = record(Merge::Gesture("a".into()), "Edit", 3, h);
    check(&h);
    let at_three = hist::byte_depth(&h);
    let h = record(Merge::Gesture("a".into()), "Edit", 4, h);
    check(&h);
    assert!(hist::byte_depth(&h) == at_three);
    let h = undo(&h).unwrap();
    check(&h);
    assert!(present(&h) == 2);
    let h = undo(&h).unwrap();
    check(&h);
    let h = redo(&h).unwrap();
    check(&h);
    // a commit abandons what redo held, releasing it once
    let h = step(50, h);
    check(&h);
    assert!(!hist::can_redo(&h));
    // the byte cap goes oldest first and keeps the newest
    let mut h = h;
    for n in [60, 70, 80, 90, 100, 110] {
        h = step(n, h);
        check(&h);
    }
    assert!(hist::byte_depth(&h) <= 200 && hist::can_undo(&h));
    let huge = hist::create(None, Some(1), Some(bytes_fn.clone()), keep.clone(), Arc::new(1000)).unwrap();
    let huge = step(1001, huge);
    assert!(depth(&huge) == 1 && hist::byte_depth(&huge) == 10 + 1000);
    let huge = hist::release(huge);
    assert!(depth(&huge) == 0 && hist::byte_depth(&huge) == 0 && present(&huge) == 1001);
    check(&h);
    // the retreat's step: only the oldest go
    let shed_from = depth(&h);
    let h = step(7, h);
    let h = step(8, h);
    let h = step(9, h);
    let shed_from = shed_from.max(depth(&h));
    let h = hist::shed(2, h);
    check(&h);
    assert!(depth(&h) == 2 && shed_from > 2);
    let h = hist::restart(Arc::new(5), h);
    check(&h);
    assert!(present(&h) == 5 && depth(&h) == 0 && !hist::can_undo(&h) && hist::byte_depth(&h) == 0);

    // an entry is only what [keep] took; the last present stays live, older ones come from [restore]
    use std::cell::RefCell;
    let restored: RefCell<Vec<String>> = RefCell::new(Vec::new());
    let restore = |entry: &String| -> Result<i64, String> {
        restored.borrow_mut().insert(0, entry.clone());
        if entry == "refuse" { Err("no".into()) } else { Ok(entry.len() as i64) }
    };
    let string_keep: Arc<dyn Fn(&i64) -> String> = Arc::new(|n: &i64| "x".repeat(*n as usize));
    let h: hist::t<i64, String> = hist::create(None, None, None, string_keep, Arc::new(1)).unwrap();
    let h = hist::record(Merge::Step, "Edit", Arc::new(2), h);
    let h = hist::record(Merge::Step, "Edit", Arc::new(3), h);
    let back = |h: &hist::t<i64, String>| hist::undo(&restore, h).unwrap().unwrap();
    let forth = |h: &hist::t<i64, String>| hist::redo(&restore, h).unwrap().unwrap();
    let h = back(&h);
    assert!(*hist::present(&h) == 2 && restored.borrow().is_empty());
    let h = forth(&h);
    assert!(*hist::present(&h) == 3 && restored.borrow().is_empty());
    let h = back(&back(&h));
    assert!(*hist::present(&h) == 1 && *restored.borrow() == vec!["x".to_string()]);

    let bad_keep: Arc<dyn Fn(&i64) -> String> = Arc::new(|_: &i64| "refuse".to_string());
    let bad: hist::t<i64, String> = hist::create(None, None, None, bad_keep, Arc::new(1)).unwrap();
    let bad = hist::record(Merge::Step, "Edit", Arc::new(2), bad);
    let bad = hist::record(Merge::Step, "Edit", Arc::new(3), bad);
    let bad = hist::undo(&|_: &String| Ok::<i64, String>(9), &bad).unwrap().unwrap();
    assert!(*hist::present(&bad) == 2);
    let bad = hist::record(Merge::Step, "Edit", Arc::new(4), bad);
    match hist::undo(&restore, &bad) {
        Some(Ok(_)) => {}
        _ => panic!("expected the last present to revive without restore"),
    }
    println!("editor history: merge, seal, undo/redo, capacity, labels ok");
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Scope {
    View,
    Graph,
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

type Cmd = command::t<Scope, Act>;

fn cmd(id: &str, label: &str, trigger: Option<Trigger>, scope: Option<Scope>, action: Act) -> Cmd {
    command::make(trigger, scope, Vec::new(), id, label, action)
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

fn frame_with(events: Vec<Event>) -> frame::t {
    let mut input = input::at_time(0.);
    input.frame = 0;
    input.tick = 0;
    input.size = (10, 10);
    input.events = events;
    frame::t { input, pixel_scale: (1., 1.), fps: 0., mouse_delta: (0., 0.) }
}

fn with_keys(mut frame: frame::t, keys: Vec<Key>) -> frame::t {
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
    focus: Scope,
    text_focus: bool,
    frame: frame::t,
    state: State,
) -> (State, Vec<Act>, Vec<Event>) {
    let (state, matched, frame) =
        router::step(&[], platform, &refs(commands), &focus, text_focus, frame, state);
    (state, matched.iter().map(|c| c.action).collect(), frame.input.events)
}

fn actions_of(matched: &[&Cmd]) -> Vec<Act> {
    matched.iter().map(|c| c.action).collect()
}

#[test]
fn router_leader_chord_scope_text_focus_and_event_consumption() {
    let b = bindings();
    let alias = |scope: Option<Scope>, trigger: Option<Trigger>, label: &str| {
        command::make(trigger, scope, Vec::new(), "alias", label, Act::Frame)
    };
    let meta = Some(Trigger::Chord(Key::KeyChar('c'), vec![Key::Meta]));
    let view = alias(Some(Scope::View), meta.clone(), "view alias");
    let graph = alias(Some(Scope::Graph), meta.clone(), "graph alias");
    let ctrl = alias(None, chord(Key::KeyChar('c'), vec![Key::Ctrl]), "Ctrl alias");
    let qj = alias(None, leader("qj"), "first leader alias");
    let qq = alias(None, leader("qq"), "second leader alias");
    let aliases = vec![view, graph.clone(), ctrl.clone(), qj.clone(), qq.clone()];
    let (_, matched, _) = router::step(
        &[],
        Platform::Apple,
        &refs(&aliases),
        &Scope::Graph,
        false,
        with_keys(frame_with(vec![pressed(Key::KeyChar('c'))]), vec![Key::Meta]),
        State::Idle,
    );
    assert!(matched == vec![&graph]);
    let (_, matched, _) = router::step(
        &[],
        Platform::Apple,
        &refs(&aliases),
        &Scope::Graph,
        false,
        with_keys(frame_with(vec![pressed(Key::KeyChar('c'))]), vec![Key::Ctrl]),
        State::Idle,
    );
    assert!(matched == vec![&ctrl]);
    let (_, matched, _) = router::step(
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
    assert!(matched == vec![&qq]);
    let (traversed, activated, remaining) = router::step(
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
    let add = command::make(
        chord(Key::Tab, vec![]),
        Some(Scope::View),
        Vec::new(),
        "graph.add",
        "add",
        Act::Add,
    );
    let (state, matched, remaining) = router::step(
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
    let (state, matched, remaining) = router::step(
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
    let step = |focus: Scope, text_focus: bool, state: State, keys: Vec<Key>| {
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
    let chord_route = |focus: Scope, text_focus: bool, keys: Vec<Key>, events: Vec<Event>| {
        router::step(
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
    let (_, matched, _) = router::step(
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
    let (_, matched, _) = router::step(
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
    let (_, matched, _) = router::step(
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
    let (ended, passed) = router::fly(frame_with(vec![pressed(Key::KeyChar('w')), pointer.clone()]));
    assert!(!ended && passed.input.events == vec![pointer]);
    let (ended, passed) = router::fly(frame_with(vec![
        pressed(Key::KeyChar('/')),
        pressed(Key::KeyChar('g')),
        Event::Text_input("g".into()),
    ]));
    assert!(ended && passed.input.events == vec![pressed(Key::KeyChar('/'))]);
    let (state, actions, passed) = router::step(
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
    let (ended, passed) = router::fly(frame_with(vec![pressed(Key::Space)]));
    assert!(ended && passed.input.events.is_empty());
    let (ended, passed) = router::fly(frame_with(vec![pressed(Key::Escape)]));
    assert!(ended && passed.input.events.is_empty());
    let (ended, passed) = router::fly(frame_with(vec![Event::Focus_lost]));
    assert!(ended && passed.input.events == vec![Event::Focus_lost]);
    // E10: a focused text field keeps its own Command chords (undo, above) and lets the others
    // run, but only commands of every pane
    let save = command::make(
        chord(Key::KeyChar('s'), vec![Key::Meta]),
        None,
        Vec::new(),
        "save",
        "save",
        Act::Palette,
    );
    let scoped = command::make(
        chord(Key::KeyChar('d'), vec![Key::Meta]),
        Some(Scope::View),
        Vec::new(),
        "dup",
        "duplicate",
        Act::Frame,
    );
    let mut typing_commands = vec![save.clone(), scoped.clone()];
    typing_commands.extend(bindings());
    let typing = |events: Vec<Event>| {
        router::step(
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
    assert!(matched == vec![&save] && passed.input.events.is_empty());
    let (_, matched, passed) = typing(vec![pressed(Key::KeyChar('d'))]);
    assert!(matched.is_empty() && passed.input.events == vec![pressed(Key::KeyChar('d'))]);
    // the leader is /: Space plays and pauses, Shift P resets, a bare / waits for the sequence
    let play = command::make(chord(Key::Space, vec![]), None, Vec::new(), "play", "play", Act::Frame);
    let reset = command::make(
        chord(Key::KeyChar('p'), vec![Key::Shift]),
        None,
        Vec::new(),
        "reset",
        "reset",
        Act::Layout,
    );
    let run_cmds = vec![play.clone(), reset.clone()];
    let run = |events: Vec<Event>| {
        let (state, matched, _) = router::step(
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
    let keys = command::make(leader("?"), None, Vec::new(), "?", "?", Act::Layout);
    let palette = command::make(leader("/"), None, Vec::new(), "/", "/", Act::Palette);
    let symbols = vec![palette.clone(), keys.clone()];
    let plus = command::make(
        chord(Key::KeyChar('+'), vec![Key::Meta]),
        None,
        Vec::new(),
        "plus",
        "larger",
        Act::Frame,
    );
    let (_, matched, _) = router::step(
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
    assert!(matched == vec![&keys]);
    let (_, matched, _) = router::step(
        &[],
        Platform::Apple,
        &refs(&symbols),
        &Scope::View,
        false,
        frame_with(vec![pressed(Key::KeyChar('/')), pressed(Key::KeyChar('/'))]),
        State::Idle,
    );
    assert!(matched == vec![&palette]);
    let (_, matched, _) = router::step(
        &[],
        Platform::Apple,
        &refs(std::slice::from_ref(&plus)),
        &Scope::View,
        false,
        with_keys(frame_with(vec![pressed(Key::KeyChar('='))]), vec![Key::Meta, Key::Shift]),
        State::Idle,
    );
    assert!(matched == vec![&plus]);
    println!("editor router: leader/chord scope, text focus, event consumption ok");
}


// The platform is an argument: Command is Meta on Apple and Ctrl elsewhere, in triggers and labels.
#[test]
fn router_platform_modifier_and_labels() {
    let undo = command::make(
        chord(Key::KeyChar('z'), vec![Key::Meta]),
        None,
        Vec::new(),
        "undo",
        "undo",
        Act::Undo,
    );
    let redo = command::make(
        chord(Key::KeyChar('z'), vec![Key::Meta, Key::Shift]),
        None,
        Vec::new(),
        "redo",
        "redo",
        Act::Redo,
    );
    let press_cmds = vec![undo.clone(), redo.clone()];
    let press = |platform: Platform, held: Vec<Key>, key: Key| -> Vec<Act> {
        let frame = with_keys(frame_with(vec![pressed(key)]), held);
        let (_, matched, _) = router::step(
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
