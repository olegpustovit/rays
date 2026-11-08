//! Port of lib/editor_core/editor_core.ml: Keymap, Guide_context, Command and Router (History is specialised in rays_editor::editor_history: it names the editor Document).
//! Each OCaml submodule is a Rust module with the same name; its main type is `t` (or the OCaml
//! type name where it has one).

pub mod keymap {
    use input::{Key, Platform};

    /// A key trigger: a leader sequence typed after `/`, or a chord with modifiers.
    #[derive(Clone, Debug, PartialEq)]
    pub enum Trigger {
        Leader(String),
        Chord(Key, Vec<Key>),
    }

    /// What a symbol key types with Shift held (the US layout).
    pub(crate) fn shifted(c: char) -> Option<char> {
        match c {
            '/' => Some('?'),
            '=' => Some('+'),
            '-' => Some('_'),
            ';' => Some(':'),
            '\'' => Some('"'),
            ',' => Some('<'),
            '.' => Some('>'),
            '[' => Some('{'),
            ']' => Some('}'),
            '\\' => Some('|'),
            '`' => Some('~'),
            '1' => Some('!'),
            '2' => Some('@'),
            '3' => Some('#'),
            '4' => Some('$'),
            '5' => Some('%'),
            '6' => Some('^'),
            '7' => Some('&'),
            '8' => Some('*'),
            '9' => Some('('),
            '0' => Some(')'),
            _ => None,
        }
    }

    /// In a trigger, Meta is the platform's primary modifier: Command on Apple, Ctrl elsewhere.
    pub fn resolve(platform: Platform, key: Key) -> Key {
        if key == Key::Meta { input::primary(platform) } else { key }
    }

    /// Shared key spelling for guides, which-key and command feedback.
    pub fn label(platform: Platform, trigger: &Trigger) -> String {
        match trigger {
            Trigger::Leader(sequence) => {
                let keys: Vec<String> = sequence.chars().map(|c| c.to_string()).collect();
                format!("/ {}", keys.join(" "))
            }
            Trigger::Chord(key, modifiers) => {
                let mut resolved: Vec<Key> = Vec::new();
                for m in modifiers.iter().map(|m| resolve(platform, *m)) {
                    if !resolved.contains(&m) {
                        resolved.push(m);
                    }
                }
                let (key, modifiers) = match key {
                    Key::KeyChar(c)
                        if resolved.contains(&Key::Shift) && shifted(*c).is_some() =>
                    {
                        let typed = shifted(*c).unwrap_or(*c);
                        (Key::KeyChar(typed), resolved.into_iter().filter(|m| *m != Key::Shift).collect::<Vec<_>>())
                    }
                    _ => (*key, resolved),
                };
                // the sheets' vocabulary: modifier glyphs before the key with no dash, a letter
                // under a modifier in capitals (⌘G, ⇧D), ↵ ⇥ ⌫ and esc
                let key_text: String = match key {
                    Key::KeyChar(c) => {
                        if modifiers.is_empty() {
                            c.to_ascii_lowercase().to_string()
                        } else {
                            c.to_ascii_uppercase().to_string()
                        }
                    }
                    Key::ArrowUp => "↑".into(),
                    Key::ArrowDown => "↓".into(),
                    Key::ArrowLeft => "←".into(),
                    Key::ArrowRight => "→".into(),
                    Key::Space => "Space".into(),
                    Key::Enter => "↵".into(),
                    Key::Escape => "esc".into(),
                    Key::Backspace => "⌫".into(),
                    Key::Tab => "⇥".into(),
                    Key::Home => "Home".into(),
                    Key::End => "End".into(),
                    Key::PageUp => "PgUp".into(),
                    Key::PageDown => "PgDn".into(),
                    Key::Insert => "Ins".into(),
                    Key::Delete => "⌦".into(),
                    Key::Shift => "⇧".into(),
                    Key::Ctrl => "⌃".into(),
                    Key::Alt => "⌥".into(),
                    Key::Meta => "⌘".into(),
                    Key::F1 => "F1".into(),
                    Key::F2 => "F2".into(),
                    Key::F3 => "F3".into(),
                    Key::F4 => "F4".into(),
                    Key::F5 => "F5".into(),
                    Key::F6 => "F6".into(),
                    Key::F7 => "F7".into(),
                    Key::F8 => "F8".into(),
                    Key::F9 => "F9".into(),
                    Key::F10 => "F10".into(),
                    Key::F11 => "F11".into(),
                    Key::F12 => "F12".into(),
                    Key::Unknown(code) => format!("Key {}", code),
                };
                let glyphs: &[(Key, &str)] = match platform {
                    Platform::Apple => &[(Key::Meta, "⌘"), (Key::Ctrl, "⌃"), (Key::Alt, "⌥"), (Key::Shift, "⇧")],
                    Platform::Other => &[(Key::Ctrl, "Ctrl-"), (Key::Alt, "Alt-"), (Key::Shift, "Shift-")],
                };
                let mut out = String::new();
                for (modifier, name) in glyphs {
                    if modifiers.contains(modifier) {
                        out.push_str(name);
                    }
                }
                out.push_str(&key_text);
                out
            }
        }
    }
}

pub mod guide_context {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum GuideContext {
        Canvas,
        Node,
        Multi,
        Hints,
        Leader,
        Search,
        List,
        Text,
    }

    pub fn name(context: GuideContext) -> &'static str {
        match context {
            GuideContext::Canvas => "Canvas",
            GuideContext::Node => "Node",
            GuideContext::Multi => "Selection",
            GuideContext::Hints => "Hints",
            GuideContext::Leader => "Leader",
            GuideContext::Search => "Search",
            GuideContext::List => "List",
            GuideContext::Text => "Text",
        }
    }
}

/// Named editor commands: the one table behind key routing, which-key, and the command palette.
pub mod command {
    use crate::editor_core::guide_context;
    use crate::editor_core::keymap::Trigger;
    use crate::panels::Panel;

    /// The scope is a kind of panel. `action` is the host's own code for what the command does (an
    /// index into its action table; editor_core cannot name the host's action type, so the table
    /// stays with the host).
    #[derive(Clone, Debug, PartialEq)]
    pub struct Command {
        /// Stable, e.g. "edit.undo"; entries sharing an id are one command.
        pub id: String,
        /// Shown in which-key and the palette.
        pub label: String,
        /// `None`: palette only.
        pub trigger: Option<Trigger>,
        /// `None`: global; else only while that pane has focus.
        pub scope: Option<Panel>,
        /// Contexts where the guide strip lists this command.
        pub guide: Vec<guide_context::GuideContext>,
        pub action: usize,
    }

    /// The OCaml optional arguments `?trigger ?scope ?guide` are positional here.
    pub fn make(
        trigger: Option<Trigger>,
        scope: Option<Panel>,
        guide: Vec<guide_context::GuideContext>,
        id: &str,
        label: &str,
        action: usize,
    ) -> Command {
        Command { id: id.into(), label: label.into(), trigger, scope, guide, action }
    }

    /// Applicable commands in their original table order.
    pub fn for_guide(commands: &[Command], focus: &Panel, context: guide_context::GuideContext) -> Vec<Command> {
        commands
            .iter()
            .filter(|command| {
                command.trigger.is_some()
                    && (command.scope.is_none() || command.scope.as_ref() == Some(focus))
                    && command.guide.contains(&context)
            })
            .cloned()
            .collect()
    }
}


pub mod router {
    use super::command::Command as Command;
    use crate::panels::Panel;
    use super::keymap::{resolve, shifted, Trigger};
    use crate::frame;
    use input::{Event, Key, Platform};

    /// Tab cancels leader routing, runs its exact-modifier host binding or passes to UI traversal,
    /// then hands the remaining ordered events to the UI (including a same-frame /).
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum State {
        Idle,
        /// The leader keys typed so far.
        Pending(String),
    }

    /// Whether a pane with this focus may run the command.
    fn visible(command: &Command, focus: &Panel) -> bool {
        command.scope.is_none() || command.scope.as_ref() == Some(focus)
    }

    fn same_key(a: Key, b: Key) -> bool {
        match (a, b) {
            (Key::KeyChar(a), Key::KeyChar(b)) => a.to_ascii_lowercase() == b.to_ascii_lowercase(),
            _ => a == b,
        }
    }

    /// The matching chord with the most modifiers; on a tie the earlier command wins.
    fn chord(platform: Platform, commands: &[&Command], focus: &Panel, keys: &[Key], key: Key) -> Option<Command> {
        let mut best: Option<(usize, &Command)> = None;
        for command in commands.iter().copied().filter(|command| visible(command, focus)) {
            let Some(Trigger::Chord(bound, modifiers)) = &command.trigger else { continue };
            let modifiers: Vec<Key> = modifiers.iter().map(|m| resolve(platform, *m)).collect();
            let matches = same_key(*bound, key)
                && modifiers.iter().all(|m| keys.contains(m))
                && (*bound != Key::Tab
                    || [Key::Shift, Key::Alt].iter().all(|m| keys.contains(m) == modifiers.contains(m)))
                && [Key::Meta, Key::Ctrl].iter().all(|m| !keys.contains(m) || modifiers.contains(m));
            if matches {
                let count = modifiers.len();
                match best {
                    Some((best_count, _)) if best_count >= count => {}
                    _ => best = Some((count, command)),
                }
            }
        }
        best.map(|(_, command)| command.clone())
    }

    /// In fly mode, keep pointer/window events and pass / to the leader router after ending the
    /// mode. Escape ends fly without opening a shortcut.
    pub fn fly(mut frame: frame::Frame) -> (bool, frame::Frame) {
        let exits = frame.input.events.iter().any(|event| {
            matches!(
                event,
                Event::Key_pressed(Key::Escape | Key::Space | Key::KeyChar('/')) | Event::Focus_lost
            )
        });
        frame.input.events.retain(|event| match event {
            Event::Key_pressed(Key::KeyChar('/')) | Event::Focus_lost => true,
            Event::Key_pressed(_) | Event::Key_released(_) | Event::Text_input(_) | Event::Text_editing { .. } => {
                false
            }
            _ => true,
        });
        (exits, frame)
    }

    fn modifier(key: Key) -> bool {
        matches!(key, Key::Shift | Key::Ctrl | Key::Alt | Key::Meta)
    }

    /// A text field owns the keyboard except the Command/Ctrl chords it has no use for (its own
    /// are select all, copy, cut, paste, undo and redo). Only commands of every pane run.
    fn text_chords(
        platform: Platform,
        keymap: &[&Command],
        focus: &Panel,
        previous_keys: &[Key],
        mut frame: frame::Frame,
    ) -> (Vec<Command>, frame::Frame) {
        let global: Vec<&Command> =
            keymap.iter().copied().filter(|command| command.scope.is_none()).collect();
        let owned = |key: Key| match key {
            Key::KeyChar(c) => ['a', 'c', 'v', 'x', 'z', 'y'].contains(&c.to_ascii_lowercase()),
            Key::F12 => false,
            _ => true,
        };
        let mut modifiers = input::keys_before(previous_keys, &frame.input.keys, &frame.input.events);
        let mut actions = Vec::new();
        let mut passed = Vec::new();
        for event in frame.input.events.iter() {
            modifiers = input::keys_after(&modifiers, event);
            match event {
                Event::Key_pressed(key)
                    if !owned(*key) && (input::command_held(platform, &modifiers) || *key == Key::F12) =>
                {
                    match chord(platform, &global, focus, &modifiers, *key) {
                        Some(command) => actions.push(command),
                        None => passed.push(event.clone()),
                    }
                }
                _ => passed.push(event.clone()),
            }
        }
        frame.input.events = passed;
        (actions, frame)
    }

    /// One routing step over the frame's events. Returns the exact matched commands in event
    /// order, the new state, and the frame with the consumed events removed.
    ///
    /// `previous_keys` supplies the previous frame's keys for changed modifiers and focus
    /// cancellation; pass `&[]` when none were held.
    pub fn step(
        previous_keys: &[Key],
        platform: Platform,
        keymap: &[&Command],
        focus: &Panel,
        text_focus: bool,
        mut frame: frame::Frame,
        state: State,
    ) -> (State, Vec<Command>, frame::Frame) {
        if text_focus {
            let (actions, frame) = text_chords(platform, keymap, focus, previous_keys, frame);
            return (State::Idle, actions, frame);
        }
        let mut modifiers = input::keys_before(previous_keys, &frame.input.keys, &frame.input.events);
        let mut traversing = false;
        let mut state = state;
        let mut actions: Vec<Command> = Vec::new();
        let mut passed: Vec<Event> = Vec::new();
        for event in frame.input.events.iter() {
            modifiers = input::keys_after(&modifiers, event);
            let command = input::command_held(platform, &modifiers);
            if traversing {
                state = State::Idle;
                passed.push(event.clone());
                continue;
            }
            let idle = state == State::Idle;
            let prefix = match &state {
                State::Pending(prefix) => prefix.clone(),
                State::Idle => String::new(),
            };
            match event {
                // Tab cancels leader routing and runs its exact-modifier binding, or passes to
                // UI traversal.
                Event::Key_pressed(Key::Tab) if !command => {
                    traversing = true;
                    state = State::Idle;
                    match chord(platform, keymap, focus, &modifiers, Key::Tab) {
                        None => passed.push(event.clone()),
                        Some(action) => actions.push(action),
                    }
                }
                // a bare / (no Shift, no Command) opens the leader
                Event::Key_pressed(Key::KeyChar('/'))
                    if idle && !text_focus && !command && !modifiers.contains(&Key::Shift) =>
                {
                    state = State::Pending(String::new());
                }
                Event::Key_pressed(key) if idle && !text_focus => {
                    // Shift and a symbol key is the symbol it types, when a chord is bound to that
                    let typed = match key {
                        Key::KeyChar(c) if modifiers.contains(&Key::Shift) => {
                            shifted(*c).and_then(|s| chord(platform, keymap, focus, &modifiers, Key::KeyChar(s)))
                        }
                        _ => None,
                    };
                    match typed.or_else(|| chord(platform, keymap, focus, &modifiers, *key)) {
                        Some(action) => actions.push(action),
                        None => passed.push(event.clone()),
                    }
                    state = State::Idle;
                }
                Event::Text_input(_) if idle && !actions.is_empty() => {
                    state = State::Idle;
                }
                _ if idle => {
                    state = State::Idle;
                    passed.push(event.clone());
                }
                Event::Key_pressed(key) if modifier(*key) => {}
                Event::Key_pressed(Key::KeyChar(character)) => {
                    // An exact sequence runs; a proper prefix opens the next page.
                    // / then Shift-/ is the sequence "?", as the sheets write it
                    let character = if modifiers.contains(&Key::Shift) {
                        shifted(*character).unwrap_or(*character)
                    } else {
                        *character
                    };
                    let typed = format!("{}{}", prefix, character.to_ascii_lowercase());
                    let commands: Vec<&Command> = keymap.iter().copied().filter(|command| visible(command, focus)).collect();
                    let exact = Trigger::Leader(typed.clone());
                    if let Some(command) = commands.iter().find(|command| command.trigger.as_ref() == Some(&exact)) {
                        actions.push((*command).clone());
                        state = State::Idle;
                    } else if commands.iter().any(|command| {
                        matches!(&command.trigger, Some(Trigger::Leader(sequence)) if sequence.starts_with(&typed))
                    }) {
                        state = State::Pending(typed);
                    } else {
                        state = State::Idle;
                    }
                }
                Event::Text_input(_) | Event::Text_editing { .. } => {}
                Event::Key_pressed(_) | Event::Pointer_pressed(..) => {
                    state = State::Idle;
                }
                Event::Focus_lost => {
                    state = State::Idle;
                    passed.push(event.clone());
                }
                _ => passed.push(event.clone()),
            }
        }
        frame.input.events = passed;
        (state, actions, frame)
    }
}
