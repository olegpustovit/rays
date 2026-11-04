//! Port of lib/editor_core/editor_core.ml: History, Keymap, Guide_context, Command and Router.
//! Each OCaml submodule is a Rust module with the same name; its main type is `t` (or the OCaml
//! type name where it has one).

pub mod history {
    use std::collections::VecDeque;
    use std::sync::Arc;

    /// How a record merges with the entry it follows.
    #[derive(Clone, Debug, PartialEq)]
    pub enum Merge {
        Step,
        Gesture(String),
        Burst { key: String, at: f64, window: f64 },
        Repair,
    }

    /// The present is a live value behind an [`Arc`]; an entry keeps only what [`keep`] takes
    /// from it. Arc identity stands in for OCaml's physical equality (`==`).
    ///
    /// [`keep`]: t::keep
    pub struct t<A, E> {
        /// Oldest first; the newest entry is at the back.
        past: VecDeque<(Arc<E>, String, i64)>,
        present: Arc<A>,
        label: String,
        /// The next redo is last.
        future: Vec<(Arc<E>, String, i64)>,
        /// The last present to become an entry, kept live beside the entries.
        recent: Option<(Arc<E>, Arc<A>)>,
        capacity: i64,
        byte_capacity: i64,
        keep: Arc<dyn Fn(&A) -> E>,
        size: Arc<dyn Fn(&E) -> i64>,
        depth: i64,
        byte_depth: i64,
        merge: Option<Merge>,
    }

    // ponytail: every field is an Arc or a plain value, so a clone is cheap and undo/redo can
    // return the old history unchanged on Error without OCaml's immutable sharing.
    impl<A, E> Clone for t<A, E> {
        fn clone(&self) -> Self {
            t {
                past: self.past.clone(),
                present: self.present.clone(),
                label: self.label.clone(),
                future: self.future.clone(),
                recent: self.recent.clone(),
                capacity: self.capacity,
                byte_capacity: self.byte_capacity,
                keep: self.keep.clone(),
                size: self.size.clone(),
                depth: self.depth,
                byte_depth: self.byte_depth,
                merge: self.merge.clone(),
            }
        }
    }

    /// [capacity] defaults to 128, [byte_capacity] to unbounded, [bytes] to 0.
    pub fn create<A, E>(
        capacity: Option<i64>,
        byte_capacity: Option<i64>,
        bytes: Option<Arc<dyn Fn(&E) -> i64>>,
        keep: Arc<dyn Fn(&A) -> E>,
        present: Arc<A>,
    ) -> Result<t<A, E>, String> {
        let capacity = capacity.unwrap_or(128);
        let byte_capacity = byte_capacity.unwrap_or(i64::MAX);
        if capacity < 1 {
            return Err("Editor_core.History.create: capacity must be positive".into());
        }
        if byte_capacity < 0 {
            return Err("Editor_core.History.create: byte_capacity must be non-negative".into());
        }
        Ok(t {
            past: VecDeque::new(),
            present,
            label: String::new(),
            future: Vec::new(),
            recent: None,
            capacity,
            byte_capacity,
            keep,
            size: bytes.unwrap_or_else(|| Arc::new(|_: &E| 0)),
            depth: 0,
            byte_depth: 0,
            merge: None,
        })
    }

    pub fn present<A, E>(h: &t<A, E>) -> &A {
        &h.present
    }

    pub fn label<A, E>(h: &t<A, E>) -> &str {
        &h.label
    }

    /// The label of the next redo, if any.
    pub fn redo_label<A, E>(h: &t<A, E>) -> Option<&str> {
        h.future.last().map(|(_, label, _)| label.as_str())
    }

    pub fn can_undo<A, E>(h: &t<A, E>) -> bool {
        !h.past.is_empty()
    }

    pub fn can_redo<A, E>(h: &t<A, E>) -> bool {
        !h.future.is_empty()
    }

    pub fn depth<A, E>(h: &t<A, E>) -> i64 {
        h.depth
    }

    /// What the byte cap counts: the entries to undo and to redo.
    pub fn byte_depth<A, E>(h: &t<A, E>) -> i64 {
        h.byte_depth
    }

    /// The entries (newest undo first, then redo), and the one live value kept beside them.
    pub fn retained<A, E>(h: &t<A, E>) -> (Vec<&E>, Option<&A>) {
        let entries = h
            .past
            .iter()
            .rev()
            .map(|(entry, _, _)| entry.as_ref())
            .chain(h.future.iter().rev().map(|(entry, _, _)| entry.as_ref()))
            .collect();
        (entries, h.recent.as_ref().map(|(_, live)| live.as_ref()))
    }

    fn entry_of<A, E>(h: &t<A, E>) -> (Arc<E>, String, i64) {
        let entry = Arc::new((h.keep)(&h.present));
        let n = (h.size)(&entry).max(0);
        (entry, h.label.clone(), n)
    }

    /// Oldest first, down to [limit] entries and the byte cap. The newest entry stays whatever it
    /// weighs: one edit of a document over the cap can still be undone.
    fn trim_to<A, E>(limit: i64, mut h: t<A, E>) -> t<A, E> {
        let excess = h.depth - limit;
        if excess <= 0 && (h.byte_depth <= h.byte_capacity || h.depth <= 1) {
            return h;
        }
        let mut dropped = 0;
        let mut bytes = h.byte_depth;
        let mut depth = h.depth;
        loop {
            let n = match h.past.front() {
                Some((_, _, n)) => *n,
                None => break,
            };
            if dropped < excess || (bytes > h.byte_capacity && depth > 1) {
                h.past.pop_front();
                dropped += 1;
                bytes -= n;
                depth -= 1;
            } else {
                break;
            }
        }
        h.byte_depth = bytes;
        h.depth = depth;
        h
    }

    fn trim<A, E>(h: t<A, E>) -> t<A, E> {
        let limit = h.capacity;
        trim_to(limit, h)
    }

    fn commit<A, E>(value: Arc<A>, label: &str, mut h: t<A, E>) -> t<A, E> {
        if Arc::ptr_eq(&value, &h.present) {
            return h;
        }
        let (entry, old_label, n) = entry_of(&h);
        let abandoned: i64 = h.future.iter().map(|(_, _, n)| *n).sum();
        h.past.push_back((entry.clone(), old_label, n));
        let old_present = std::mem::replace(&mut h.present, value);
        h.label = label.to_string();
        h.future.clear();
        h.recent = Some((entry, old_present));
        h.depth += 1;
        h.byte_depth = h.byte_depth + n - abandoned;
        h.merge = None;
        trim(h)
    }

    /// The present replaced in place: a continued gesture, or a repair that is no edit of the
    /// user's (camera bookkeeping), which leaves what redo would restore.
    fn amend<A, E>(value: Arc<A>, mut h: t<A, E>) -> t<A, E> {
        if Arc::ptr_eq(&value, &h.present) {
            return h;
        }
        h.present = value;
        h
    }

    /// `Step` adds an undo entry; matching `Gesture` keys merge until [`seal`]; matching `Burst`
    /// keys merge while edits stay within the window; `Repair` changes the current entry without
    /// adding a step. `label` names the entry (OCaml's default is "Edit").
    pub fn record<A, E>(merge: Merge, label: &str, value: Arc<A>, h: t<A, E>) -> t<A, E> {
        let continuation = match (&merge, &h.merge) {
            (Merge::Repair, _) => true,
            (Merge::Gesture(id), Some(Merge::Gesture(previous))) => id == previous,
            (
                Merge::Burst { key, at, window },
                Some(Merge::Burst { key: previous_key, at: previous_at, .. }),
            ) => key == previous_key && *at >= *previous_at && *at - *previous_at < *window,
            _ => false,
        };
        let repair = matches!(merge, Merge::Repair);
        let mut h = if continuation { amend(value, h) } else { commit(value, label, h) };
        if !repair {
            h.merge = Some(merge);
        }
        h
    }

    pub fn seal<A, E>(gesture_only: bool, mut h: t<A, E>) -> t<A, E> {
        match &h.merge {
            None => h,
            Some(Merge::Gesture(_)) => {
                h.merge = None;
                h
            }
            Some(_) if !gesture_only => {
                h.merge = None;
                h
            }
            Some(_) => h,
        }
    }

    fn revive<A, E, X>(
        restore: &dyn Fn(&E) -> Result<A, X>,
        h: &t<A, E>,
        entry: &Arc<E>,
    ) -> Result<Arc<A>, X> {
        match &h.recent {
            Some((kept, live)) if Arc::ptr_eq(kept, entry) => Ok(live.clone()),
            _ => restore(entry).map(Arc::new),
        }
    }

    /// `None` with nothing to undo. `Some(Err)`: nothing changed; `h` stays as it was.
    pub fn undo<A, E, X>(
        restore: &dyn Fn(&E) -> Result<A, X>,
        h: &t<A, E>,
    ) -> Option<Result<t<A, E>, X>> {
        let (previous, _, _) = h.past.back()?;
        let live = match revive(restore, h, previous) {
            Ok(live) => live,
            Err(error) => return Some(Err(error)),
        };
        let mut next = h.clone();
        let (_, label, n) = next.past.pop_back().expect("checked non-empty above");
        let (entry, current_label, m) = entry_of(&next);
        let old_present = std::mem::replace(&mut next.present, live);
        next.future.push((entry.clone(), current_label, m));
        next.label = label;
        next.recent = Some((entry, old_present));
        next.depth -= 1;
        next.byte_depth = next.byte_depth + m - n;
        next.merge = None;
        Some(Ok(next))
    }

    /// `None` with nothing to redo. `Some(Err)`: nothing changed; `h` stays as it was.
    pub fn redo<A, E, X>(
        restore: &dyn Fn(&E) -> Result<A, X>,
        h: &t<A, E>,
    ) -> Option<Result<t<A, E>, X>> {
        let (next_entry, _, _) = h.future.last()?;
        let live = match revive(restore, h, next_entry) {
            Ok(live) => live,
            Err(error) => return Some(Err(error)),
        };
        let mut next = h.clone();
        let (_, label, n) = next.future.pop().expect("checked non-empty above");
        let (entry, current_label, m) = entry_of(&next);
        let old_present = std::mem::replace(&mut next.present, live);
        next.past.push_back((entry.clone(), current_label, m));
        next.label = label;
        next.recent = Some((entry, old_present));
        next.depth += 1;
        next.byte_depth = next.byte_depth + m - n;
        next.merge = None;
        Some(Ok(trim(next)))
    }

    /// The oldest undo entries gone, down to [keep] (at least one).
    pub fn shed<A, E>(keep: i64, h: t<A, E>) -> t<A, E> {
        trim_to(keep.max(1), h)
    }

    /// No entries left, and none charged: the editor is closing.
    pub fn release<A, E>(mut h: t<A, E>) -> t<A, E> {
        h.past.clear();
        h.future.clear();
        h.recent = None;
        h.depth = 0;
        h.byte_depth = 0;
        h.merge = None;
        h
    }

    /// A new document: [`release`], with this present.
    pub fn restart<A, E>(value: Arc<A>, h: t<A, E>) -> t<A, E> {
        let mut h = release(h);
        h.present = value;
        h.label = String::new();
        h
    }
}

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
    pub enum t {
        Canvas,
        Node,
        Multi,
        Hints,
        Leader,
        Search,
        List,
        Text,
    }

    pub fn name(context: t) -> &'static str {
        match context {
            t::Canvas => "Canvas",
            t::Node => "Node",
            t::Multi => "Selection",
            t::Hints => "Hints",
            t::Leader => "Leader",
            t::Search => "Search",
            t::List => "List",
            t::Text => "Text",
        }
    }
}

/// Named editor commands: the one table behind key routing, which-key, and the command palette.
pub mod command {
    use crate::editor_core::guide_context;
    use crate::editor_core::keymap::Trigger;

    /// `S` is the scope (pane) type and `A` the action type; both are the host's.
    #[derive(Clone, Debug, PartialEq)]
    pub struct t<S, A> {
        /// Stable, e.g. "edit.undo"; entries sharing an id are one command.
        pub id: String,
        /// Shown in which-key and the palette.
        pub label: String,
        /// `None`: palette only.
        pub trigger: Option<Trigger>,
        /// `None`: global; else only while that pane has focus.
        pub scope: Option<S>,
        /// Contexts where the guide strip lists this command.
        pub guide: Vec<guide_context::t>,
        pub action: A,
    }

    /// The OCaml optional arguments `?trigger ?scope ?guide` are positional here.
    pub fn make<S, A>(
        trigger: Option<Trigger>,
        scope: Option<S>,
        guide: Vec<guide_context::t>,
        id: &str,
        label: &str,
        action: A,
    ) -> t<S, A> {
        t { id: id.into(), label: label.into(), trigger, scope, guide, action }
    }

    /// Applicable commands in their original table order.
    pub fn for_guide<'k, S: PartialEq, A>(
        commands: &'k [t<S, A>],
        focus: &S,
        context: guide_context::t,
    ) -> Vec<&'k t<S, A>> {
        commands
            .iter()
            .filter(|command| {
                command.trigger.is_some()
                    && (command.scope.is_none() || command.scope.as_ref() == Some(focus))
                    && command.guide.contains(&context)
            })
            .collect()
    }
}


pub mod router {
    use super::command::t as Command;
    use super::keymap::{resolve, shifted, Trigger};
    use input::{Event, Key, Platform};
    use rays::frame;

    /// Tab cancels leader routing, runs its exact-modifier host binding or passes to UI traversal,
    /// then hands the remaining ordered events to the UI (including a same-frame /).
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum State {
        Idle,
        /// The leader keys typed so far.
        Pending(String),
    }

    /// The commands a pane with this focus may run, in table order.
    fn visible<'k, S: PartialEq, A>(commands: &[&'k Command<S, A>], focus: &S) -> Vec<&'k Command<S, A>> {
        commands
            .iter()
            .copied()
            .filter(|command| command.scope.is_none() || command.scope.as_ref() == Some(focus))
            .collect()
    }

    fn same_key(a: Key, b: Key) -> bool {
        match (a, b) {
            (Key::KeyChar(a), Key::KeyChar(b)) => a.to_ascii_lowercase() == b.to_ascii_lowercase(),
            _ => a == b,
        }
    }

    /// The matching chord with the most modifiers; on a tie the earlier command wins.
    fn chord<'k, S: PartialEq, A>(
        platform: Platform,
        commands: &[&'k Command<S, A>],
        focus: &S,
        keys: &[Key],
        key: Key,
    ) -> Option<&'k Command<S, A>> {
        let mut best: Option<(usize, &'k Command<S, A>)> = None;
        for command in visible(commands, focus) {
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
        best.map(|(_, command)| command)
    }

    /// In fly mode, keep pointer/window events and pass / to the leader router after ending the
    /// mode. Escape ends fly without opening a shortcut.
    pub fn fly(mut frame: frame::t) -> (bool, frame::t) {
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
    fn text_chords<'k, S: PartialEq, A>(
        platform: Platform,
        keymap: &[&'k Command<S, A>],
        focus: &S,
        previous_keys: &[Key],
        mut frame: frame::t,
    ) -> (Vec<&'k Command<S, A>>, frame::t) {
        let global: Vec<&'k Command<S, A>> =
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
    pub fn step<'k, S: PartialEq, A>(
        previous_keys: &[Key],
        platform: Platform,
        keymap: &[&'k Command<S, A>],
        focus: &S,
        text_focus: bool,
        mut frame: frame::t,
        state: State,
    ) -> (State, Vec<&'k Command<S, A>>, frame::t) {
        if text_focus {
            let (actions, frame) = text_chords(platform, keymap, focus, previous_keys, frame);
            return (State::Idle, actions, frame);
        }
        let mut modifiers = input::keys_before(previous_keys, &frame.input.keys, &frame.input.events);
        let mut traversing = false;
        let mut state = state;
        let mut actions: Vec<&'k Command<S, A>> = Vec::new();
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
                    let commands = visible(keymap, focus);
                    let exact = Trigger::Leader(typed.clone());
                    if let Some(command) = commands.iter().find(|command| command.trigger.as_ref() == Some(&exact)) {
                        actions.push(command);
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
