//! Text widgets: single-line fields, numeric and value fields, the inspector head, the multiline text
//! area with its language hooks, the picker and the context menu. Every text widget edits as the
//! host's conventions say (Apple: Command and Option; elsewhere Ctrl): a click places the caret and
//! Shift-click or a drag extends the selection, a double click selects the word (a triple the line),
//! the word keys move and delete by words, Command-arrows (Apple) and Home/End move by lines,
//! Command-A/C/X/V select all, copy, cut and paste, and Command-Z, Shift-Command-Z (or Command-Y)
//! undo and redo the focused text.
use crate::theme::{self, Theme};
use crate::ui_core::*;
use crate::ui_kit::*;
use input::{Event, Key, MouseButton};
use rays_math::color::{self, T as Color};
use std::rc::Rc;

pub fn previous_utf8(text: &str, index: usize) -> usize {
    if index == 0 {
        return 0;
    }
    let bytes = text.as_bytes();
    let mut i = index - 1;
    loop {
        if i == 0 {
            return 0;
        }
        if bytes[i] & 0xc0 != 0x80 {
            return i;
        }
        i -= 1;
    }
}

pub fn next_utf8(text: &str, index: usize) -> usize {
    let length = text.len();
    if index >= length {
        return length;
    }
    let bytes = text.as_bytes();
    let mut i = index + 1;
    loop {
        if i >= length {
            return length;
        }
        if bytes[i] & 0xc0 != 0x80 {
            return i;
        }
        i += 1;
    }
}

fn numeric_character(c: char) -> bool {
    matches!(c, '0'..='9' | '+' | '-' | '.' | 'e' | 'E')
}

fn clipboard_command(command: bool, event: &Event) -> Option<char> {
    match event {
        Event::Key_pressed(Key::KeyChar(key)) if command => Some(key.to_ascii_lowercase()),
        _ => None,
    }
}

/// The text being edited with its caret and selection anchor (byte offsets).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextEdit {
    pub text: String,
    pub caret: usize,
    pub anchor: usize,
}

/// Snapshots hold the whole text, and five stacks of 200 at 100 KB is 100 MB: so one cap covers the
/// open stacks and the shelf together. The shelf's oldest stack goes first, then the open text's
/// oldest step; the newest step stays, so a text over the cap still undoes once.
pub const TEXT_UNDO_BYTE_CAP: usize = 8 * 1024 * 1024;

fn stack_bytes(stack: &[(String, usize, usize)]) -> usize {
    stack.iter().map(|(text, _, _)| text.len()).sum()
}

pub fn text_selection(edit: &TextEdit) -> (usize, usize) {
    (edit.caret.min(edit.anchor), edit.caret.max(edit.anchor))
}

pub fn replace_text(edit: &mut TextEdit, inserted: &str) {
    let (start, stop) = text_selection(edit);
    edit.text = format!("{}{}{}", &edit.text[..start], inserted, &edit.text[stop..]);
    edit.caret = start + inserted.len();
    edit.anchor = edit.caret;
}

/// Words as macOS counts them: letters, digits, underscores and every non-ASCII byte.
fn word_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c >= 128
}

/// Option-Left: back over the separators, then over the word
pub fn word_left(text: &str, i: usize) -> usize {
    let b = text.as_bytes();
    let mut i = i;
    while i > 0 && !word_char(b[i - 1]) {
        i -= 1;
    }
    while i > 0 && word_char(b[i - 1]) {
        i -= 1;
    }
    i
}

/// Option-Right: over the separators, then to the end of the word
pub fn word_right(text: &str, i: usize) -> usize {
    let b = text.as_bytes();
    let n = b.len();
    let mut i = i;
    while i < n && !word_char(b[i]) {
        i += 1;
    }
    while i < n && word_char(b[i]) {
        i += 1;
    }
    i
}

pub fn line_start(text: &str, i: usize) -> usize {
    let b = text.as_bytes();
    let mut i = i;
    while i > 0 && b[i - 1] != b'\n' {
        i -= 1;
    }
    i
}

pub fn line_end(text: &str, i: usize) -> usize {
    let b = text.as_bytes();
    let n = b.len();
    let mut i = i;
    while i < n && b[i] != b'\n' {
        i += 1;
    }
    i
}

// ---- line and form edits of the text area ----

/// the lines a selection touches, as the byte range `[first, stop)` without the last break
fn line_block(text: &str, a: usize, b: usize) -> (usize, usize) {
    let end = if b > a && text.as_bytes()[b - 1] == b'\n' { b - 1 } else { b };
    (line_start(text, a), line_end(text, end))
}

/// the bytes `[first, stop)` of the edit's text replaced by `replacement`
fn replace_block(edit: &mut TextEdit, first: usize, stop: usize, replacement: &str) {
    edit.text = format!("{}{}{}", &edit.text[..first], replacement, &edit.text[stop..]);
}

/// a selection over the replacement afterwards keeps its direction
fn keep_selection(edit: &mut TextEdit, first: usize, last: usize) {
    if edit.caret >= edit.anchor {
        edit.anchor = first;
        edit.caret = last;
    } else {
        edit.anchor = last;
        edit.caret = first;
    }
}

/// `block` with every line commented by `prefix`, or with it taken off when each non-blank line
/// has it (and the space after it); a comment goes in at the block's smallest indent
fn toggle_comment(prefix: &str, block: &str) -> String {
    let lines: Vec<&str> = block.split('\n').collect();
    let blank = |l: &str| l.trim().is_empty();
    let indent = |l: &str| l.bytes().take_while(|c| *c == b' ' || *c == b'\t').count();
    let body: Vec<&str> = lines.iter().copied().filter(|l| !blank(l)).collect();
    let n = prefix.len();
    let commented = |l: &str| l[indent(l)..].starts_with(prefix);
    let all_commented = !body.is_empty() && body.iter().all(|l| commented(l));
    let col = body.iter().map(|l| indent(l)).min().unwrap_or(usize::MAX);
    lines
        .iter()
        .map(|l| {
            if blank(l) {
                l.to_string()
            } else {
                let i = indent(l);
                if all_commented {
                    let n = if i + n < l.len() && l.as_bytes()[i + n] == b' ' { n + 1 } else { n };
                    format!("{}{}", &l[..i], &l[i + n..])
                } else {
                    format!("{}{} {}", &l[..col], prefix, &l[col..])
                }
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// the innermost bracket pair that holds `[a, b)` and is larger than it, as (open, past the close)
fn enclosing_pair(pairs: &[(usize, usize)], a: usize, b: usize) -> Option<(usize, usize)> {
    let mut best: Option<(usize, usize)> = None;
    for (o, c) in pairs {
        let (o, c) = (*o, *c);
        if o <= a && b <= c + 1 && (o, c + 1) != (a, b) {
            match best {
                Some((bo, bc)) if bc - bo <= c + 1 - o => {}
                _ => best = Some((o, c + 1)),
            }
        }
    }
    best
}

/// the caret across the bracket beside it, else out to the opener of the pair around it
fn bracket_jump(pairs: &[(usize, usize)], caret: usize) -> Option<usize> {
    for (o, c) in pairs {
        if *o == caret {
            return Some(c + 1);
        } else if c + 1 == caret {
            return Some(*o);
        }
    }
    enclosing_pair(pairs, caret, caret).map(|(o, _)| o)
}

/// what a double click selects at `i`: the word there, else the run of blanks, else the one
/// punctuation character; nothing on a line break
pub fn word_at(text: &str, i: usize) -> (usize, usize) {
    let b = text.as_bytes();
    let n = b.len();
    let class_of = |c: u8| -> i32 {
        if word_char(c) {
            1
        } else if c == b' ' || c == b'\t' {
            2
        } else if c == b'\n' || c == b'\r' {
            0
        } else {
            3
        }
    };
    let pick = if i < n && class_of(b[i]) != 0 {
        Some(i)
    } else if i > 0 && class_of(b[i - 1]) != 0 {
        Some(i - 1)
    } else {
        None
    };
    match pick {
        None => (i, i),
        Some(p) if class_of(b[p]) == 3 => (p, p + 1),
        Some(p) => {
            let k = class_of(b[p]);
            let mut a = p;
            let mut e = p + 1;
            while a > 0 && class_of(b[a - 1]) == k {
                a -= 1;
            }
            while e < n && class_of(b[e]) == k {
                e += 1;
            }
            (a, e)
        }
    }
}

/// A number literal dragged `dx` points: a float moves a tenth of its last decimal place per
/// point (Shift: ten times that) and keeps one more decimal; an integer moves one per six points
/// (`param::number::int_scrub_delta`, same as the inspector).
pub fn scrubbed(literal: &str, dx: f64, coarse: bool) -> String {
    let int_value = literal.parse::<i64>().ok();
    match (literal.find('.'), int_value) {
        (None, Some(v)) => {
            let n = param::number::int_scrub_delta(dx);
            if n == 0.0 {
                literal.to_owned()
            } else {
                (((v as f64) + n).round() as i64).to_string()
            }
        }
        (Some(dot), _) => {
            let decimals = literal.len() - dot - 1;
            let step = 10.0f64.powf(-((decimals + 1) as f64)) * if coarse { 10.0 } else { 1.0 };
            let v = cfmt::parse_ocaml_float(literal).unwrap_or(0.0) + dx * step;
            let s = cfmt::f(v, decimals + 1);
            let mut n = s.len();
            while n > 0 && s.as_bytes()[n - 1] == b'0' {
                n -= 1;
            }
            if n > 0 && s.as_bytes()[n - 1] == b'.' { s[..n + 1].to_owned() } else { s[..n].to_owned() }
        }
        (None, None) => literal.to_owned(),
    }
}

// ---- the retained edit state ----

impl Ui {
    pub(crate) fn text_caret_at(&mut self, size: Option<i64>, text: &str, x: f64) -> usize {
        let density = self.fonts_mut().density as f64;
        let Some(font) = self.fonts_mut().face(size) else { return text.len() };
        let length = text.len();
        let id = if length == 0 { 0 } else { self.fonts_mut().atlas_font_id(&font) };
        let mut width = 0.0;
        for (index, c) in text.char_indices() {
            let advance = match self.fonts_mut().glyph(&font, id, c as i64) {
                Some(g) => g.advance as f64 / density,
                None => 0.0,
            };
            if x < width + advance / 2.0 {
                return index;
            }
            width += advance;
        }
        length
    }

    pub fn text_undo_bytes(&self) -> usize {
        stack_bytes(&self.edit_undo)
            + stack_bytes(&self.edit_redo)
            + self.edit_shelf.iter().map(|(_, _, undo, redo)| stack_bytes(undo) + stack_bytes(redo)).sum::<usize>()
    }

    /// the ledger follows the stacks' one running total
    fn charge_text_undo(&mut self, bytes: i64) {
        mem::charge(text_undo_ledger(), bytes - self.edit_undo_charged);
        mem::release(text_undo_ledger(), self.edit_undo_charged - bytes);
        self.edit_undo_charged = bytes;
    }

    fn enforce_text_undo(&mut self) {
        loop {
            let bytes = self.text_undo_bytes();
            let dropped = bytes > TEXT_UNDO_BYTE_CAP
                && if !self.edit_shelf.is_empty() {
                    self.edit_shelf.pop();
                    true
                } else if self.edit_undo.len() >= 2 {
                    self.edit_undo.pop();
                    true
                } else {
                    false
                };
            if !dropped {
                self.charge_text_undo(bytes as i64);
                return;
            }
        }
    }

    /// A text starts being edited: the caret at its end, the selection from `anchor`, and nothing of
    /// the text edited before it (scroll, undo stacks, a double click's selection unit).
    pub(crate) fn reset_edit(&mut self, key: i64, text: &str, anchor: usize) {
        self.edit_focus = key;
        self.edit_value = text.to_owned();
        self.edit_caret = text.len();
        self.edit_anchor = anchor;
        self.edit_scroll_x = 0.0;
        self.edit_undo.clear();
        self.edit_redo.clear();
        self.edit_group = -1;
        self.edit_unit = None;
        self.edit_column = -1.0;
        self.enforce_text_undo();
    }

    pub fn load_text_edit(&mut self, key: i64, text: &str) -> TextEdit {
        if self.edit_focus != key || self.edit_value != text {
            let left: ShelfEntry = (self.edit_focus, self.edit_value.clone(), self.edit_undo.clone(), self.edit_redo.clone());
            let back = self.edit_shelf.iter().find(|(k, v, _, _)| *k == key && v == text).cloned();
            let mut shelf: Vec<ShelfEntry> = self.edit_shelf.iter().filter(|e| Some(*e) != back.as_ref()).cloned().collect();
            if !(self.edit_undo.is_empty() && self.edit_redo.is_empty()) {
                shelf.insert(0, left);
            }
            shelf.truncate(4);
            self.edit_shelf = shelf;
            self.reset_edit(key, text, text.len());
            if let Some((_, _, undo, redo)) = back {
                self.edit_undo = undo;
                self.edit_redo = redo;
            }
            self.enforce_text_undo();
        }
        TextEdit { text: text.to_owned(), caret: self.edit_caret, anchor: self.edit_anchor }
    }

    pub(crate) fn save_text_edit(&mut self, edit: &TextEdit) {
        self.edit_value = edit.text.clone();
        self.edit_caret = edit.caret;
        self.edit_anchor = edit.anchor;
    }

    // ---- undo inside the focused text: snapshots before each change, consecutive typing as one

    pub fn remember(&mut self, edit: &TextEdit, typing: bool) {
        if !(typing && edit.caret == edit.anchor && self.edit_group == edit.caret as i64) {
            self.edit_undo.insert(0, (edit.text.clone(), edit.caret, edit.anchor));
            self.edit_undo.truncate(200);
            self.edit_redo.clear();
            self.enforce_text_undo();
        }
        self.edit_group = -1;
    }

    fn undo_text(&mut self, edit: &mut TextEdit, redo: bool) -> bool {
        let stack = if redo { &mut self.edit_redo } else { &mut self.edit_undo };
        if stack.is_empty() {
            return false;
        }
        let (text, caret, anchor) = stack.remove(0);
        let now = (edit.text.clone(), edit.caret, edit.anchor);
        if redo {
            self.edit_undo.insert(0, now);
        } else {
            self.edit_redo.insert(0, now);
        }
        edit.text = text;
        edit.caret = caret;
        edit.anchor = anchor;
        self.edit_group = -1;
        self.enforce_text_undo();
        true
    }

    /// an exhausted stack hands the key to the host's own undo (`passed_undo`)
    fn pass_undo(&mut self, edit: &mut TextEdit, redo: bool) -> bool {
        if self.undo_text(edit, redo) {
            return true;
        }
        self.set_passed_undo(if redo { UndoKind::Redo } else { UndoKind::Undo });
        false
    }

    /// A press places the caret (Shift extends the selection), a double click selects the word
    /// there, a triple click the `line_of` range; the drag that follows extends by the same unit.
    pub(crate) fn press_select(&mut self, edit: &mut TextEdit, shift: bool, clicks: i64, line_of: &dyn Fn(&TextEdit, usize) -> (usize, usize), at: usize) {
        if clicks < 2 {
            edit.caret = at;
            if !shift {
                edit.anchor = at;
            }
            self.edit_unit = None;
        } else {
            let (a, b) = if clicks == 2 { word_at(&edit.text, at) } else { line_of(edit, at) };
            edit.anchor = a;
            edit.caret = b;
            self.edit_unit = Some((a, b, clicks));
        }
    }

    pub(crate) fn drag_select(&mut self, edit: &mut TextEdit, line_of: &dyn Fn(&TextEdit, usize) -> (usize, usize), at: usize) {
        match self.edit_unit {
            None => edit.caret = at,
            Some((a, b, clicks)) => {
                let (c, d) = if clicks == 2 { word_at(&edit.text, at) } else { line_of(edit, at) };
                if c < a {
                    edit.anchor = b;
                    edit.caret = c;
                } else {
                    edit.anchor = a;
                    edit.caret = b.max(d);
                }
            }
        }
    }

    pub(crate) fn point_text_caret(&mut self, size: Option<i64>, edit: &mut TextEdit, signal: &Signal, shift: bool, x: f64, right: f64) {
        let scroll = self.edit_scroll_x;
        let at = |ui: &mut Ui, px: f64, text: &str| ui.text_caret_at(size, text, 0.0f64.max(right.min(px) - x + scroll));
        let line_of = |e: &TextEdit, _: usize| (0, e.text.len());
        if signal.pressed {
            let a = at(self, signal.press_point.0, &edit.text);
            self.press_select(edit, shift, signal.clicks, &line_of, a);
        }
        if signal.dragging || (signal.held && signal.pointer != signal.press_point) {
            let px = signal.pointer.0;
            if px < x {
                self.edit_scroll_x = 0.0f64.max(self.edit_scroll_x - 1.0f64.max(24.0f64.min((x - px) * 0.25)));
            } else if px > right {
                self.edit_scroll_x += 1.0f64.max(24.0f64.min((px - right) * 0.25));
            }
            let scroll = self.edit_scroll_x;
            let a = self.text_caret_at(size, &edit.text, 0.0f64.max(right.min(px) - x + scroll));
            self.drag_select(edit, &line_of, a);
        }
    }

    /// The keys every text widget shares, as the host binds them. On Apple: Command-A/C/X/V,
    /// Command-Z and Shift-Command-Z (or Command-Y) undo and redo, Option-arrows move by words,
    /// Command-arrows and Home/End by lines, Option-Backspace/Delete take a word,
    /// Command-Backspace/Delete the line to the caret. Elsewhere Ctrl replaces Command and Option
    /// (Ctrl-arrows and Ctrl-Backspace move and delete by words; Home and End are the lines).
    /// Returns whether the text changed.
    pub(crate) fn edit_text_event(&mut self, multiline: bool, edit: &mut TextEdit, accept: &dyn Fn(&str) -> bool, modifiers: &[Key], event: &Event) -> bool {
        let command = self.command_modifiers(modifiers);
        let shift = modifiers.contains(&Key::Shift);
        // Command moves and deletes by lines on Apple only; elsewhere Ctrl moves by words
        let line = self.line_modifiers(modifiers);
        let word = input::word_held(self.platform(), modifiers);
        let selected = |e: &TextEdit| e.caret != e.anchor;
        let clip = clipboard_command(command, event);
        match clip {
            Some('a') => {
                edit.anchor = 0;
                edit.caret = edit.text.len();
                return false;
            }
            Some('c') => {
                let (start, stop) = text_selection(edit);
                let _ = rays::clipboard::set_text(if selected(edit) { &edit.text[start..stop] } else { &edit.text });
                return false;
            }
            Some('x') => {
                let (start, stop) = text_selection(edit);
                let copied = if selected(edit) { edit.text[start..stop].to_owned() } else { edit.text.clone() };
                if rays::clipboard::set_text(&copied).is_ok() {
                    self.remember(edit, false);
                    if selected(edit) {
                        replace_text(edit, "");
                    } else {
                        edit.text.clear();
                        edit.caret = 0;
                        edit.anchor = 0;
                    }
                    return true;
                }
                return false;
            }
            Some('v') => {
                // a field of one line takes a copied line without its break
                return match rays::clipboard::get_text() {
                    Ok(text) => {
                        let text: String = if multiline { text } else { text.chars().filter(|c| *c != '\n' && *c != '\r').collect() };
                        if accept(&text) {
                            self.remember(edit, false);
                            replace_text(edit, &text);
                            true
                        } else {
                            false
                        }
                    }
                    Err(_) => false,
                };
            }
            Some('z') => return self.pass_undo(edit, shift),
            Some('y') => return self.pass_undo(edit, true),
            _ => {}
        }
        match event {
            Event::Text_input(text) if accept(text) => {
                let typing = !text.chars().any(|c| c == ' ' || c == '\t' || c == '\n');
                self.remember(edit, typing);
                replace_text(edit, text);
                if typing {
                    self.edit_group = edit.caret as i64;
                }
                true
            }
            Event::Key_pressed(Key::Backspace) => {
                let target = if line {
                    line_start(&edit.text, edit.caret)
                } else if word {
                    word_left(&edit.text, edit.caret)
                } else {
                    previous_utf8(&edit.text, edit.caret)
                };
                self.delete_to(edit, target)
            }
            Event::Key_pressed(Key::Delete) => {
                let target = if line {
                    line_end(&edit.text, edit.caret)
                } else if word {
                    word_right(&edit.text, edit.caret)
                } else {
                    next_utf8(&edit.text, edit.caret)
                };
                self.delete_to(edit, target)
            }
            Event::Key_pressed(Key::ArrowLeft) => {
                let target = if line {
                    line_start(&edit.text, edit.caret)
                } else if word {
                    word_left(&edit.text, edit.caret)
                } else if selected(edit) && !shift {
                    text_selection(edit).0
                } else {
                    previous_utf8(&edit.text, edit.caret)
                };
                move_caret(edit, target, shift);
                false
            }
            Event::Key_pressed(Key::ArrowRight) => {
                let target = if line {
                    line_end(&edit.text, edit.caret)
                } else if word {
                    word_right(&edit.text, edit.caret)
                } else if selected(edit) && !shift {
                    text_selection(edit).1
                } else {
                    next_utf8(&edit.text, edit.caret)
                };
                move_caret(edit, target, shift);
                false
            }
            Event::Key_pressed(Key::Home) => {
                let t = line_start(&edit.text, edit.caret);
                move_caret(edit, t, shift);
                false
            }
            Event::Key_pressed(Key::End) => {
                let t = line_end(&edit.text, edit.caret);
                move_caret(edit, t, shift);
                false
            }
            _ => false,
        }
    }

    /// the selection, else from the caret to `target`
    fn delete_to(&mut self, edit: &mut TextEdit, target: usize) -> bool {
        let selected = edit.caret != edit.anchor;
        if !selected && target == edit.caret {
            false
        } else {
            self.remember(edit, false);
            if !selected {
                edit.anchor = target;
            }
            replace_text(edit, "");
            true
        }
    }
}

pub(crate) fn move_caret(edit: &mut TextEdit, target: usize, shift: bool) {
    edit.caret = target;
    if !shift {
        edit.anchor = target;
    }
}

#[allow(clippy::too_many_arguments)]
pub fn paint_text_edit(
    paint: &mut Paint,
    size: Option<i64>,
    right: bool,
    inset: f64,
    caret: bool,
    control: IRect,
    y: i64,
    composition: &str,
    edit: &TextEdit,
) {
    let (cx, cy, cw, ch) = control;
    let theme = paint.theme;
    let before = edit.text[..edit.caret].to_owned();
    let visible = 1.max(cw - 4) as f64;
    let full = paint.text_width(size, &edit.text);
    let scroll = 0.0f64.max(paint.edit_scroll_x.min(0.0f64.max(full - visible)));
    let caret_width = paint.text_width(size, &before);
    let scroll = if caret_width < scroll {
        caret_width
    } else if caret_width - scroll > visible {
        caret_width - visible
    } else {
        scroll
    };
    paint.edit_scroll_x = scroll;
    // a number keeps its right edge while it is edited, until it outgrows the field
    let origin = if right && full <= visible { (cx + cw - 2) as f64 - full } else { cx as f64 + inset };
    let text_x = origin - scroll;
    let caret_x = text_x + caret_width;
    paint.input_region(caret_x - cx as f64, cx as f64, cy as f64, cw as f64, ch as f64, true);
    let previous_clip = paint.clip_rect;
    paint.clip_rect = intersect(previous_clip, (cx as f64, cy as f64, cw as f64, ch as f64));
    if edit.caret != edit.anchor {
        let (start, stop) = text_selection(edit);
        let x0 = paint.text_width(size, &edit.text[..start]);
        let w = paint.text_width(size, &edit.text[start..stop]);
        paint.fill(text_x + x0, cy as f64 + 0.5, w, 1.max(ch - 2) as f64, theme::tint(&theme));
    }
    if composition.is_empty() {
        paint.text_with((text_x, y as f64), size, 0.0, None, &edit.text);
    } else {
        paint.text_with((text_x, y as f64), size, 0.0, None, &before);
        paint.text_with((caret_x, y as f64), size, 0.0, None, composition);
        let cwid = paint.text_width(size, composition);
        paint.text_with((caret_x + cwid, y as f64), size, 0.0, None, &edit.text[edit.caret..]);
    }
    // the caret is the line of the text it sits in: 14 of a 20-point field at body size
    let (caret_y, caret_h) = match size {
        None => (cy as f64 + 2.5, 14.0),
        Some(size) => {
            let h = size as f64 * 1.1;
            (cy as f64 + (ch as f64 - h) / 2.0, h)
        }
    };
    if caret {
        paint.fill(caret_x, caret_y, 1.0, caret_h, theme.accent);
    }
    paint.clip_rect = previous_clip;
}

pub struct NumParams {
    pub size: Option<i64>,
    pub control: Option<IRect>,
    pub click_to_edit: bool,
    pub edit_request: bool,
    pub alt_to_edit: bool,
    pub align_right: bool,
}

impl Default for NumParams {
    fn default() -> NumParams {
        NumParams { size: None, control: None, click_to_edit: false, edit_request: false, alt_to_edit: false, align_right: false }
    }
}

impl Ui {
    /// Numeric label editing shared by float and integer sliders. The retained state records
    /// validity; `parse` validates the edit buffer. Returns the committed value and whether an
    /// editor is open.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn numeric_editor(
        &mut self,
        p: &NumParams,
        accept: &dyn Fn(&str) -> bool,
        row: &UiBox,
        signal: &Signal,
        keys: &[(Event, Vec<Key>)],
        current: &dyn Fn() -> String,
        parse: &dyn Fn(&str) -> Option<String>,
    ) -> (Option<String>, bool) {
        let bounds = ints(self.rect(row));
        let control = p.control.unwrap_or_else(|| value_control(bounds));
        let inside_control = |point: (f64, f64)| contains(floats(control), point);
        let editing = self.text_state(row);
        let state = self.state(row, 1);
        match editing {
            Some(text) if !self.focused(row) || (signal.pressed && !inside_control(signal.press_point)) => {
                // Focus moved away: commit a valid value, drop an invalid one.
                self.set_text_state(row, None);
                self.set_state(row, 1);
                if self.focused(row) {
                    self.unfocus();
                }
                (parse(&text), false)
            }
            Some(text) => {
                let mut edit = self.load_text_edit(row.box_key, &text);
                let mut committed: Option<String> = None;
                let mut state = state;
                let mut cancelled = false;
                let (cx, _, cw, _) = control;
                let left = if p.align_right {
                    ((cx + 2) as f64).max((cx + cw - 2) as f64 - self.text_width(p.size, &edit.text))
                } else {
                    (cx + 2) as f64
                };
                let shift = self.press_shift(row);
                self.point_text_caret(p.size, &mut edit, signal, shift, left, (cx + cw - 2) as f64);
                for (event, modifiers) in keys {
                    match event {
                        Event::Key_pressed(Key::Enter) if !self.command_modifiers(modifiers) => match parse(&edit.text) {
                            Some(value) => committed = Some(value),
                            None => state = 0,
                        },
                        Event::Key_pressed(Key::Escape) => cancelled = true,
                        event => {
                            if self.edit_text_event(false, &mut edit, accept, modifiers, event) {
                                state = if parse(&edit.text).is_some() { 1 } else { 0 };
                            }
                        }
                    }
                }
                if cancelled {
                    self.set_text_state(row, None);
                    self.set_state(row, 1);
                    self.unfocus();
                    (None, false)
                } else if let Some(value) = committed {
                    self.set_text_state(row, None);
                    self.set_state(row, 1);
                    self.unfocus();
                    (Some(value), false)
                } else {
                    self.save_text_edit(&edit);
                    self.set_text_state(row, Some(edit.text.clone()));
                    self.set_state(row, if state & 1 != 0 { 1 } else { 0 });
                    (None, true)
                }
            }
            None => {
                let click = p.click_to_edit
                    && signal.clicked
                    && (signal.release_point.0 - signal.press_point.0).abs() < 4.0
                    && (signal.release_point.1 - signal.press_point.1).abs() < 4.0;
                let platform = self.platform();
                let enter = move |(event, modifiers): &(Event, Vec<Key>)| {
                    matches!(event, Event::Key_pressed(Key::Enter)) && !input::command_held(platform, modifiers)
                };
                let enter_pressed = keys.iter().any(|k| enter(k));
                if p.edit_request
                    || (p.alt_to_edit && signal.clicked && self.press_keys(row).contains(&Key::Alt))
                    || click
                    || (signal.double_clicked && !inside_control(signal.press_point))
                    || (self.focused(row) && enter_pressed)
                {
                    let text = current();
                    self.set_text_state(row, Some(text.clone()));
                    self.set_state(row, 1);
                    self.focus_box(row);
                    self.reset_edit(row.box_key, &text, 0);
                    let keys: Vec<(Event, Vec<Key>)> = if signal.double_clicked || signal.clicked {
                        keys.to_vec()
                    } else {
                        // after_enter: what follows the first Enter
                        match keys.iter().position(|k| enter(k)) {
                            Some(at) => keys[at + 1..].to_vec(),
                            None => Vec::new(),
                        }
                    };
                    let mut again = signal.clone();
                    again.keys = keys.iter().map(|(e, _)| e.clone()).collect();
                    again.pressed = false;
                    again.clicked = false;
                    again.double_clicked = false;
                    let np = NumParams { size: p.size, control: Some(control), click_to_edit: p.click_to_edit, edit_request: false, alt_to_edit: false, align_right: p.align_right };
                    self.numeric_editor(&np, accept, row, &again, &keys, current, parse)
                } else {
                    (None, false)
                }
            }
        }
    }
}

/// Options of `value_field` beyond its place and key.
#[derive(Clone)]
pub struct ValueFieldOpts {
    pub size: Option<i64>,
    pub display: Option<String>,
    pub fraction: Option<f64>,
    pub slide: Option<Rc<dyn Fn(f64) -> String>>,
    pub scrub: Option<Rc<dyn Fn(&str, f64, bool) -> String>>,
    pub left: bool,
    pub edit: bool,
    pub lead: Option<(String, Color)>,
    pub trail: Option<(String, Color)>,
    pub line: Option<Color>,
    pub bare: bool,
    pub placeholder: Option<String>,
    pub tracking: Option<f64>,
}

impl Default for ValueFieldOpts {
    fn default() -> ValueFieldOpts {
        ValueFieldOpts {
            size: None,
            display: None,
            fraction: None,
            slide: None,
            scrub: None,
            left: false,
            edit: false,
            lead: None,
            trail: None,
            line: None,
            bare: false,
            placeholder: None,
            tracking: None,
        }
    }
}

impl Ui {
    /// Compact field. Numeric sliders follow the pointer with `slide`; Option-click, a
    /// double-click or `edit` opens text entry. `scrub` handles fields without a track. `left`
    /// aligns text values to the left (default false for numeric fields). `lead` and `trail` frame
    /// the value at the left and right with 6 points between; `line` is the idle hairline's colour;
    /// `bare` leaves the hairline out until the field is edited or invalid; `placeholder` is shown
    /// in ink-3 where an empty value would be. Only valid text commits; the boolean reports an open
    /// text editor.
    #[allow(clippy::too_many_arguments)]
    pub fn value_field(&mut self, at: (f64, f64), w: f64, h: f64, o: &ValueFieldOpts, valid: &dyn Fn(&str) -> bool, label: &str, value: &str) -> (String, bool) {
        let bx = self.box_(
            label,
            BoxOpts::default().flags(CLICKABLE | TAB_STOP | BLOCKING | CLIP).at(at).w(Size::Px(w)).h(Size::Px(h)),
        );
        let signal = self.signal(&bx);
        let bounds = ints(self.rect(&bx));
        let slider = o.slide.is_some() || o.scrub.is_some();
        let keys = self.key_events(&bx);
        let np = NumParams {
            size: o.size,
            control: Some(bounds),
            click_to_edit: !slider,
            edit_request: o.edit || (slider && signal.double_clicked),
            alt_to_edit: slider,
            align_right: false,
        };
        let current_value = value.to_owned();
        let (typed, editing) = self.numeric_editor(
            &np,
            &|_| true,
            &bx,
            &signal,
            &keys,
            &move || current_value.clone(),
            &|text| if valid(text) { Some(text.to_owned()) } else { None },
        );
        if editing {
            self.set_flags_of_box(&bx, CLICKABLE | FOCUSABLE | BLOCKING | CLIP);
        }
        let origin = if signal.pressed {
            self.scrub_origin = Some((bx.box_key, value.to_owned()));
            value.to_owned()
        } else {
            match &self.scrub_origin {
                Some((key, origin)) if *key == bx.box_key => origin.clone(),
                _ => value.to_owned(),
            }
        };
        let press_keys = self.press_keys(&bx);
        let typed_is_some = typed.is_some();
        let value: String = match typed {
            Some(text) => text,
            None if !editing
                && (signal.pressed || signal.held || signal.released)
                && signal.button == Some(MouseButton::LeftButton)
                && !press_keys.contains(&Key::Alt) =>
            {
                let pointer = if signal.released { signal.release_point } else { signal.pointer };
                match (&o.slide, &o.scrub) {
                    (Some(slide), _) => slide(fraction_at(bounds, pointer.0)),
                    (None, Some(scrub)) => scrub(&origin, pointer.0 - signal.press_point.0, press_keys.contains(&Key::Shift)),
                    (None, None) => value.to_owned(),
                }
            }
            None => value.to_owned(),
        };
        if !signal.held && self.scrub_origin.as_ref().is_some_and(|(key, _)| *key == bx.box_key) {
            self.scrub_origin = None;
        }
        let buffer = self.text_state(&bx);
        let focused_now = self.focused(&bx);
        let edit = if editing && focused_now {
            self.load_text_edit(bx.box_key, buffer.as_deref().unwrap_or(&value))
        } else {
            TextEdit { text: value.clone(), caret: 0, anchor: 0 }
        };
        let composition = self.composition().to_owned();
        let shown = if typed_is_some || signal.held || signal.released { value.clone() } else { o.display.clone().unwrap_or_else(|| value.clone()) };
        let invalid = self.state(&bx, 1) & 1 == 0;
        let theme = self.theme();
        let o = o.clone();
        let size = o.size;
        self.draw(&bx, move |paint, rect| {
            let bounds = ints(rect);
            let (x, y, w, h) = bounds;
            let previous_clip = paint.clip_rect;
            paint.clip_rect = intersect(previous_clip, (x as f64, y as f64, w as f64, h as f64));
            if invalid || editing || !o.bare {
                underline(
                    paint,
                    bounds,
                    if invalid {
                        theme::invalid()
                    } else if editing {
                        theme.accent
                    } else {
                        o.line.unwrap_or(theme::edge(&theme))
                    },
                );
            }
            // the content is centred in the 19 points above the hairline: a body line starts half a point up
            let text_y = match size {
                Some(size) => (y + 0.max((h - size) / 2)) as f64,
                None => y as f64 + (h - 1 - paint.font_size) as f64 / 2.0 - 0.5,
            };
            if editing && focused_now {
                let ty = y + 0.max((h - size.unwrap_or(paint.font_size)) / 2);
                paint_text_edit(paint, size, false, 2.0, true, bounds, ty, &composition, &edit);
            } else {
                // the 2-point line under the value is its position in the soft range
                if let Some(f) = o.fraction {
                    paint.fill(x as f64, (y + h - 3) as f64, w as f64 * 0.0f64.max(1.0f64.min(f)), 2.0, theme::ink_2(&theme));
                }
                let tracking = o.tracking.unwrap_or(0.0);
                match (&o.lead, &o.trail) {
                    (None, None) if shown.is_empty() && o.placeholder.is_some() => {
                        paint.text_with(((x + 2) as f64, text_y), size, 0.0, Some(theme::ink_3(&theme)), o.placeholder.as_ref().unwrap());
                    }
                    (None, None) => {
                        let width = paint.text_width(size, &shown);
                        let tx = if o.left { (x + 2) as f64 } else { (x + w - 2) as f64 - width };
                        paint.text_with((tx, text_y), size, tracking, None, &shown);
                    }
                    _ => {
                        // a lead (the expression's f) and a trail (its live value) frame the value: 6 between
                        let mut draw_part = |paint: &mut Paint, right: bool, text: &str, color: Color| -> f64 {
                            let width = paint.text_width(size, text);
                            let tx = if right { (x + w - 2) as f64 - width } else { (x + 2) as f64 };
                            paint.text_with((tx, text_y), size, 0.0, Some(color), text);
                            width
                        };
                        let lead_w = match &o.lead {
                            Some((t, c)) => draw_part(paint, false, t, *c) + 6.0,
                            None => 0.0,
                        };
                        let trail_w = match &o.trail {
                            Some((t, c)) => draw_part(paint, true, t, *c) + 6.0,
                            None => 0.0,
                        };
                        // with no lead the value sits against its trail (a unit: [35 mm])
                        let fit_size = size.unwrap_or(paint.font_size);
                        let fitted = inspector_fit(paint, fit_size, w as f64 - 4.0 - lead_w - trail_w, &shown);
                        let value_x = if o.lead.is_none() && !o.left {
                            (x + w - 2) as f64 - trail_w - paint.text_width(size, &fitted)
                        } else {
                            (x + 2) as f64 + lead_w
                        };
                        paint.text_with((value_x, text_y), size, 0.0, Some(theme.foreground), &fitted);
                    }
                }
            }
            paint.clip_rect = previous_clip;
        });
        (value, editing)
    }

    /// A `value_field` over `param::number`: a drag scrubs the text by the step of `kind` and soft
    /// `range`, so a click writes nothing. The new text; `valid` defaults to the kind's.
    #[allow(clippy::too_many_arguments)]
    pub fn number_field(
        &mut self,
        at: (f64, f64),
        w: f64,
        h: f64,
        o: &ValueFieldOpts,
        kind: param::number::Kind,
        range: Option<(f64, f64)>,
        valid: Option<Rc<dyn Fn(&str) -> bool>>,
        key: &str,
        text: &str,
    ) -> String {
        let mut o = o.clone();
        o.fraction = range.and_then(|r| param::number::fraction(r, text));
        o.scrub = Some(Rc::new(move |origin, dx, fine| param::number::scrub(kind, range, origin, dx, fine)));
        let valid: Rc<dyn Fn(&str) -> bool> = match valid {
            Some(v) => v,
            None => Rc::new(move |t| param::number::valid(kind, t)),
        };
        self.value_field(at, w, h, &o, &*valid, key, text).0
    }

    /// The head block of a panel, from the kit's inspector sheet: 12 above, a chips row (the kind
    /// in ink-2, a badge in the accent, an index at the right in ink-3), the name at the display
    /// size (edited in place when `rename` says which names are valid), one detail line in ink-2, a
    /// row of text buttons with their keys and a trailing `reset` button, 8 below and a line-2
    /// hairline.
    #[allow(clippy::too_many_arguments)]
    pub fn inspector_header(
        &mut self,
        key: &str,
        kind: Option<&str>,
        badge: Option<&str>,
        index: Option<&str>,
        rename: Option<Rc<dyn Fn(&str) -> bool>>,
        actions: &[HeadAction],
        reset: Option<&str>,
        reset_enabled: bool,
        title: &str,
        detail: &str,
    ) -> Head {
        let win = self.kit_window_flag();
        let font_size = self.font_size();
        // a window's head (windows.html): the 20-point title and one detail line, 8 above, no
        // chips, buttons or hairline
        let display_size = font_size * (if win { theme::TITLE_SIZE } else { theme::DISPLAY_SIZE }) / theme::FONT_SIZE;
        let small = 8.max(font_size - 2);
        let line = self.row_height() as f64;
        let title_h = if win { self.row_height() as f64 } else { (display_size + 8) as f64 };
        let width = self.inspector_width();
        let chips = !win && (kind.is_some() || badge.is_some() || index.is_some());
        let actions: Vec<HeadAction> = if win { Vec::new() } else { actions.to_vec() };
        let reset = if win { None } else { reset };
        let buttons = !actions.is_empty() || reset.is_some();
        let text_x = if win { 12.0 } else { 8.0 };
        let y_title = if win { 8.0 } else if chips { 12.0 + line + 4.0 } else { 12.0 };
        let y_detail = y_title + title_h + if win { 0.0 } else { 4.0 };
        let y_buttons = y_detail + line + 8.0;
        let height = if win {
            y_detail + line
        } else {
            (if buttons { y_buttons + 20.0 } else { y_detail + line }) + 8.0 + 1.0
        };
        let header = self.box_(key, BoxOpts::default().flags(CLIP).w(Size::Grow).h(Size::Px(height)));
        let theme = self.theme();
        let renaming = rename.is_some();
        let title_shown = match &rename {
            Some(valid) => {
                let saved = self.enter(&header);
                let o = ValueFieldOpts { size: Some(display_size), left: true, bare: true, tracking: Some(-0.01 * display_size as f64), ..ValueFieldOpts::default() };
                let (t, _) = self.value_field(
                    (text_x - 2.0, if win { y_title - 1.0 } else { y_title - 2.0 }),
                    width - 2.0 * (text_x - 2.0),
                    title_h,
                    &o,
                    &**valid,
                    &format!("{}-name", key),
                    title,
                );
                self.leave(saved);
                t
            }
            None => title.to_owned(),
        };
        // the buttons: a box each, in a row from 8 with 4 between; `reset` is bare at the right
        let mut pressed: Option<i64> = None;
        let mut place = 8.0;
        for (i, a) in actions.iter().enumerate() {
            let w = 14.0 + mono_width(0.0, font_size, &a.caption) + if a.keycap.is_empty() { 0.0 } else { 6.0 + mono_width(0.0, small, &a.keycap) };
            self.head_button(&header, key, i as i64, &a.caption, &a.keycap, a.active, false, a.usable, place, w, 6.0, y_buttons, &mut pressed);
            place += w + 4.0;
        }
        let mut reset_pressed = false;
        if let Some(label) = reset {
            let w = 10.0 + mono_width(0.0, font_size, label);
            let before = pressed;
            self.head_button(&header, key, -1, label, "", false, true, reset_enabled, width - 8.0 - w, w, 4.0, y_buttons, &mut pressed);
            if pressed != before {
                pressed = before;
                reset_pressed = true;
            }
        }
        let kind = kind.map(|s| s.to_owned());
        let badge = badge.map(|s| s.to_owned());
        let index = index.map(|s| s.to_owned());
        let title_own = title.to_owned();
        let detail = detail.to_owned();
        self.draw(&header, move |paint, (x, y, w, h)| {
            if !win {
                paint.fill(x, y + h - 1.0, w, 1.0, theme::edge(&theme));
            }
            if chips {
                let at = y + 12.0 + paint.text_top(Some(small), 0.0, line);
                let mut cx = x + 8.0;
                // the kind gives way when the index and badge need the room
                let per = small as f64 / 2.0 + 0.08 * small as f64;
                let taken = index.as_ref().map(|t| mono_width(0.0, small, t) + 6.0).unwrap_or(0.0)
                    + badge.as_ref().map(|t| mono_width(0.08 * small as f64, small, t) + 6.0).unwrap_or(0.0);
                if let Some(text) = &kind {
                    let room = ((w - 16.0 - taken) / per) as i64;
                    let text: String = if text.len() as i64 <= room {
                        text.clone()
                    } else if room <= 1 {
                        String::new()
                    } else {
                        format!("{}.", &text[..(room - 1) as usize])
                    };
                    paint.cap((cx, at), None, &text);
                    // the glyphs run a little wider than the design's metrics: never closer than 6
                    cx += mono_width(0.08 * small as f64, small, &text).max(paint.cap_width(&text)) + 6.0;
                }
                if let Some(text) = &badge {
                    paint.cap((cx, at), Some(theme.accent), text);
                }
                // the sheet's 0.04em tracking follows every letter, the last too, so it ends 8 from the edge
                if let Some(text) = &index {
                    let tracking = 0.04 * small as f64;
                    let count = text.bytes().filter(|b| b & 0xc0 != 0x80).count();
                    let tw = paint.text_width(Some(small), text);
                    paint.text_with((x + w - 8.0 - tw - tracking * count as f64, at), Some(small), tracking, Some(theme::ink_3(&theme)), text);
                }
            }
            if !renaming {
                let dy = if win { paint.text_top(Some(display_size), 0.0, title_h) } else { 2.0 };
                let fitted = inspector_fit(paint, display_size, w - 2.0 * text_x, &title_own);
                paint.text_with((x + text_x, y + y_title + dy), Some(display_size), -0.01 * display_size as f64, Some(theme.foreground), &fitted);
            }
            let ty = paint.text_top(None, 0.0, line);
            let fs = paint.font_size;
            let fitted = inspector_fit(paint, fs, w - 2.0 * text_x, &detail);
            paint.text_c((x + text_x, y + y_detail + ty), theme::ink_2(&theme), &fitted);
        });
        Head { renamed: title_shown, chosen: pressed, reset_pressed }
    }

    #[allow(clippy::too_many_arguments)]
    fn head_button(
        &mut self,
        header: &UiBox,
        key: &str,
        index: i64,
        label: &str,
        hint: &str,
        on: bool,
        dim: bool,
        enabled: bool,
        x: f64,
        w: f64,
        pad: f64,
        y_buttons: f64,
        pressed: &mut Option<i64>,
    ) {
        let saved = self.enter(header);
        let b = self.box_(
            &format!("{}-button-{}", key, index),
            BoxOpts::default().flags(if enabled { CLICKABLE | TAB_STOP } else { NO_FLAGS }).at((x, y_buttons)).w(Size::Px(w)).h(Size::Px(20.0)),
        );
        self.leave(saved);
        let signal = self.signal(&b);
        if enabled && signal.clicked {
            *pressed = Some(index);
        }
        let theme = self.theme();
        let font_size = self.font_size();
        let small = 8.max(font_size - 2);
        let label = label.to_owned();
        let hint = hint.to_owned();
        self.draw(&b, move |paint, rect| {
            let (x, y, _, h) = rect;
            paint_button_ground(paint, &theme, enabled && signal.held, enabled && signal.hovered, on, false, rect);
            let ty = paint.text_top(None, y, h);
            let ink = if !enabled {
                theme::ink_3(&theme)
            } else if dim {
                theme::ink_2(&theme)
            } else {
                theme.foreground
            };
            paint.text_c((x + 1.0 + pad, ty), ink, &label);
            if !hint.is_empty() {
                let hy = paint.text_top(Some(small), y, h);
                paint.text_s((x + 1.0 + pad + mono_width(0.0, font_size, &label) + 6.0, hy), small, theme::ink_3(&theme), &hint);
            }
        });
    }

    /// A single-line field. A click places the caret and Shift-click or a drag extends the
    /// selection, a double click selects the word (a triple the line), the word keys move and
    /// delete by words, Command-arrows (Apple) and Home/End move by lines, Command-A/C/X/V select
    /// all, copy, cut and paste, and Command-Z, Shift-Command-Z (or Command-Y) undo and redo the
    /// focused text.
    pub fn text_field(&mut self, text: &str, value: &str) -> String {
        let row = self.kit_row(Some(CLICKABLE | FOCUSABLE | BLOCKING | FOCUS_MARK), Some(control_hit(value_control)), text);
        let signal = self.signal(&row);
        let focused = self.focused(&row);
        let mut edit = if focused {
            self.load_text_edit(row.box_key, value)
        } else {
            let end = value.len();
            TextEdit { text: value.to_owned(), caret: end, anchor: end }
        };
        if focused {
            let (cx, _, cw, _) = value_control(ints(self.rect(&row)));
            let shift = self.press_shift(&row);
            self.point_text_caret(None, &mut edit, &signal, shift, (cx + 2) as f64, (cx + cw - 2) as f64);
            for (event, modifiers) in self.key_events(&row) {
                self.edit_text_event(false, &mut edit, &|_| true, &modifiers, &event);
            }
            self.save_text_edit(&edit);
        }
        let value = edit.text.clone();
        let theme = self.theme();
        let shown = display(text);
        let composition = self.composition().to_owned();
        let hovered = signal.hovered;
        // the I-beam over the value, as in any text field
        {
            let (px, py) = self.pointer();
            let (cx, cy, cw, ch) = value_control(ints(self.rect(&row)));
            if hovered && px >= cx as f64 && px < (cx + cw) as f64 && py >= cy as f64 && py < (cy + ch) as f64 {
                self.request_cursor(Cursor::Text);
            }
        }
        let font_size = self.font_size();
        let shown_value = value.clone();
        self.draw(&row, move |paint, rect| {
            let bounds = ints(rect);
            let (x, y, _, h) = bounds;
            let control = value_control(bounds);
            let (cx, cy, cw, ch) = control;
            if hovered {
                hover_row(paint, &theme, bounds);
            }
            kit_text(paint, Some(theme::ink_2(&theme)), x + SIDE, label_y(font_size, y, h), &shown);
            underline(paint, control, if focused { theme.accent } else { theme::edge(&theme) });
            if focused {
                paint_text_edit(paint, None, false, 2.0, true, control, label_y(font_size, y, h), &composition, &edit);
            } else {
                paint.input_region(0.0, cx as f64, cy as f64, cw as f64, ch as f64, false);
                kit_text(paint, Some(theme.foreground), cx + 2, label_y(font_size, y, h), &shown_value);
            }
        });
        value
    }

    /// A one-line search field at `at` (the first string is its key, the second its value) with a
    /// hairline: the new value and what its keys asked, Enter or Down `Next`, Shift-Enter or Up
    /// `Previous`, Escape `Close`. `grab` takes the keyboard for it.
    #[allow(clippy::too_many_arguments)]
    pub fn line_field(&mut self, at: (f64, f64), w: f64, h: f64, grab: bool, placeholder: &str, label: &str, value: &str) -> (String, LineStep) {
        let field = self.box_(label, BoxOpts::default().flags(CLICKABLE | FOCUSABLE | BLOCKING | FOCUS_MARK).at(at).w(Size::Px(w)).h(Size::Px(h)));
        if grab {
            self.focus_box(&field);
        }
        let signal = self.signal(&field);
        let focused = self.focused(&field);
        let mut edit = if focused {
            self.load_text_edit(field.box_key, value)
        } else {
            TextEdit { text: value.to_owned(), caret: value.len(), anchor: value.len() }
        };
        let mut step = LineStep::None;
        if focused {
            let (fx, _, fw, _) = ints(self.rect(&field));
            let shift = self.press_shift(&field);
            self.point_text_caret(None, &mut edit, &signal, shift, (fx + 2) as f64, (fx + fw - 2) as f64);
            for (event, modifiers) in self.key_events(&field) {
                match &event {
                    Event::Key_pressed(Key::Enter) if !self.command_modifiers(&modifiers) => {
                        step = if modifiers.contains(&Key::Shift) { LineStep::Previous } else { LineStep::Next };
                    }
                    Event::Key_pressed(Key::ArrowDown) => step = LineStep::Next,
                    Event::Key_pressed(Key::ArrowUp) => step = LineStep::Previous,
                    Event::Key_pressed(Key::Escape) => step = LineStep::Close,
                    event => {
                        self.edit_text_event(false, &mut edit, &|_| true, &modifiers, event);
                    }
                }
            }
            self.save_text_edit(&edit);
        }
        if signal.hovered {
            self.request_cursor(Cursor::Text);
        }
        let theme = self.theme();
        let composition = self.composition().to_owned();
        let font_size = self.font_size();
        let placeholder = placeholder.to_owned();
        let pedit = edit.clone();
        self.draw(&field, move |paint, rect| {
            let control = ints(rect);
            let (x, y, _, h) = control;
            underline(paint, control, if focused { theme.accent } else { theme::edge(&theme) });
            if focused {
                paint_text_edit(paint, None, false, 2.0, true, control, label_y(font_size, y, h), &composition, &pedit);
            } else {
                kit_text(
                    paint,
                    Some(if pedit.text.is_empty() { theme::ink_3(&theme) } else { theme.foreground }),
                    x + 2,
                    label_y(font_size, y, h),
                    if pedit.text.is_empty() { &placeholder } else { &pedit.text },
                );
            }
        });
        (edit.text, step)
    }
}

/// What a `line_field`'s keys asked.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LineStep {
    None,
    Next,
    Previous,
    Close,
}

pub fn context_clicked(signal: &Signal) -> bool {
    let ((px, py), (rx, ry)) = (signal.press_point, signal.release_point);
    signal.released && signal.button == Some(MouseButton::RightButton) && (px - rx) * (px - rx) + (py - ry) * (py - ry) < 16.0
}

/// Case-insensitive subsequence match.
pub fn fuzzy_match(query: &str, text: &str) -> bool {
    // ponytail: case-insensitive subsequence match, no scoring; swap for fzf-style ranking if
    // lists grow long.
    let query = query.to_ascii_lowercase();
    let text = text.to_ascii_lowercase();
    let q = query.as_bytes();
    let mut index = 0;
    for c in text.bytes() {
        if index == q.len() {
            break;
        }
        if c == q[index] {
            index += 1;
        }
    }
    index == q.len()
}

// ------------------------------------------------------------------ text area

/// One ranked suggestion of `Language::complete`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Completion {
    /// the byte span `insert` replaces
    pub replace: (i64, i64),
    pub insert: String,
    /// the row
    pub label: String,
    /// the row's right column: a type, a category
    pub detail: String,
    /// one line under the rows while the row is selected
    pub doc: String,
}

/// What a character typed at the caret lands in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeContext {
    Code,
    String,
    Comment,
}

/// What a code editor knows about its text. A host supplies one (the editor's Lisp); the text area
/// itself is language-free. Byte positions are byte offsets into the text.
#[derive(Clone)]
pub struct Language {
    /// sorted, non-overlapping byte spans and their colour; the rest is the foreground
    pub colorize: Rc<dyn Fn(&str) -> Vec<(i64, i64, Color)>>,
    /// the matched bracket pairs as (open, close) byte positions: the pair at the caret is lit
    pub brackets: Rc<dyn Fn(&str) -> Vec<(i64, i64)>>,
    /// `indent text caret`: the indentation Enter puts after the line break at `caret`
    pub indent: Rc<dyn Fn(&str, i64) -> String>,
    /// brackets typed in pairs: the opener wraps the selection or inserts both, the closer typed
    /// before itself steps over it, Backspace between an empty pair takes both
    pub pairs: Vec<(char, char)>,
    /// `complete text caret`: the ranked suggestions for the token ending at `caret`, best first
    pub complete: Rc<dyn Fn(&str, i64) -> Vec<Completion>>,
    /// `describe text byte`: the token at `byte` and its description, shown as a tooltip
    pub describe: Rc<dyn Fn(&str, i64) -> Option<(i64, i64, String)>>,
    /// `number_at text byte`: the numeric literal at `byte`; dragging it sideways changes the value
    pub number_at: Rc<dyn Fn(&str, i64) -> Option<(i64, i64)>>,
    /// `context_at text caret`: what a character typed at `caret` lands in
    pub context_at: Rc<dyn Fn(&str, i64) -> CodeContext>,
    /// what starts a line comment: Command-/ comments or uncomments the lines the selection touches
    pub comment: String,
    /// `rewrite text caret`, run after every frame that edited the text: the text as the language
    /// keeps it and where `caret` lands in it
    pub rewrite: Option<Rc<dyn Fn(&str, i64) -> (String, i64)>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrubPhase {
    Live,
    Done,
}

/// Options of `text_area_submit` (see there).
#[derive(Clone)]
pub struct TextAreaOpts {
    pub wrap: bool,
    pub errors: Vec<i64>,
    pub messages: Vec<(i64, Option<(i64, i64)>, String)>,
    pub spans: Vec<(i64, i64)>,
    pub reveal: Option<i64>,
    pub reveal_nonce: i64,
    pub select: Option<(i64, i64)>,
    pub splice: Vec<(i64, i64, String)>,
    pub language: Option<Language>,
    pub on_context: Option<Rc<dyn Fn((f64, f64))>>,
    pub on_scrub: Option<Rc<dyn Fn(ScrubPhase)>>,
    pub on_scrub_edit: Option<Rc<dyn Fn((i64, i64), &str)>>,
    pub on_click: Option<Rc<dyn Fn(i64, bool)>>,
    pub on_caret: Option<Rc<dyn Fn(i64)>>,
    pub on_caret_move: Option<Rc<dyn Fn(i64)>>,
    pub on_select: Option<Rc<dyn Fn((i64, i64))>>,
    pub on_drop: Option<Rc<dyn Fn(i64, Drop)>>,
    pub chips: Vec<(i64, i64, Color)>,
}

impl Default for TextAreaOpts {
    fn default() -> TextAreaOpts {
        TextAreaOpts {
            wrap: false,
            errors: Vec::new(),
            messages: Vec::new(),
            spans: Vec::new(),
            reveal: None,
            reveal_nonce: 0,
            select: None,
            splice: Vec::new(),
            language: None,
            on_context: None,
            on_scrub: None,
            on_scrub_edit: None,
            on_click: None,
            on_caret: None,
            on_caret_move: None,
            on_select: None,
            on_drop: None,
            chips: Vec::new(),
        }
    }
}

type Rows = Rc<Vec<(usize, usize, usize)>>;
type Notes = Vec<(usize, Option<(i64, i64)>, String)>;

fn start_of(rows: &Rows, i: usize) -> usize {
    rows[i].0
}

fn stop_of(rows: &Rows, i: usize) -> usize {
    rows[i].1
}

fn row_text(text: &str, rows: &Rows, i: usize) -> String {
    text[start_of(rows, i)..stop_of(rows, i)].to_owned()
}

/// the row holding byte `index`
pub fn row_at(rows: &[(usize, usize, usize)], index: usize) -> usize {
    let (mut low, mut high) = (0usize, rows.len() - 1);
    while low < high {
        let middle = (low + high + 1) / 2;
        if rows[middle].0 <= index {
            low = middle;
        } else {
            high = middle - 1;
        }
    }
    low
}

fn slot_of(notes: &Notes, i: usize) -> usize {
    i + notes.iter().filter(|(after, _, _)| *after < i).count()
}

fn row_at_slot(notes: &Notes, count: usize, k: i64) -> usize {
    if notes.is_empty() {
        0.max((count as i64 - 1).min(k)) as usize
    } else {
        let mut i = 0usize;
        while i + 1 < count && (slot_of(notes, i + 1) as i64) <= k {
            i += 1;
        }
        i
    }
}

fn gutter_of(char_w: f64, lines: usize) -> f64 {
    36.0 + char_w * 0.max(lines.to_string().len() as i64 - 3) as f64
}

fn clamp_pos(v: i64) -> usize {
    v.max(0) as usize
}

/// How a text area wraps: the gutter width follows the number of logical lines, the wrap width
/// follows the gutter.
#[derive(Clone, Copy)]
struct Wrap {
    wrap: bool,
    w: f64,
    char_w: f64,
}

#[derive(Default)]
struct AreaState {
    selected: usize,
    chosen: bool,
    closed: bool,
    submitted: bool,
    changed: bool,
    typed: bool,
}

type Scrubbing = Option<(usize, String, f64, bool)>;

impl Ui {
    /// The rows of a text: (start, stop, logical line) with `stop` exclusive and before the
    /// newline. Without `cols` a row is a line; with it a line continues every `cols` characters.
    /// An unchanged text costs no walk: the handle keeps the rows of the last few texts it laid
    /// out. ponytail: 8 panes showing text; a ninth walks.
    pub fn text_rows(&mut self, cols: Option<usize>, text: &str) -> Rows {
        if let Some((_, _, rows)) = self.text_rows_seen.iter().find(|(c, t, _)| *c == cols && t == text) {
            return rows.clone();
        }
        let n = text.len();
        let mut rows: Vec<(usize, usize, usize)> = Vec::new();
        let mut start = 0usize;
        let mut logical = 0usize;
        loop {
            let stop = text[start..].find('\n').map(|i| start + i).unwrap_or(n);
            let mut from = start;
            loop {
                match cols {
                    None => {
                        rows.push((from, stop, logical));
                        break;
                    }
                    Some(cols) => {
                        let mut i = from;
                        let mut k = 1.max(cols);
                        while k > 0 && i < stop {
                            i += text[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
                            k -= 1;
                        }
                        if i >= stop {
                            rows.push((from, stop, logical));
                            break;
                        }
                        rows.push((from, i, logical));
                        from = i;
                    }
                }
            }
            if stop < n {
                start = stop + 1;
                logical += 1;
            } else {
                break;
            }
        }
        let rows = Rc::new(rows);
        self.text_rows_seen.insert(0, (cols, text.to_owned(), rows.clone()));
        self.text_rows_seen.truncate(8);
        rows
    }

    fn area_rows(&mut self, cfg: Wrap, text: &str) -> Rows {
        let logical = 1 + text.bytes().filter(|c| *c == b'\n').count();
        let cols = if !cfg.wrap {
            None
        } else {
            let gutter = gutter_of(cfg.char_w, logical);
            Some(((cfg.w - gutter - 16.0).max(1.0) / cfg.char_w.max(1.0)).floor() as usize)
        };
        self.text_rows(cols, text)
    }

    /// A language's colours for a text, kept like its rows: one list a pane on screen. A theme is
    /// part of the colours. ponytail: 8 panes showing text; a ninth colours again.
    pub fn text_colors(&mut self, language: &Language, text: &str) -> Rc<Vec<(i64, i64, Color)>> {
        let theme = self.theme();
        if let Some((_, _, colors)) = self.text_colors_seen.iter().find(|(seen, known, _)| *seen == theme && known == text) {
            return colors.clone();
        }
        let colors = Rc::new((language.colorize)(text));
        self.text_colors_seen.insert(0, (theme, text.to_owned(), colors.clone()));
        self.text_colors_seen.truncate(8);
        colors
    }

    /// A box at the root, laid out and painted after every pane (like a tooltip), so a popup under
    /// a caret is never clipped by its pane.
    fn overlay_box(&mut self, flags: Flags, at: (f64, f64), w: f64, h: f64, label: &str) -> UiBox {
        let saved = self.enter_root();
        let overlay = self.box_(label, BoxOpts::default().flags(flags).w(Size::Px(w)).h(Size::Px(h)).at(at));
        self.push_overlay(overlay.index);
        self.leave(saved);
        overlay
    }

    fn accept_completion(&mut self, edit: &mut TextEdit, suggest: &UiBox, st: &mut AreaState, c: &Completion) {
        self.remember(edit, false);
        let (a, b) = c.replace;
        let a = (a.max(0) as usize).min(edit.text.len());
        let b = (b.max(0) as usize).min(edit.text.len());
        edit.text = format!("{}{}{}", &edit.text[..a], c.insert, &edit.text[b..]);
        edit.caret = a + c.insert.len();
        edit.anchor = edit.caret;
        self.set_state(suggest, 0);
        st.closed = true;
    }

    /// a dragged number: the literal at the press follows the pointer
    fn apply_scrub(&mut self, scrubbing: &Scrubbing, on_scrub: &Option<Rc<dyn Fn(ScrubPhase)>>, suggest: &UiBox, on_scrub_edit: &Option<Rc<dyn Fn((i64, i64), &str)>>, edit: &mut TextEdit) {
        if let Some((start, literal, dx, first)) = scrubbing {
            let start = *start;
            if start <= edit.text.len() && !literal.is_empty() {
                // the whole drag is one step of the editor's undo
                if *first {
                    self.remember(edit, false);
                }
                let mut stop = start;
                let bytes = edit.text.as_bytes();
                while stop < bytes.len() && numeric_character(bytes[stop] as char) {
                    stop += 1;
                }
                let coarse = match self.input_frame() {
                    Some(frame) => frame.input.keys.contains(&Key::Shift),
                    None => false,
                };
                let next = scrubbed(literal, *dx, coarse);
                if let Some(f) = on_scrub_edit {
                    f((start as i64, stop as i64), &next);
                }
                edit.text = format!("{}{}{}", &edit.text[..start], next, &edit.text[stop..]);
                edit.caret = start + next.len();
                edit.anchor = edit.caret;
                self.set_state(suggest, 0);
                if let Some(f) = on_scrub {
                    f(ScrubPhase::Live);
                }
            }
        }
    }

    /// the keys of a focused text area: completion, pairs, indent, line edits, movement, typing
    #[allow(clippy::too_many_arguments)]
    fn handle_keys(
        &mut self,
        body: &UiBox,
        cfg: Wrap,
        page: i64,
        language: &Option<Language>,
        suggest: &UiBox,
        open: i64,
        items: &[Completion],
        shown: usize,
        st: &mut AreaState,
        edit: &mut TextEdit,
    ) {
        for (event, modifiers) in self.key_events(body) {
            let command = self.command_modifiers(&modifiers);
            let shift = modifiers.contains(&Key::Shift);
            let rows = self.area_rows(cfg, &edit.text);
            let count = rows.len();
            let line = row_at(&rows, edit.caret);
            // Up and Down keep the column they started from across shorter rows
            match &event {
                Event::Key_pressed(Key::ArrowUp) | Event::Key_pressed(Key::ArrowDown) if !command => {}
                Event::Key_pressed(Key::PageUp | Key::PageDown | Key::Shift | Key::Ctrl | Key::Alt | Key::Meta) => {}
                _ => self.edit_column = -1.0,
            }
            let listing = shown > 0 && !st.closed;
            match &event {
                Event::Key_pressed(Key::Enter) if command => st.submitted = true,
                // the completion popup takes Up, Down, Tab, Enter and Escape while it is open
                Event::Key_pressed(key @ (Key::ArrowUp | Key::ArrowDown)) if listing => {
                    let n = items.len();
                    st.chosen = true;
                    st.selected = (st.selected + if *key == Key::ArrowUp { n - 1 } else { 1 }) % n;
                }
                Event::Key_pressed(Key::Tab) if listing && !command => {
                    let c = items[st.selected].clone();
                    self.accept_completion(edit, suggest, st, &c);
                }
                Event::Key_pressed(Key::Enter) if listing && st.chosen => {
                    let c = items[st.selected].clone();
                    self.accept_completion(edit, suggest, st, &c);
                }
                Event::Key_pressed(Key::Escape) if open != 0 && !st.closed => {
                    self.set_state(suggest, 0);
                    st.closed = true;
                }
                Event::Key_pressed(Key::Escape) => self.unfocus(),
                Event::Key_pressed(Key::Enter) => {
                    let indent = match language {
                        Some(l) => (l.indent)(&edit.text, text_selection(edit).0 as i64),
                        None => String::new(),
                    };
                    self.change(edit, st);
                    replace_text(edit, &format!("\n{}", indent));
                    if open != 0 {
                        self.set_state(suggest, 0);
                        st.closed = true;
                    }
                }
                // brackets come in pairs: an opener wraps the selection or inserts both, a closer
                // typed before itself steps over it, Backspace between an empty pair takes both
                Event::Text_input(s) if language.is_some() && s.len() == 1 && !command => {
                    let language_ref = language.as_ref().unwrap();
                    let c = s.chars().next().unwrap();
                    let pairs = &language_ref.pairs;
                    let (start, stop) = text_selection(edit);
                    let next = if edit.caret < edit.text.len() { Some(edit.text.as_bytes()[edit.caret] as char) } else { None };
                    // a bracket is a bracket only in code: in a string only a quote steps over its
                    // closing quote, and a comment takes every character as typed; in code a quote
                    // before a quote is the next string's opening one
                    let context = (language_ref.context_at)(&edit.text, edit.caret as i64);
                    let quote = pairs.contains(&(c, c));
                    let step_over = next == Some(c)
                        && match context {
                            CodeContext::Code => pairs.iter().any(|(_, close)| *close == c) && !quote,
                            CodeContext::String => quote,
                            CodeContext::Comment => false,
                        };
                    let close = pairs.iter().find(|(o, _)| *o == c).map(|(_, k)| *k);
                    match (context, close) {
                        (CodeContext::Code, Some(close)) if start != stop => {
                            self.change(edit, st);
                            let inner = edit.text[start..stop].to_owned();
                            replace_text(edit, &format!("{}{}{}", c, inner, close));
                            edit.anchor = start + 1;
                            edit.caret = stop + 1;
                        }
                        _ if step_over => {
                            edit.caret += 1;
                            edit.anchor = edit.caret;
                        }
                        (CodeContext::Code, Some(close)) => {
                            self.change(edit, st);
                            replace_text(edit, &format!("{}{}", c, close));
                            edit.caret -= 1;
                            edit.anchor = edit.caret;
                        }
                        _ => {
                            let changed = self.edit_text_event(true, edit, &|_| true, &modifiers, &event);
                            st.changed = changed || st.changed;
                        }
                    }
                    st.typed = true;
                }
                Event::Key_pressed(Key::Backspace)
                    if language.is_some()
                        && !command
                        && edit.caret == edit.anchor
                        && edit.caret > 0
                        && edit.caret < edit.text.len()
                        && language
                            .as_ref()
                            .unwrap()
                            .pairs
                            .contains(&(edit.text.as_bytes()[edit.caret - 1] as char, edit.text.as_bytes()[edit.caret] as char)) =>
                {
                    self.change(edit, st);
                    edit.anchor = edit.caret - 1;
                    edit.caret += 1;
                    replace_text(edit, "");
                    st.typed = true;
                }
                // Command-X / C with no selection take the caret's whole logical line, newline included
                Event::Key_pressed(Key::KeyChar(key @ ('x' | 'X' | 'c' | 'C'))) if command && edit.caret == edit.anchor => {
                    let s = line_start(&edit.text, edit.caret);
                    let e = edit.text.len().min(line_end(&edit.text, edit.caret) + 1);
                    if rays::clipboard::set_text(&edit.text[s..e]).is_ok() && (*key == 'x' || *key == 'X') {
                        self.change(edit, st);
                        edit.anchor = s;
                        edit.caret = e;
                        replace_text(edit, "");
                        st.typed = true;
                    }
                }
                Event::Key_pressed(Key::Tab) if !command && edit.caret != edit.anchor => {
                    // Tab and Shift-Tab with a selection indent and dedent every line it touches
                    let (a, b) = text_selection(edit);
                    let (first, stop) = line_block(&edit.text, a, b);
                    let block = edit.text[first..stop].to_owned();
                    let shifted = block
                        .split('\n')
                        .map(|l| {
                            if !shift {
                                if l.is_empty() { l.to_owned() } else { format!("  {}", l) }
                            } else if let Some(rest) = l.strip_prefix("  ") {
                                rest.to_owned()
                            } else if let Some(rest) = l.strip_prefix(' ') {
                                rest.to_owned()
                            } else {
                                l.to_owned()
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    if shifted != block {
                        self.change(edit, st);
                        replace_block(edit, first, stop, &shifted);
                        keep_selection(edit, first, first + shifted.len());
                    }
                }
                Event::Key_pressed(Key::Tab) if !command => {
                    if !shift {
                        self.change(edit, st);
                        replace_text(edit, "  ");
                    } else {
                        // Shift-Tab: up to two spaces leave the start of the line
                        let ls = line_start(&edit.text, edit.caret);
                        let b = edit.text.as_bytes();
                        let spaces = if ls < b.len() && b[ls] == b' ' {
                            if ls + 1 < b.len() && b[ls + 1] == b' ' { 2 } else { 1 }
                        } else {
                            0
                        };
                        if spaces > 0 {
                            self.change(edit, st);
                            edit.text = format!("{}{}", &edit.text[..ls], &edit.text[ls + spaces..]);
                            edit.caret = ls.max(edit.caret.saturating_sub(spaces));
                            edit.anchor = edit.caret;
                        }
                    }
                }
                // line and form edits: Command-/ toggles comments, Option-Up and Down move the
                // lines, with Shift duplicate them, Command-Option-Up selects the enclosing form,
                // Command-Shift-\ jumps to the matching bracket
                Event::Key_pressed(Key::KeyChar('/')) if command && language.is_some() => {
                    let (a, b) = text_selection(edit);
                    let (first, stop) = line_block(&edit.text, a, b);
                    let block = edit.text[first..stop].to_owned();
                    let toggled = toggle_comment(&language.as_ref().unwrap().comment, &block);
                    if toggled != block {
                        self.change(edit, st);
                        replace_block(edit, first, stop, &toggled);
                        let last = first + toggled.len();
                        if a == b {
                            edit.caret = first.max((a + last).saturating_sub(stop));
                            edit.anchor = edit.caret;
                        } else {
                            keep_selection(edit, first, last);
                        }
                    }
                }
                Event::Key_pressed(key @ (Key::ArrowUp | Key::ArrowDown)) if modifiers.contains(&Key::Alt) && !command => {
                    let (a, b) = text_selection(edit);
                    let (first, stop) = line_block(&edit.text, a, b);
                    let n = edit.text.len();
                    let block = edit.text[first..stop].to_owned();
                    let up = *key == Key::ArrowUp;
                    let shift_by = |edit: &mut TextEdit, d: i64| {
                        edit.anchor = (edit.anchor as i64 + d) as usize;
                        edit.caret = (edit.caret as i64 + d) as usize;
                    };
                    if shift {
                        // a copy below keeps the selection on the copy, a copy above on the original
                        self.change(edit, st);
                        replace_block(edit, stop, stop, &format!("\n{}", block));
                        if !up {
                            shift_by(edit, block.len() as i64 + 1);
                        }
                    } else if up && first > 0 {
                        let before = line_start(&edit.text, first - 1);
                        self.change(edit, st);
                        let above = edit.text[before..first - 1].to_owned();
                        replace_block(edit, before, stop, &format!("{}\n{}", block, above));
                        shift_by(edit, before as i64 - first as i64);
                    } else if !up && stop < n {
                        let after = line_end(&edit.text, stop + 1);
                        self.change(edit, st);
                        let below = edit.text[stop + 1..after].to_owned();
                        replace_block(edit, first, after, &format!("{}\n{}", below, block));
                        shift_by(edit, after as i64 - stop as i64);
                    }
                }
                Event::Key_pressed(Key::ArrowUp) if command && modifiers.contains(&Key::Alt) => {
                    let (a, b) = text_selection(edit);
                    if let Some(l) = language {
                        let pairs: Vec<(usize, usize)> = (l.brackets)(&edit.text).iter().map(|(o, c)| (clamp_pos(*o), clamp_pos(*c))).collect();
                        if let Some((o, c)) = enclosing_pair(&pairs, a, b) {
                            edit.anchor = o;
                            edit.caret = c;
                        }
                    }
                }
                Event::Key_pressed(Key::KeyChar('\\')) if command && shift && language.is_some() => {
                    let l = language.as_ref().unwrap();
                    let pairs: Vec<(usize, usize)> = (l.brackets)(&edit.text).iter().map(|(o, c)| (clamp_pos(*o), clamp_pos(*c))).collect();
                    if let Some(at) = bracket_jump(&pairs, edit.caret) {
                        edit.caret = at;
                        edit.anchor = at;
                    }
                }
                Event::Key_pressed(Key::ArrowUp) if command => move_caret(edit, 0, shift),
                Event::Key_pressed(Key::ArrowDown) if command => move_caret(edit, edit.text.len(), shift),
                Event::Key_pressed(Key::ArrowUp) => self.vertical(edit, &rows, count, line, -1, shift),
                Event::Key_pressed(Key::ArrowDown) => self.vertical(edit, &rows, count, line, 1, shift),
                Event::Key_pressed(Key::PageUp) => self.vertical(edit, &rows, count, line, -page, shift),
                Event::Key_pressed(Key::PageDown) => self.vertical(edit, &rows, count, line, page, shift),
                Event::Key_pressed(Key::Home | Key::ArrowLeft)
                    if self.line_modifiers(&modifiers) || event == Event::Key_pressed(Key::Home) =>
                {
                    move_caret(edit, start_of(&rows, line), shift)
                }
                Event::Key_pressed(Key::End | Key::ArrowRight)
                    if self.line_modifiers(&modifiers) || event == Event::Key_pressed(Key::End) =>
                {
                    move_caret(edit, stop_of(&rows, line), shift)
                }
                event => {
                    if self.edit_text_event(true, edit, &|_| true, &modifiers, event) {
                        st.changed = true;
                        if matches!(event, Event::Text_input(_) | Event::Key_pressed(Key::Backspace | Key::Delete)) {
                            st.typed = true;
                        }
                    }
                }
            }
        }
    }

    fn change(&mut self, edit: &TextEdit, st: &mut AreaState) {
        st.changed = true;
        self.remember(edit, false);
    }

    fn vertical(&mut self, edit: &mut TextEdit, rows: &Rows, count: usize, line: usize, step: i64, shift: bool) {
        let target = line as i64 + step;
        let x = if self.edit_column >= 0.0 {
            self.edit_column
        } else {
            let s = start_of(rows, line);
            self.text_width(None, &edit.text[s..edit.caret])
        };
        self.edit_column = x;
        if target < 0 {
            move_caret(edit, 0, shift);
        } else if target as usize >= count {
            move_caret(edit, edit.text.len(), shift);
        } else {
            let t = target as usize;
            let rt = row_text(&edit.text, rows, t);
            let at = start_of(rows, t) + self.text_caret_at(None, &rt, x);
            move_caret(edit, at, shift);
        }
    }

    /// A text area that also reports Command- or Ctrl-Enter pressed in it this frame (the host's
    /// "apply"). Tab inserts two spaces and Shift-Tab takes up to two leading spaces off the line
    /// (the editor keeps Tab instead of moving the focus). With `wrap` a long line continues on the
    /// next row, so nothing scrolls sideways; the gutter numbers logical lines and `errors` are
    /// logical lines. A `messages` entry (1-based line, the wrong byte span, the text) is a row of
    /// its own in the error colour under that line, with the span underlined. `language` colours
    /// the text, lights the bracket pair at the caret, indents after Enter, pairs brackets,
    /// completes the token at the caret and describes the token under the pointer. A numeric
    /// literal of the language dragged sideways follows the pointer (a float by a tenth of its last
    /// decimal place per point, an integer by one per six points, Shift ten times faster): `on_scrub
    /// Live` is called on each frame the returned text changed that way and `Done` when the drag
    /// ends. `reveal` scrolls the offset into view once per (offset, text length, `reveal_nonce`);
    /// `select` (anchor, caret) takes the keyboard for the area and places the selection; `splice`
    /// is a list of (start, stop, replacement) byte ranges of the given text applied together as
    /// one undo step before `select`.
    #[allow(clippy::too_many_arguments)]
    pub fn text_area_submit(&mut self, at: (f64, f64), w: f64, h: f64, o: &TextAreaOpts, label: &str, text: &str) -> (String, bool) {
        // a line of code is one and a half times its text, as code editors set it (17 points at
        // 11, 20 at 13): the 24-point row is a control's, twice the text
        let row = self.text_line_height() as f64;
        let body = self.box_(
            label,
            BoxOpts::default().flags(CLICKABLE | FOCUSABLE | BLOCKING | SCROLL | CLIP | KEEP_TAB).at(at).w(Size::Px(w)).h(Size::Px(h)).scroll_step(row),
        );
        // a `select` request takes the keyboard for the area
        if o.select.is_some() {
            self.focus_box(&body);
        }
        let char_w = self.text_width(None, "0");
        // the sheet's code area: a 36-point gutter (wider only past 999 lines), the text at its
        // edge, 6 points of padding above the first row
        let pad = 6.0;
        let cfg = Wrap { wrap: o.wrap, w, char_w };
        let rows = self.area_rows(cfg, text);
        let count = rows.len();
        let language = o.language.clone();
        // a diagnostic's message is a row of its own under the last row of its line
        let note_rows = |rows: &Rows| -> Notes {
            if o.messages.is_empty() {
                return Vec::new();
            }
            let mut found: Notes = Vec::new();
            for (line, span, message) in &o.messages {
                let mut last: Option<usize> = None;
                for (i, (_, _, logical)) in rows.iter().enumerate() {
                    if *logical as i64 == line - 1 {
                        last = Some(i);
                    }
                }
                if let Some(l) = last {
                    found.push((l, *span, message.clone()));
                }
            }
            found.sort_by(|a, b| a.0.cmp(&b.0));
            found
        };
        let logical_count = |rows: &Rows| rows[rows.len() - 1].2 + 1;
        // the content box scrolls; two empty boxes keep the completion popup's and the number
        // scrub's state between frames
        let saved = self.enter(&body);
        let content = self.box_(
            &format!("{}-content", label),
            BoxOpts::default().w(Size::Px(w)).h(Size::Px((count + note_rows(&rows).len()) as f64 * row + pad)),
        );
        let suggest = self.box_(&format!("{}-suggest", label), BoxOpts::default().w(Size::Px(0.0)).h(Size::Px(0.0)).at((0.0, 0.0)));
        let scrub = self.box_(&format!("{}-scrub", label), BoxOpts::default().w(Size::Px(0.0)).h(Size::Px(0.0)).at((0.0, 0.0)));
        self.leave(saved);
        let signal = self.signal(&body);
        // the I-beam over text; a number under the pointer asks for the scrub cursor further down,
        // and the last request wins
        if signal.hovered {
            self.request_cursor(Cursor::Text);
        }
        let focused = self.focused(&body);
        let gutter = gutter_of(char_w, logical_count(&rows));
        let (bx, by, bw, bh) = self.rect(&body);
        let horizontal0 = self.state(&body, 0) as f64;
        let scroll_pos = self.scroll_position(&body);
        // the byte at a point; `strict` answers only over the row's glyphs
        let point_in = |ui: &mut Ui, strict: bool, text: &str, rows: &Rows, (px, py): (f64, f64)| -> Option<usize> {
            let count = rows.len();
            let notes = note_rows(rows);
            let line = row_at_slot(&notes, count, ((py - by + scroll_pos - pad) / row).floor() as i64);
            let x = px - bx - gutter + horizontal0;
            let line_text = row_text(text, rows, line);
            if strict && (x < 0.0 || x > ui.text_width(None, &line_text) || py < by || py > by + bh) {
                None
            } else {
                Some(start_of(rows, line) + ui.text_caret_at(None, &line_text, 0.0f64.max(x)))
            }
        };
        // a number is dragged by its underline: the lowest 9 points of its row, so a drag that
        // selects text from a number does not rewrite it
        let on_handle = |(_, py): (f64, f64)| {
            let y = py - by + scroll_pos - pad;
            y - row * (y / row).floor() >= row - 9.0
        };
        let mut edit = if focused {
            self.load_text_edit(body.box_key, text)
        } else {
            TextEdit { text: text.to_owned(), caret: text.len(), anchor: text.len() }
        };
        let mut moved = false;
        let mut st = AreaState::default();
        if let Some(f) = &o.on_context {
            if context_clicked(&signal) {
                f(signal.release_point);
            }
        }
        let scrub_state = self.state(&scrub, 0);
        let mut scrubbing: Scrubbing = None;
        // arm_scrub: a press on a number arms a scrub (its state is the start + 1, negated once
        // dragging); three points sideways make it one, three points down a selection as before
        if let Some(l) = &language {
            if signal.pressed && signal.button == Some(MouseButton::LeftButton) {
                let number = if on_handle(signal.press_point) {
                    let byte = point_in(self, false, text, &rows, signal.press_point).unwrap();
                    (l.number_at)(text, byte as i64)
                } else {
                    None
                };
                match number {
                    Some((a, b)) => {
                        self.set_state(&scrub, a + 1);
                        self.set_text_state(&scrub, Some(text[clamp_pos(a)..clamp_pos(b)].to_owned()));
                    }
                    None => self.set_state(&scrub, 0),
                }
            } else if scrub_state != 0 && signal.held {
                let dx = signal.pointer.0 - signal.press_point.0;
                let dy = signal.pointer.1 - signal.press_point.1;
                if scrub_state < 0 || (dx.abs() >= 3.0 && dx.abs() >= dy.abs()) {
                    let start = scrub_state.abs() - 1;
                    self.set_state(&scrub, -(start + 1));
                    scrubbing = Some((start as usize, self.text_state(&scrub).unwrap_or_default(), dx, scrub_state > 0));
                } else if dy.abs() >= 3.0 {
                    self.set_state(&scrub, 0);
                }
            } else if scrub_state != 0 && !signal.held {
                self.set_state(&scrub, 0);
                if scrub_state < 0 {
                    if let Some(f) = &o.on_scrub {
                        f(ScrubPhase::Done);
                    }
                }
            }
        } else if scrub_state != 0 && !signal.held {
            self.set_state(&scrub, 0);
            if scrub_state < 0 {
                if let Some(f) = &o.on_scrub {
                    f(ScrubPhase::Done);
                }
            }
        }
        // the completion popup: its state is (token start + 1) * 512 + 256 once an arrow has moved
        // the selection + the selected row; the rows are built before the keys so a click on one
        // is seen this frame
        let open = if focused {
            self.state(&suggest, 0)
        } else {
            self.set_state(&suggest, 0);
            0
        };
        let items: Vec<Completion> = match &language {
            Some(l) if open != 0 => (l.complete)(&edit.text, edit.caret as i64),
            _ => Vec::new(),
        };
        let shown = 8.min(items.len());
        st.selected = ((open & 255) as usize).min(items.len().saturating_sub(1));
        // Enter takes the row only after Up or Down chose it; Tab takes it always
        st.chosen = open & 256 != 0;
        let first = (st.selected + 1).saturating_sub(shown);
        let popup: Option<(UiBox, Vec<UiBox>, f64, f64)> = if shown == 0 {
            None
        } else {
            let doc_empty = items[st.selected].doc.is_empty();
            let mut width: f64 = 220.0;
            for c in &items {
                width = width.max(self.text_width(None, &c.label) + self.text_width(None, &c.detail) + 44.0);
            }
            let (view_w, _) = self.view_size();
            let width = width.min(120.0f64.max(view_w - 16.0));
            let height = (shown as i64 * self.row_height()) as f64 + if doc_empty { 4.0 } else { 22.0 };
            let container = self.overlay_box(BLOCKING, (0.0, 0.0), width, height, &format!("{}-completions", label));
            let saved = self.enter(&container);
            let mut boxes = Vec::new();
            for i in 0..shown {
                boxes.push(self.kit_row(Some(CLICKABLE | BLOCKING | KEEP_FOCUS), None, &format!("row-{}", i)));
            }
            self.leave(saved);
            for (i, r) in boxes.iter().enumerate() {
                if self.signal(r).clicked {
                    let c = items[first + i].clone();
                    self.accept_completion(&mut edit, &suggest, &mut st, &c);
                }
            }
            Some((container, boxes, width, height))
        };
        if focused {
            let caret0 = edit.caret;
            // a triple click takes the logical line with its break
            let line_of = |e: &TextEdit, at: usize| {
                let s = line_start(&e.text, at);
                let en = line_end(&e.text, at);
                (s, if en < e.text.len() { en + 1 } else { en })
            };
            if signal.pressed && signal.button == Some(MouseButton::LeftButton) {
                self.edit_column = -1.0;
                let shift = self.press_shift(&body);
                let at = point_in(self, false, text, &rows, signal.press_point).unwrap();
                self.press_select(&mut edit, shift, signal.clicks, &line_of, at);
                // a double click on true / false flips it, as dragging flips a number
                if signal.clicks == 2 && language.is_some() {
                    let (a, b) = text_selection(&edit);
                    let flipped = match &edit.text[a..b] {
                        "true" => Some("false"),
                        "false" => Some("true"),
                        _ => None,
                    };
                    if let Some(word) = flipped {
                        self.remember(&edit, false);
                        replace_text(&mut edit, word);
                        edit.anchor = a;
                        edit.caret = a + word.len();
                    }
                }
            } else if scrubbing.is_none() && scrub_state == 0 && (signal.dragging || (signal.held && signal.pointer != signal.press_point)) {
                let at = point_in(self, false, text, &rows, signal.pointer).unwrap();
                self.drag_select(&mut edit, &line_of, at);
            }
            // the host's replacements (one undo step, last first so the offsets hold), then its selection
            if !o.splice.is_empty() {
                self.remember(&edit, false);
                let mut splice = o.splice.clone();
                splice.sort_by(|a, b| b.0.cmp(&a.0));
                for (a, b, by_) in &splice {
                    if 0 <= *a && a <= b && *b as usize <= edit.text.len() {
                        edit.text = format!("{}{}{}", &edit.text[..*a as usize], by_, &edit.text[*b as usize..]);
                    }
                }
                st.changed = true;
            }
            if let Some((a, b)) = o.select {
                let n = edit.text.len() as i64;
                edit.anchor = a.max(0).min(n) as usize;
                edit.caret = b.max(0).min(n) as usize;
            }
            let page = 1.max((bh / row) as i64 - 1);
            self.handle_keys(&body, cfg, page, &language, &suggest, open, &items, shown, &mut st, &mut edit);
            // a dragged number: the literal at the press follows the pointer (the caret sits after it)
            self.apply_scrub(&scrubbing, &o.on_scrub, &suggest, &o.on_scrub_edit, &mut edit);
            // typing opens the popup on the token at the caret (the selection starts over); a popup
            // left open follows the caret and closes when the caret leaves its token
            if let Some(l) = &language {
                if st.typed || (open != 0 && !st.closed) {
                    let found = (l.complete)(&edit.text, edit.caret as i64);
                    match found.first() {
                        Some(c) if st.typed => self.set_state(&suggest, (c.replace.0 + 1) * 512),
                        Some(c) if c.replace.0 == (((open as u64 >> 9) as i64)) - 1 => {
                            self.set_state(&suggest, (((open as u64 >> 9) as i64)) * 512 + if st.chosen { 256 } else { 0 } + st.selected as i64)
                        }
                        _ => self.set_state(&suggest, 0),
                    }
                }
            }
            // the language's rewrite (parinfer) after this frame's edits; the anchor maps like the caret
            if let Some(l) = &language {
                if let Some(rewrite) = &l.rewrite {
                    if st.changed {
                        let (text2, caret2) = rewrite(&edit.text, edit.caret as i64);
                        let anchor2 = if edit.anchor == edit.caret { caret2 } else { rewrite(&edit.text, edit.anchor as i64).1 };
                        edit.text = text2;
                        edit.caret = caret2 as usize;
                        edit.anchor = anchor2 as usize;
                    }
                }
            }
            self.save_text_edit(&edit);
            moved = edit.caret != caret0 || edit.text != text || signal.pressed;
        }
        // scrolling: the wheel, then whatever keeps the caret (or `reveal`) in view
        let final_text = edit.text.clone();
        if focused {
            if let Some(f) = &o.on_caret {
                f(edit.caret as i64);
            }
            if let Some(f) = &o.on_select {
                let (a, b) = text_selection(&edit);
                f((a as i64, b as i64));
            }
        }
        if focused && moved && (signal.pressed || signal.dragging || !self.key_events(&body).is_empty()) {
            if let Some(f) = &o.on_caret_move {
                f(edit.caret as i64);
            }
        }
        if let Some(f) = &o.on_click {
            if signal.clicked && signal.button == Some(MouseButton::LeftButton) && scrub_state >= 0 {
                if let Some(byte) = point_in(self, true, text, &rows, signal.release_point) {
                    let c = self.command_modifiers(&self.press_keys(&body));
                    f(byte as i64, c);
                }
            }
        }
        // a payload in flight over the area: the byte under the pointer, hovering or released
        if let Some(f) = &o.on_drop {
            if let Some(drop) = self.drop_target(&body) {
                if let Some(byte) = point_in(self, false, text, &rows, signal.pointer) {
                    f(byte as i64, drop);
                }
            }
        }
        let rows = if final_text == text { rows } else { self.area_rows(cfg, &final_text) };
        let count = rows.len();
        let notes = note_rows(&rows);
        // `reveal` scrolls once per (offset, length, nonce): the content box remembers it, and
        // forgets it while there is nothing to reveal
        let revealed = match o.reveal {
            Some(index)
                if bw > 0.0
                    && self.state(&content, 0)
                        != index.wrapping_mul(1_000_003) + final_text.len() as i64 + 1 + o.reveal_nonce.wrapping_mul(1_000_000_007) =>
            {
                self.set_state(&content, index.wrapping_mul(1_000_003) + final_text.len() as i64 + 1 + o.reveal_nonce.wrapping_mul(1_000_000_007));
                Some(index)
            }
            None => {
                self.set_state(&content, 0);
                None
            }
            _ => None,
        };
        let target = if moved { Some(edit.caret) } else { revealed.map(|index| final_text.len().min(clamp_pos(index))) };
        let longest = rows.iter().map(|(s, e, _)| e - s).max().unwrap_or(0);
        let visible = 1.0f64.max(bw - gutter - 16.0);
        let max_x = if o.wrap { 0.0 } else { 0.0f64.max(longest as f64 * char_w - visible) };
        let mut horizontal = 0.0f64.max(max_x.min(horizontal0 + signal.scroll.0 * row));
        let mut vertical = 0.0f64.max((0.0f64.max((count + notes.len()) as f64 * row + pad - bh)).min(self.scroll_offset(&body)));
        if let Some(index) = target {
            let line = row_at(&rows, index);
            let top = pad + slot_of(&notes, line) as f64 * row;
            if top < vertical {
                vertical = top;
            } else if top + row > vertical + bh {
                vertical = top + row - bh;
            }
            let ls = start_of(&rows, line);
            let x = self.text_width(None, &final_text[ls..index.min(stop_of(&rows, line))]);
            if x < horizontal {
                horizontal = x;
            } else if x - horizontal > visible {
                horizontal = x - visible;
            }
        }
        self.set_state(&body, horizontal as i64);
        if vertical != self.scroll_offset(&body) {
            self.set_scroll_offset(&body, vertical);
        }
        // painted where the shared elastic scroll puts the content, overshoot included
        let offset = self.scroll_position(&body);
        let theme = self.theme();
        let composition = self.composition().to_owned();
        // the language's colours and the bracket pair at the caret, once per frame
        let colors: Rc<Vec<(i64, i64, Color)>> = match &language {
            Some(l) => self.text_colors(l, &final_text),
            None => Rc::new(Vec::new()),
        };
        let matched: Option<(usize, usize)> = match &language {
            Some(l) if focused => {
                let c = edit.caret as i64;
                (l.brackets)(&final_text).into_iter().find(|(o, k)| *o == c - 1 || *k == c - 1 || *o == c || *k == c).map(|(a, b)| (clamp_pos(a), clamp_pos(b)))
            }
            _ => None,
        };
        // the pointer over a number invites a drag; over any other token it describes it after a rest
        let under_pointer = match &language {
            Some(_) if signal.hovered && self.active().is_none() => point_in(self, true, &final_text, &rows, signal.pointer),
            _ => None,
        };
        let hovered_number: Option<(usize, usize)> = match (&language, under_pointer, &scrubbing) {
            (_, _, Some((start, _, _, _))) => {
                let mut stop = *start;
                let b = final_text.as_bytes();
                while stop < b.len() && numeric_character(b[stop] as char) {
                    stop += 1;
                }
                Some((*start, stop))
            }
            (Some(l), Some(byte), None) if on_handle(signal.pointer) => (l.number_at)(&final_text, byte as i64).map(|(a, b)| (clamp_pos(a), clamp_pos(b))),
            _ => None,
        };
        if hovered_number.is_some() {
            self.request_cursor(Cursor::Horizontal_resize);
        }
        if let (Some(l), Some(byte)) = (&language, under_pointer) {
            if hovered_number.is_none() && open == 0 {
                if let Some((start, _, doc)) = (l.describe)(&final_text, byte as i64) {
                    if !doc.is_empty() {
                        self.tooltip(&format!("{}#{}", label, start), &doc);
                    }
                }
            }
        }
        // the completion popup sits under the caret's row (above it near the bottom of the view)
        let text_x0 = bx + gutter - horizontal;
        if let Some((container, boxes, width, height)) = &popup {
            let caret_line = row_at(&rows, edit.caret);
            let index = clamp_pos(items[st.selected].replace.0);
            let line = row_at(&rows, index);
            let ls = start_of(&rows, line);
            let anchor = text_x0 + self.text_width(None, &final_text[ls..index]);
            let (view_w, view_h) = self.view_size();
            let px = 8.0f64.max((anchor - 8.0).min(view_w - width - 8.0));
            let below = by - offset + pad + (slot_of(&notes, caret_line) + 1) as f64 * row;
            let py = if below + height <= view_h - 8.0 {
                below
            } else {
                8.0f64.max(by - offset + pad + slot_of(&notes, caret_line) as f64 * row - height)
            };
            self.set_at(container, (px, py));
            let selected = st.selected;
            let doc = items[selected].doc.clone();
            self.draw(container, move |paint, (x, y, w, h)| {
                paint.fill(x, y, w, h, theme.input);
                paint.frame(x, y, w, h, theme::edge(&theme));
                if !doc.is_empty() {
                    paint.text_c((x + 8.0, y + h - 18.0), theme::muted(&theme), &display(&doc));
                }
            });
            for (i, r) in boxes.iter().enumerate() {
                let c = items[first + i].clone();
                let hovered = self.signal(r).hovered;
                let is_sel = first + i == selected;
                let font_size = self.font_size();
                self.draw(r, move |paint, rect| {
                    let bounds = ints(rect);
                    let (rx, ry, rw, rh) = bounds;
                    if is_sel {
                        fill_i(paint, bounds, theme.control);
                    } else if hovered {
                        hover_row(paint, &theme, bounds);
                    }
                    kit_text(paint, Some(theme.foreground), rx + SIDE, label_y(font_size, ry, rh), &c.label);
                    let dw = paint.text_width(None, &c.detail) as i64;
                    kit_text(paint, Some(theme::muted(&theme)), rx + rw - dw - SIDE, label_y(font_size, ry, rh), &c.detail);
                });
            }
        }
        // the text area's painting: gutter, selection, runs, diagnostics, caret
        let painted = AreaPaint {
            colors,
            composition,
            messages: o.messages.clone(),
            chips: o.chips.clone(),
            hovered_number,
            scrubbing: scrubbing.is_some(),
            matched,
            spans: o.spans.clone(),
            errors: o.errors.clone(),
            notes,
            char_w,
            final_text,
            rows,
            count,
            focused,
            edit: edit.clone(),
            row,
            pad,
            offset,
            horizontal,
            gutter,
            theme,
        };
        self.draw(&body, move |paint, rect| painted.paint(paint, rect));
        (edit.text, st.submitted)
    }
}

struct AreaPaint {
    colors: Rc<Vec<(i64, i64, Color)>>,
    composition: String,
    messages: Vec<(i64, Option<(i64, i64)>, String)>,
    chips: Vec<(i64, i64, Color)>,
    hovered_number: Option<(usize, usize)>,
    scrubbing: bool,
    matched: Option<(usize, usize)>,
    spans: Vec<(i64, i64)>,
    errors: Vec<i64>,
    notes: Notes,
    char_w: f64,
    final_text: String,
    rows: Rows,
    count: usize,
    focused: bool,
    edit: TextEdit,
    row: f64,
    pad: f64,
    offset: f64,
    horizontal: f64,
    gutter: f64,
    theme: Theme,
}

impl AreaPaint {
    fn paint(&self, paint: &mut Paint, (bx, by, bw, bh): Rect) {
        let theme = self.theme;
        let final_text = self.final_text.as_str();
        let rows = &self.rows;
        let row = self.row;
        let char_w = self.char_w;
        let (gutter, offset, pad) = (self.gutter, self.offset, self.pad);
        let edit = &self.edit;
        paint.fill(bx, by, bw, bh, theme.input);
        let previous = paint.clip_rect;
        let text_x = bx + gutter - self.horizontal;
        paint.fill(bx + gutter, by, 1.0, bh, theme::faint_border(&theme));
        let small = paint.label_size();
        let first = 0.max(((offset - pad) / row).floor() as i64 - self.notes.len() as i64);
        let last = (self.count as i64 - 1).min(((offset + bh) / row).floor() as i64);
        let caret_line = row_at(rows, edit.caret);
        let selected = self.focused && edit.caret != edit.anchor;
        let (s0, s1) = text_selection(edit);
        let width = |paint: &mut Paint, t: &str| paint.text_width(None, t);
        let clip = |paint: &mut Paint, x: f64, w: f64| paint.clip_rect = intersect(previous, (x, by, w, bh));
        let band = |paint: &mut Paint, line: usize, start: usize, stop: usize, color: Color, y: f64| {
            let ls = start_of(rows, line);
            let le = stop_of(rows, line);
            // a selection runs on past the end of a row that ends a line
            let newline = le < final_text.len() && final_text.as_bytes()[le] == b'\n';
            let start = start.max(ls);
            let stop = stop.min(if newline { le + 1 } else { le });
            if start < stop || (start <= le && stop > le) {
                let x0 = width(paint, &final_text[ls..start.min(le)]);
                let x1 = if stop > le { width(paint, &final_text[ls..le]) + char_w } else { width(paint, &final_text[ls..stop]) };
                paint.fill(text_x + x0, y, 1.0f64.max(x1 - x0), row, color);
            }
        };
        // the colours are sorted and the rows come in order: one walk of them, whatever the rows
        let mut colors_ahead = 0usize;
        let colors = &*self.colors;
        let mut line = first;
        while line <= last {
            let lu = line as usize;
            let y = by - offset + pad + slot_of(&self.notes, lu) as f64 * row;
            let text_y = y + 0.max((paint.font_size + (paint.font_size + 1) / 2 - paint.font_size) / 2) as f64;
            let logical = rows[lu].2;
            let starts_line = lu == 0 || rows[lu - 1].2 != logical;
            clip(paint, bx, bw);
            let wrong = self.errors.contains(&(logical as i64 + 1));
            let current = self.focused && lu == caret_line;
            if wrong {
                paint.fill(bx, y, bw, row, color::with_alpha(theme::invalid(), 18));
            } else if current {
                paint.fill(bx, y, bw, row, theme::faint_border(&theme));
            }
            if starts_line {
                let number = (logical + 1).to_string();
                let number_x = bx + gutter - 10.0 - paint.text_width(Some(small), &number);
                if wrong {
                    paint.fill(number_x - 10.0, y + row / 2.0 - 3.0, 6.0, 6.0, theme::invalid());
                }
                paint.text_s(
                    (number_x, text_y + 1.0),
                    small,
                    if wrong {
                        theme::invalid()
                    } else if current {
                        theme.foreground
                    } else {
                        theme::ink_3(&theme)
                    },
                    &number,
                );
            }
            clip(paint, bx + gutter, bw - gutter);
            for (a, b) in &self.spans {
                band(paint, lu, clamp_pos(*a), clamp_pos(*b), theme::tint(&theme), y);
            }
            if selected {
                band(paint, lu, s0, s1, theme::tint(&theme), y);
            }
            // the bracket pair at the caret: an accent outline
            if let Some((o, k)) = self.matched {
                for b in [o, k] {
                    let ls = start_of(rows, lu);
                    let le = stop_of(rows, lu);
                    if b >= ls && b < le {
                        // the glyph's box (6.5 x 14) with a 1-point outline outside it: 8.5 x 16
                        let x = text_x + width(paint, &final_text[ls..b]) - 1.0;
                        paint.frame(x, y + 2.0, char_w + 2.0, 16.0, theme.accent);
                    }
                }
            }
            // a number under the pointer (or being dragged) wears an accent underline
            if let Some((a, b)) = self.hovered_number {
                let ls = start_of(rows, lu);
                let le = stop_of(rows, lu);
                if a >= ls && a < le {
                    let x0 = width(paint, &final_text[ls..a]);
                    let x1 = width(paint, &final_text[ls..b.min(le)]);
                    paint.fill(
                        text_x + x0,
                        y + row - 4.0,
                        1.0f64.max(x1 - x0),
                        2.0,
                        if self.scrubbing { theme.accent } else { color::with_alpha(theme.accent, 160) },
                    );
                }
            }
            let ls = start_of(rows, lu);
            let le = stop_of(rows, lu);
            // a colour literal wears its colour as a bar under it
            for (a, b, c) in &self.chips {
                let (a, b) = (clamp_pos(*a), clamp_pos(*b));
                if a >= ls && a < le && b <= final_text.len() {
                    let x0 = width(paint, &final_text[ls..a]);
                    let w1 = width(paint, &final_text[a..b.min(le)]);
                    paint.fill(text_x + x0, y + row - 5.0, w1, 4.0, *c);
                }
            }
            // the wrong span of a diagnostic is underlined in the error colour
            for (_, span, _) in &self.messages {
                if let Some((a, b)) = span {
                    let (a, b) = (clamp_pos(*a), clamp_pos(*b));
                    if a < le && b > ls && b > a && b <= final_text.len() {
                        let a = a.max(ls);
                        let b = b.min(le);
                        let x0 = width(paint, &final_text[ls..a]);
                        let w1 = 1.0f64.max(width(paint, &final_text[a..b]));
                        paint.fill(text_x + x0, y + row - 3.0, w1, 1.0, theme::invalid());
                    }
                }
            }
            let line_str = &final_text[ls..le];
            if self.focused && lu == caret_line && !self.composition.is_empty() {
                let before = &final_text[ls..edit.caret];
                let caret_x = text_x + width(paint, before);
                paint.text_c((text_x, text_y), theme.foreground, before);
                paint.text_c((caret_x, text_y), theme.foreground, &self.composition);
                let cw = width(paint, &self.composition);
                paint.text_c((caret_x + cw, text_y), theme.foreground, &line_str[before.len()..]);
            } else if colors.is_empty() {
                paint.text_c((text_x, text_y), theme.foreground, line_str);
            } else {
                // the row in coloured runs, the gaps in the foreground colour
                while colors_ahead < colors.len() && (colors[colors_ahead].1 as usize) <= ls {
                    colors_ahead += 1;
                }
                let mut pos = ls;
                let mut k = colors_ahead;
                let run = |paint: &mut Paint, a: usize, b: usize, c: Color| {
                    if a < b {
                        let ax = text_x + width(paint, &final_text[ls..a]);
                        paint.text_c((ax, text_y), c, &final_text[a..b]);
                    }
                };
                while k < colors.len() && (colors[k].0 as usize) < le {
                    let (a, b, c) = colors[k];
                    let a = (a as usize).max(ls);
                    let b = (b as usize).min(le);
                    if a >= b || a < pos {
                        k += 1;
                        continue;
                    }
                    run(paint, pos, a, theme.foreground);
                    run(paint, a, b, c);
                    pos = b;
                    k += 1;
                }
                run(paint, pos, le, theme.foreground);
            }
            line += 1;
        }
        // the message rows: the diagnostic at the label size, under the indent of its line
        clip(paint, bx + gutter, bw - gutter);
        for (k, (after, _, message)) in self.notes.iter().enumerate() {
            let y = by - offset + pad + (*after + 1 + k) as f64 * row;
            if y + row > by && y < by + bh {
                let ls = start_of(rows, *after);
                let mut indent = 0;
                while ls + indent < final_text.len() && final_text.as_bytes()[ls + indent] == b' ' {
                    indent += 1;
                }
                let iw = width(paint, &" ".repeat(indent));
                let ty = paint.text_top(Some(small), y, row);
                paint.text_s((text_x + iw, ty), small, theme::invalid(), message);
            }
        }
        clip(paint, bx + gutter, bw - gutter);
        if self.focused {
            let y = by - offset + pad + slot_of(&self.notes, caret_line) as f64 * row;
            let ls = start_of(rows, caret_line);
            let caret_x = text_x + width(paint, &final_text[ls..edit.caret]);
            paint.input_region(caret_x - bx, bx, y, bw, row, true);
            paint.fill(caret_x.round(), y + 2.0, 1.0, row - 4.0, theme.accent);
        } else {
            paint.input_region(0.0, bx, by, bw, bh, false);
        }
        paint.clip_rect = previous;
    }
}

// ------------------------------------------------------------------ picker

/// The result of a `picker` for a frame.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Pick {
    None,
    Pick(i64),
    Delete(i64),
    Submit,
    Back,
    Cancel,
}

#[derive(Clone)]
pub struct PickerOpts {
    pub limit: i64,
    pub mark: Option<Rc<dyn Fn(i64) -> Option<Color>>>,
    pub off: Option<Rc<dyn Fn(i64) -> bool>>,
    pub slash: bool,
    pub at_rest: bool,
}

impl Default for PickerOpts {
    fn default() -> PickerOpts {
        PickerOpts { limit: 10, mark: None, off: None, slash: true, at_rest: false }
    }
}

impl Ui {
    /// A focused search row (the label is its placeholder and key) over the `(label, detail)` rows
    /// for the current query, windowed to `limit` (default 10) around the cursor. Rows are
    /// recomputed as typing changes the query, so indices refer to the rows of the returned query.
    /// Returns the edited query and at most one result: `Pick` on Enter or a row click, `Submit` on
    /// Enter with no rows, `Delete` on a second Delete over the same (red, armed) row, `Back` on
    /// Backspace or Left with an empty query, and `Cancel` on Escape. `mark` gives a row its type
    /// square. An `off` row (one that cannot be chosen here) reads in ink-3 whole, square included,
    /// and neither Enter nor a click picks it. With `at_rest` the field keeps the keyboard but looks
    /// at rest (a hairline, no caret) until something is typed in it. Build it inside a panel or
    /// `modal`.
    pub fn picker(&mut self, o: &PickerOpts, label: &str, query: &str, rows_of: &dyn Fn(&str) -> Vec<(String, String)>) -> (String, Pick) {
        let off = |i: i64| o.off.as_ref().map(|f| f(i)).unwrap_or(false);
        let mut rows = rows_of(query);
        // the search row: a 20-point field in 4 points of padding, a [/] before the query
        let search = self.box_(label, BoxOpts::default().flags(CLICKABLE | FOCUSABLE | BLOCKING | FOCUS_MARK).w(Size::Grow).h(Size::Px(28.0)));
        if self.focus_key() == 0 || self.slot_rw(search.box_slot) == 0.0 {
            self.focus_box(&search);
        }
        let list = self.box_keyed(int_key(search.box_key, 0), BoxOpts::default().w(Size::Grow).h(Size::Fit).axis(Axis::Column));
        let search_signal = self.signal(&search);
        if search_signal.hovered {
            self.request_cursor(Cursor::Text);
        }
        let keys = self.key_events(&search);
        let clamp = |cursor: i64, count: usize| if count == 0 { 0 } else { 0.max((count as i64 - 1).min(cursor)) };
        let mut cursor = clamp(self.state(&search, 0), rows.len());
        let mut armed = self.state(&list, -1);
        let mut query = query.to_owned();
        let mut result = Pick::None;
        let mut edit = self.load_text_edit(search.box_key, &query);
        let (sx, _, sw, _) = ints(self.rect(&search));
        let prefix: i64 = if o.slash { 13 } else { 0 };
        // the slash is 6.5 wide and 6 of gap: the query starts 14.5 in from the field's edge
        let inset = if o.slash { 14.5 - prefix as f64 } else { 2.0 };
        let shift = self.press_shift(&search);
        self.point_text_caret(None, &mut edit, &search_signal, shift, (sx + 4 + prefix) as f64 + inset, (sx + sw - 4 - 2) as f64);
        for (event, modifiers) in keys {
            let count = rows.len() as i64;
            if result == Pick::None {
                match &event {
                    Event::Key_pressed(Key::Backspace) if edit.text.is_empty() => result = Pick::Back,
                    Event::Key_pressed(Key::ArrowLeft) if edit.text.is_empty() => result = Pick::Back,
                    Event::Key_pressed(Key::ArrowDown) if count > 0 => {
                        cursor = (cursor + 1) % count;
                        armed = -1;
                    }
                    Event::Key_pressed(Key::ArrowUp) if count > 0 => {
                        cursor = (cursor + count - 1) % count;
                        armed = -1;
                    }
                    Event::Key_pressed(Key::Enter) => {
                        result = if count > 0 {
                            if off(cursor) { Pick::None } else { Pick::Pick(cursor) }
                        } else {
                            Pick::Submit
                        };
                    }
                    Event::Key_pressed(Key::Delete) if count > 0 && edit.caret == edit.anchor => {
                        if armed == cursor {
                            result = Pick::Delete(cursor);
                            armed = -1;
                        } else {
                            armed = cursor;
                        }
                    }
                    Event::Key_pressed(Key::Escape) => result = Pick::Cancel,
                    event => {
                        self.edit_text_event(false, &mut edit, &|_| true, &modifiers, event);
                        if edit.text != query {
                            query = edit.text.clone();
                            rows = rows_of(&query);
                            cursor = 0;
                            armed = -1;
                        }
                    }
                }
            }
        }
        self.save_text_edit(&edit);
        let count = rows.len() as i64;
        let length = o.limit.min(count);
        let start = if length == count { 0 } else { 0.max((count - length).min(cursor - length / 2)) };
        let theme = self.theme();
        let composition = self.composition().to_owned();
        let placeholder = display(label);
        let font_size = self.font_size();
        let (slash, at_rest) = (o.slash, o.at_rest);
        {
            let edit = edit.clone();
            self.draw(&search, move |paint, rect| {
                let (x, y, w, h) = ints(rect);
                let field = (x + 4, y + 4, 1.max(w - 8), 1.max(h - 8));
                let (cx, cy, cw, ch) = field;
                // a field that has not been typed in yet can sit at rest: the hairline and no caret
                let resting = at_rest && edit.text.is_empty();
                underline(paint, field, if resting { theme::edge(&theme) } else { theme.accent });
                if slash {
                    kit_text(paint, Some(theme::ink_2(&theme)), cx + 2, label_y(font_size, cy, ch), "/");
                }
                let control = (cx + prefix, cy, 1.max(cw - prefix), ch);
                if edit.text.is_empty() {
                    paint.text_c(((cx + prefix) as f64 + inset, label_y(font_size, cy, ch) as f64), theme::ink_3(&theme), &placeholder);
                }
                paint_text_edit(paint, None, false, inset, !edit.text.is_empty(), control, label_y(font_size, cy, ch), &composition, &edit);
            });
        }
        let saved = self.enter(&list);
        let row_height = self.row_height();
        for visible in 0..length {
            let index = start + visible;
            // a row that cannot be picked leaves the keyboard with the search field
            let row = self.box_keyed(
                int_key(list.box_key, visible),
                BoxOpts::default()
                    .flags(CLICKABLE | FOCUSABLE | BLOCKING | if off(index) { KEEP_FOCUS } else { 0 })
                    .w(Size::Grow)
                    .h(Size::Px(row_height as f64)),
            );
            let row_signal = self.signal(&row);
            if result == Pick::None && row_signal.clicked && !off(index) {
                result = Pick::Pick(index);
            }
            let current = index == cursor;
            let hovered = row_signal.hovered;
            let doomed = index == armed;
            let (text, detail) = rows[index as usize].clone();
            let swatch = o.mark.as_ref().and_then(|m| m(index));
            let has_mark = o.mark.is_some();
            let unavailable = off(index);
            let query = query.clone();
            self.draw(&row, move |paint, rect| {
                let bounds = ints(rect);
                let (x, y, w, h) = bounds;
                // the keyboard cursor is the control fill; a row armed for deletion reads in the error colour
                if doomed {
                    fill_i(paint, bounds, theme::pressed_fill(&theme));
                } else if current {
                    fill_i(paint, bounds, theme.control);
                } else if hovered {
                    hover_row(paint, &theme, bounds);
                }
                let text_x = if !has_mark { x + SIDE } else { x + SIDE + 16 };
                // a label that would run into the detail is cut with an ellipsis
                let key_size = 8.max(paint.font_size - 2);
                let key_text = "\u{21b5}";
                let right = if current {
                    (x + w - SIDE) as f64 - paint.text_width(Some(key_size), key_text) - 8.0
                } else {
                    (x + w - SIDE) as f64
                };
                let limit = right - paint.text_width(None, &detail) - 12.0 - text_x as f64;
                let text = ellipsis(&mut |s| paint.text_width(None, s), limit, &text);
                if let Some(color) = swatch {
                    fill_i(paint, (x + SIDE, y + (h - 8) / 2, 8, 8), if unavailable { theme::ink_3(&theme) } else { color });
                }
                // the query's letters, matched in order, in the accent
                let q = query.to_ascii_lowercase();
                let lower = text.to_ascii_lowercase();
                let mut matched = vec![false; text.len()];
                let mut next = 0usize;
                for (at, c) in text.char_indices() {
                    if next < q.len() && lower.len() > at && lower.as_bytes()[at] == q.as_bytes()[next] {
                        matched[at] = true;
                        next += 1;
                    }
                    let _ = c;
                }
                let mut pen = text_x as f64;
                let mut start = 0usize;
                while start < text.len() {
                    let flag = matched[start];
                    let mut stop = start;
                    while stop < text.len() && (stop == start || matched[stop] == flag || text.as_bytes()[stop] & 0xc0 == 0x80) {
                        stop += 1;
                    }
                    let run = &text[start..stop];
                    paint.text_c(
                        (pen, label_y(paint.font_size, y, h) as f64),
                        if doomed {
                            theme::invalid()
                        } else if unavailable {
                            theme::ink_3(&theme)
                        } else if flag {
                            theme.accent
                        } else {
                            theme.foreground
                        },
                        run,
                    );
                    pen += paint.text_width(None, run);
                    start = stop;
                }
                let right = if current {
                    let width = paint.text_width(Some(key_size), key_text);
                    let ty = paint.text_top(Some(key_size), y as f64, h as f64);
                    paint.text_s(((x + w - SIDE) as f64 - width, ty), key_size, theme::ink_3(&theme), key_text);
                    (x + w - SIDE) as f64 - width - 8.0
                } else {
                    (x + w - SIDE) as f64
                };
                let dw = paint.text_width(None, &detail);
                paint.text_c(
                    (right - dw, label_y(paint.font_size, y, h) as f64),
                    if unavailable { theme::ink_3(&theme) } else { theme::muted(&theme) },
                    &detail,
                );
            });
        }
        self.leave(saved);
        self.set_state(&search, cursor);
        self.set_state(&list, armed);
        (query, result)
    }
}

// ------------------------------------------------------------------ context menu

/// A submenu of a `context_menu`.
#[derive(Clone, Debug, PartialEq)]
pub struct Submenu {
    /// the row of the menu that opens it (drawn with a chevron)
    pub row: i64,
    pub rows: Vec<(String, bool)>,
    pub keys: Vec<String>,
    /// the row marked with the accent square
    pub current: Option<i64>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MenuResult {
    Open,
    Pick(i64),
    Dismiss,
}

#[derive(Clone, Default)]
pub struct MenuOpts {
    pub width: Option<f64>,
    pub selected: Option<i64>,
    pub swatches: Vec<Option<Color>>,
    pub keys: Vec<String>,
    pub danger: Vec<i64>,
    pub submenus: Vec<Submenu>,
    pub lead_from: i64,
}

struct MenuRows {
    items: Vec<(String, bool)>,
    keys: Vec<String>,
    selected: Option<i64>,
    swatches: Vec<Option<Color>>,
    danger: Vec<i64>,
    opened: Option<i64>,
    chevrons: Vec<i64>,
    lead_from: i64,
    swatch_pad: f64,
    key_size: i64,
    gap: f64,
}

impl Ui {
    /// the rows of one menu: the row picked, and the row the pointer is on
    fn menu_rows(&mut self, m: &MenuRows) -> (Option<i64>, Option<i64>) {
        let mut hovered = None;
        let mut picked = None;
        let theme = self.theme();
        let font_size = self.font_size();
        let lead_of = |index: i64| if m.selected.is_none() || index < m.lead_from { 0 } else { 14 };
        for (index, (text, enabled)) in m.items.iter().enumerate() {
            let index = index as i64;
            if text.is_empty() {
                let line = self.box_(&format!("separator-{}", index), BoxOpts::default().flags(BLOCKING).w(Size::Grow).h(Size::Px(m.gap)));
                self.draw(&line, move |paint, (x, y, w, _)| paint.fill(x, y + 4.0, w, 1.0, theme::edge(&theme)));
            } else {
                let enabled = *enabled;
                let row = self.kit_row(Some(if enabled { CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY } else { BLOCKING }), None, text);
                let signal = self.signal(&row);
                if enabled && signal.hovered {
                    hovered = Some(index);
                }
                let shown = display(text);
                let chevron = m.chevrons.contains(&index);
                let opened = m.opened;
                let selected = m.selected;
                let swatch = m.swatches.get(index as usize).copied().flatten();
                let danger = m.danger.contains(&index);
                let swatch_pad = m.swatch_pad;
                let key_size = m.key_size;
                let key = m.keys.get(index as usize).filter(|k| !k.is_empty()).cloned();
                let lead = lead_of(index);
                self.draw(&row, move |paint, rect| {
                    let bounds = ints(rect);
                    let (rx, y, w, h) = bounds;
                    if enabled && (signal.hovered || opened == Some(index)) {
                        fill_i(paint, bounds, theme.control);
                    }
                    // the current choice: a 6-point accent square before the label
                    if selected == Some(index) {
                        paint.fill(rx as f64 + SIDE as f64, y as f64 + h as f64 / 2.0 - 3.0, 6.0, 6.0, theme.accent);
                    }
                    if let Some(color) = swatch {
                        let sx = (rx + SIDE + lead) as f64;
                        let sy = y as f64 + h as f64 / 2.0 - 4.0;
                        paint.fill(sx, sy, 8.0, 8.0, color);
                        paint.frame(sx, sy, 8.0, 8.0, theme::edge(&theme));
                    }
                    // the destructive row reads in the error ink
                    kit_text(
                        paint,
                        Some(if !enabled {
                            theme::ink_3(&theme)
                        } else if danger {
                            theme::invalid()
                        } else {
                            theme.foreground
                        }),
                        rx + SIDE + lead + swatch_pad as i64,
                        label_y(font_size, y, h),
                        &shown,
                    );
                    if chevron {
                        paint.chevron(((rx + w) as f64 - 15.25, y as f64 + h as f64 / 2.0), Dir::Right, theme::ink_2(&theme));
                    }
                    if let Some(key) = &key {
                        let kw = paint.text_width(Some(key_size), key);
                        let ty = paint.text_top(Some(key_size), y as f64, h as f64);
                        paint.text_s(((rx + w - SIDE) as f64 - kw, ty), key_size, theme::ink_2(&theme), key);
                    }
                });
                if enabled && signal.clicked && !chevron && picked.is_none() {
                    picked = Some(index);
                }
            }
        }
        (picked, hovered)
    }

    fn menu_measure(&mut self, lead: f64, keys: &[String], chevrons: &[i64], base: f64, items: &[(String, bool)], swatch_pad: f64, key_size: i64) -> f64 {
        // the 1-point edge, 12 points of padding, then the label, 24 points and the key or chevron
        let mut w = base;
        for (index, (text, _)) in items.iter().enumerate() {
            let extra = match keys.get(index).filter(|k| !k.is_empty()) {
                Some(key) => 24.0 + self.text_width(Some(key_size), key),
                None => if chevrons.contains(&(index as i64)) { 32.5 } else { 0.0 },
            };
            w = w.max(self.text_width(None, text) + 26.0 + lead + swatch_pad + extra);
        }
        w
    }

    /// A floating menu at `at` with `(label, enabled)` rows, as wide as its longest row and at
    /// least `width` wide, capped to the frame. An empty label is a separator line (never picked).
    /// `selected` marks the current choice; `swatches` gives a row a small colour square before its
    /// label. The host keeps it open while this returns `Open`; a row commits on press and release
    /// inside it, and Escape, focus loss, or a press outside return `Dismiss`. Build it before
    /// content it shields; it floats at the root regardless of its parent. `keys` are the shortcuts
    /// in ink-2 at the right; `danger` rows (a delete) read in the error ink. A row named by a
    /// `Submenu` has a chevron and, while the pointer is on it, opens a second menu overlapping the
    /// first by a point, level with the row; the rows of the submenus are numbered after those of
    /// the menu, in the order the submenus are given. With `selected`, the rows from `lead_from`
    /// (default 0) on keep a slot for the accent square before their label. A press outside the
    /// menu in the frame that first builds it dismisses it.
    pub fn context_menu(&mut self, at: (f64, f64), o: &MenuOpts, label: &str, items: &[(String, bool)]) -> MenuResult {
        let (x, y) = at;
        let submenu_key = key_of(self.current_seed(), &format!("{}-sub", label));
        // the width follows the longest row; an empty label is a separator line; `keys` are the
        // shortcuts at the right of the rows, in ink-2 at the label size
        let row_height = self.row_height() as f64;
        let gap = 9.0;
        let key_size = 8.max(self.font_size() - 2);
        let swatch_pad = if o.swatches.iter().any(|s| s.is_some()) { 14.0 } else { 0.0 };
        let lead = if o.selected.is_none() { 0.0 } else { 14.0 };
        let (view_w, view_h) = self.view_size();
        let chevrons: Vec<i64> = o.submenus.iter().map(|s| s.row).collect();
        let width = view_w.min(self.menu_measure(lead, &o.keys, &chevrons, o.width.unwrap_or(150.0), items, swatch_pad, key_size));
        let height_of = |items: &[(String, bool)]| items.iter().fold(14.0, |h, (text, _)| h + if text.is_empty() { gap } else { row_height });
        let height = height_of(items);
        let x = 0.0f64.max(x.min(view_w - width));
        let y = 0.0f64.max(y.min(view_h - height));
        // the submenu of a row: a second menu overlapping this one by a point, level with its row
        let row_top = |index: i64| {
            y + 7.0 + items.iter().take(0.max(index) as usize).fold(0.0, |top, (text, _)| top + if text.is_empty() { gap } else { row_height })
        };
        let mut sub_geometry = |ui: &mut Ui, sub: &Submenu| -> Rect {
            let sub_width = view_w.min(ui.menu_measure(if sub.current.is_none() { 0.0 } else { 14.0 }, &sub.keys, &[], 0.0, &sub.rows, swatch_pad, key_size));
            let sub_height = height_of(&sub.rows);
            let sx = if x + width - 1.0 + sub_width <= view_w { x + width - 1.0 } else { x - sub_width + 1.0 };
            let sy = 0.0f64.max((row_top(sub.row) - 7.0).min(view_h - sub_height));
            (sx, sy, sub_width, sub_height)
        };
        // the rows of the submenus are numbered after those of this menu, in order
        let mut bases = Vec::new();
        let mut n = items.len() as i64;
        for sub in &o.submenus {
            bases.push(n);
            n += sub.rows.len() as i64;
        }
        let keep: Vec<Rect> = o.submenus.iter().map(|s| sub_geometry(self, s)).collect();
        let theme = self.theme();
        let stroke = theme::edge(&theme);
        let related = if o.submenus.is_empty() { Vec::new() } else { vec![submenu_key] };
        let popts = PopupOpts { stroke: Some(stroke), keep, related, ..PopupOpts::default() };
        let mut opened: Option<i64> = None;
        let mut picked: Option<i64> = None;
        let menu_items = items.to_vec();
        let shown = self.popup_with_related(popts, (x, y), width, height, label, &mut |ui: &mut Ui| {
            let top = ui.box_("menu-top", BoxOpts::default().flags(BLOCKING).w(Size::Grow).h(Size::Px(6.0)));
            let open_row = ui.state(&top, -1);
            opened = if open_row >= 0 { Some(open_row) } else { None };
            let m = MenuRows {
                items: menu_items.clone(),
                keys: o.keys.clone(),
                selected: o.selected,
                swatches: o.swatches.clone(),
                danger: o.danger.clone(),
                opened,
                chevrons: chevrons.clone(),
                lead_from: o.lead_from,
                swatch_pad,
                key_size,
                gap,
            };
            let (p, hovered) = ui.menu_rows(&m);
            picked = p;
            // the pointer opens the submenu of a row, or closes it on any other
            if let Some(index) = hovered {
                let now = if o.submenus.iter().any(|s| s.row == index) { index } else { -1 };
                if now != open_row {
                    ui.set_state(&top, now);
                }
                opened = if now >= 0 { Some(now) } else { None };
            }
            ui.box_("menu-bottom", BoxOpts::default().flags(BLOCKING).w(Size::Grow).h(Size::Px(6.0)));
        });
        if !shown {
            self.dismiss_popup();
            return MenuResult::Dismiss;
        }
        if let Some(index) = picked {
            self.dismiss_popup();
            return MenuResult::Pick(index);
        }
        let sub = opened.and_then(|row| o.submenus.iter().position(|s| s.row == row));
        match sub {
            None => MenuResult::Open,
            Some(si) => {
                let sub = o.submenus[si].clone();
                let (sx, sy, sw, sh) = sub_geometry(self, &sub);
                let mut sub_picked: Option<i64> = None;
                self.popup(Some(stroke), None, true, true, (sx, sy), sw, sh, &format!("{}-sub", label), &mut |ui: &mut Ui| {
                    ui.box_("menu-top", BoxOpts::default().flags(BLOCKING).w(Size::Grow).h(Size::Px(6.0)));
                    let m = MenuRows {
                        items: sub.rows.clone(),
                        keys: sub.keys.clone(),
                        selected: sub.current,
                        swatches: Vec::new(),
                        danger: Vec::new(),
                        opened: None,
                        chevrons: Vec::new(),
                        lead_from: 0,
                        swatch_pad,
                        key_size,
                        gap,
                    };
                    let (p, _) = ui.menu_rows(&m);
                    sub_picked = p;
                    ui.box_("menu-bottom", BoxOpts::default().flags(BLOCKING).w(Size::Grow).h(Size::Px(6.0)));
                });
                match sub_picked {
                    Some(index) => {
                        self.dismiss_popup();
                        MenuResult::Pick(bases[si] + index)
                    }
                    None => MenuResult::Open,
                }
            }
        }
    }
}
