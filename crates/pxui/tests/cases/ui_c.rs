//! Text-area editing, completion, scrubbing and rewrite contracts.
use crate::support::*;
use crate::ui_b::*;
use input::{Event, Key, MouseButton, Platform};
use pxui::ui::*;
use rays::scene::{self, layer};
use pxui::geom::color;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

fn plain_language() -> Language {
    Language {
        colorize: Rc::new(|_| Vec::new()),
        brackets: Rc::new(|_| Vec::new()),
        indent: Rc::new(|text, caret| if text[..caret as usize].contains('(') { "  ".to_owned() } else { String::new() }),
        pairs: vec![('(', ')'), ('"', '"')],
        complete: Rc::new(|_, _| Vec::new()),
        describe: Rc::new(|_, _| None),
        number_at: Rc::new(|_, _| None),
        context_at: Rc::new(|text, caret| {
            // an odd number of quotes before the caret: inside a string
            let quotes = text.bytes().take(caret as usize).filter(|c| *c == b'"').count();
            if quotes % 2 == 1 { CodeContext::String } else { CodeContext::Code }
        }),
        comment: ";".to_owned(),
        rewrite: None,
    }
}

pub fn text_area_2(rig: &mut Rig) {
    // a language: brackets in pairs, Enter indents, right-click reports the point
    rig.errors = vec![];
    rig.language = Some(plain_language());
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12)), Event::TextInput("(".into())]);
    rig.expect("an opener inserts its pair", "()");
    rig.plain(vec![Event::TextInput("a".into()), Event::TextInput(")".into())]);
    rig.expect("a closer typed before itself steps over it", "(a)");
    rig.plain(vec![key_down(Key::ArrowLeft), key_down(Key::Enter)]);
    rig.expect("Enter indents as the language says", "(a\n  )");
    rig.plain(vec![Event::TextInput("\"".into()), Event::TextInput("\"".into())]);
    rig.expect("a quote pairs, then steps over its twin", "(a\n  \"\")");
    rig.plain(vec![key_down(Key::ArrowLeft), key_down(Key::Backspace)]);
    rig.expect("Backspace between an empty pair takes both", "(a\n  )");
    rig.keyed(vec![Key::Shift], vec![key_down(Key::Home)]);
    rig.plain(vec![Event::TextInput("(".into())]);
    rig.expect("an opener wraps the selection", "(a\n(  ))");
    rig.plain(vec![
        Event::PointerPressed(MouseButton::RightButton, (100.0, 20.0)),
        Event::PointerReleased(MouseButton::RightButton, (100.0, 20.0)),
    ]);
    if rig.context.get().is_none() {
        fail("a right-click did not reach on_context");
    }
}

fn pairs_of(text: &str) -> Vec<(i64, i64)> {
    let mut stack: Vec<i64> = Vec::new();
    let mut out = Vec::new();
    for (i, c) in text.bytes().enumerate() {
        if c == b'(' {
            stack.push(i as i64);
        } else if c == b')' {
            if let Some(o) = stack.pop() {
                out.push((o, i as i64));
            }
        }
    }
    out.sort();
    out.sort_by_key(|(_, c)| *c);
    out
}

pub fn text_area_3(rig: &mut Rig) {
    // completions: typing opens the popup, Down moves, Tab accepts; Escape closes it before it
    // leaves the editor
    let digit = |c: u8| c.is_ascii_digit() || c == b'.';
    let base = rig.language.clone().unwrap();
    rig.language = Some(Language {
        complete: Rc::new(|text, caret| {
            let caret = caret as usize;
            let b = text.as_bytes();
            let mut i = caret;
            while i > 0 && b[i - 1] != b' ' && b[i - 1] != b'(' {
                i -= 1;
            }
            let start = i;
            let prefix = &text[start..caret];
            if prefix.is_empty() {
                return Vec::new();
            }
            ["zebra", "zephyr", "zoo"]
                .iter()
                .filter(|w| w.starts_with(prefix))
                .map(|w| Completion {
                    replace: (start as i64, caret as i64),
                    insert: w.to_string(),
                    label: w.to_string(),
                    detail: "word".to_owned(),
                    doc: w.to_string(),
                })
                .collect()
        }),
        number_at: Rc::new(move |text, byte| {
            let b = text.as_bytes();
            let byte = byte as usize;
            if byte < b.len() && digit(b[byte]) {
                let mut a = byte;
                while a > 0 && digit(b[a - 1]) {
                    a -= 1;
                }
                let mut e = byte;
                while e < b.len() && digit(b[e]) {
                    e += 1;
                }
                Some((a as i64, e as i64))
            } else {
                None
            }
        }),
        ..base
    });
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12)), Event::TextInput("z".into()), Event::TextInput("e".into())]);
    rig.expect("typing opens the popup without changing the text", "ze");
    rig.plain(vec![key_down(Key::ArrowDown)]);
    rig.expect("Down moves in the popup, not in the text", "ze");
    rig.plain(vec![key_down(Key::Tab)]);
    rig.expect("Tab accepts the chosen completion", "zephyr");
    rig.plain(vec![Event::TextInput(" ".into()), Event::TextInput("z".into())]);
    rig.plain(vec![key_down(Key::Escape)]);
    if !rig.ui.text_input_focused() {
        fail("Escape closed the editor instead of the popup");
    }
    rig.plain(vec![key_down(Key::Escape)]);
    if rig.ui.text_input_focused() {
        fail("a second Escape did not leave the editor");
    }
    // Enter breaks the line unless Up or Down chose a row; Tab always accepts
    let saved = rig.language.clone();
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12)), Event::TextInput("z".into()), Event::TextInput("e".into())]);
    rig.plain(vec![key_down(Key::Enter)]);
    rig.expect("Enter after a typed word breaks the line, it does not take the popup", "ze\n");
    rig.plain(vec![Event::TextInput(" ".into()), Event::TextInput("z".into()), Event::TextInput("e".into())]);
    rig.plain(vec![key_down(Key::ArrowDown)]);
    rig.plain(vec![key_down(Key::Enter)]);
    rig.expect("Enter takes the row an arrow chose", "ze\n zephyr");
    rig.plain(vec![key_down(Key::Escape), key_down(Key::Escape)]);
    // Tab and Shift-Tab with a selection indent and dedent every line it touches
    let mut l = rig.language.clone().unwrap();
    l.complete = Rc::new(|_, _| Vec::new());
    rig.language = Some(l);
    rig.area = "a\n\nb\nc".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('a'))]);
    rig.plain(vec![key_down(Key::Tab)]);
    rig.expect("Tab with a selection indents every line and keeps the selection", "  a\n\n  b\n  c");
    let sel = rig.selected.get();
    if sel.0 != 0 || sel.1 != rig.area.len() as i64 {
        fail("the indented lines lost their selection");
    }
    rig.keyed(vec![Key::Shift], vec![key_down(Key::Tab)]);
    rig.expect("Shift-Tab with a selection dedents every line", "a\n\nb\nc");
    rig.plain(vec![key_down(Key::ArrowDown)]);
    rig.plain(vec![key_down(Key::Tab)]);
    rig.expect("Tab without a selection still inserts two spaces", "a\n\nb\nc  ");
    // brackets and quotes pair and step over only in code
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12)), Event::TextInput("\"".into()), Event::TextInput("(".into())]);
    rig.expect("a bracket inside a string is typed as it is", "\"(\"");
    rig.plain(vec![Event::TextInput("\"".into())]);
    rig.expect("a quote before the closing quote steps over it", "\"(\"");
    rig.area = "\"x\"".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowUp)]);
    rig.plain(vec![Event::TextInput("\"".into())]);
    rig.expect("a quote before a string's opening quote does not step over it", "\"\"\"x\"");
    // Up and Down keep their column across a short line
    rig.area = "abcdef\nab\nabcdef".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowUp)]);
    rig.plain((0..5).map(|_| key_down(Key::ArrowRight)).collect());
    rig.plain(vec![key_down(Key::ArrowDown)]);
    rig.plain(vec![key_down(Key::ArrowDown)]);
    rig.plain(vec![Event::TextInput("X".into())]);
    rig.expect("Down across a short line returns to the column it left", "abcdef\nab\nabcdeXf");
    // `select` takes the keyboard and places the selection; `splice` is one undo step
    rig.language = None;
    rig.area = "one two one".to_owned();
    rig.ui.unfocus();
    rig.step(AreaStep { select: Some((4, 7)), ..AreaStep::default() }, vec![]);
    if !rig.ui.text_input_focused() {
        fail("a select request did not focus the area");
    }
    rig.plain(vec![Event::TextInput("2".into())]);
    rig.expect("typing replaces the requested selection", "one 2 one");
    rig.area = "one two one".to_owned();
    rig.step(
        AreaStep { splice: vec![(0, 3, "1".into()), (8, 11, "111".into())], select: Some((1, 1)), ..AreaStep::default() },
        vec![],
    );
    rig.expect("a splice replaces every range", "1 two 111");
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.expect("Command-Z takes the whole splice back", "one two one");
    // `reveal` fires again for a new nonce, not for the same one after the user scrolled away
    rig.area = (0..40).map(|i| i.to_string()).collect::<Vec<_>>().join("\n");
    fn visible(rig: &Rig) -> bool {
        let y = rig.region().0;
        y >= 0 && y < 96
    }
    fn back_to_end(rig: &mut Rig, reveal: Option<i64>, nonce: i64) {
        rig.step(AreaStep { keys: vec![Key::Meta], reveal, reveal_nonce: nonce, ..AreaStep::default() }, vec![key_down(Key::ArrowUp)]);
        rig.step(AreaStep { keys: vec![Key::Meta], reveal, reveal_nonce: nonce, ..AreaStep::default() }, vec![key_down(Key::ArrowDown)]);
    }
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    back_to_end(rig, None, 0);
    let off = (0..30).map(|i| i.to_string()).collect::<Vec<_>>().join("\n").len() as i64;
    if !visible(rig) {
        fail("the caret at the end was not in view");
    }
    rig.step(AreaStep { reveal: Some(off), ..AreaStep::default() }, vec![]);
    if visible(rig) {
        fail("reveal did not scroll the offset into view");
    }
    back_to_end(rig, Some(off), 0);
    rig.step(AreaStep { reveal: Some(off), ..AreaStep::default() }, vec![]);
    if !visible(rig) {
        fail("reveal fired twice for one request");
    }
    rig.step(AreaStep { reveal: Some(off), reveal_nonce: 1, ..AreaStep::default() }, vec![]);
    if visible(rig) {
        fail("reveal did not fire again for a new nonce");
    }
    back_to_end(rig, Some(off), 1);
    rig.plain(vec![]);
    if !visible(rig) {
        fail("the caret did not return");
    }
    rig.step(AreaStep { reveal: Some(off), reveal_nonce: 1, ..AreaStep::default() }, vec![]);
    if visible(rig) {
        fail("reveal did not fire again after an absent reveal");
    }
    // line and form edits: comment toggle, move and duplicate lines, enclosing form, bracket jump
    let mut l = saved.clone().unwrap();
    l.brackets = Rc::new(|t| pairs_of(t));
    l.complete = Rc::new(|_, _| Vec::new());
    rig.language = Some(l);
    let alt = vec![Key::Alt];
    let alt_shift = vec![Key::Alt, Key::Shift];
    rig.area = "a\n\n  b\nc".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('a'))]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('/'))]);
    rig.expect("Command-/ comments every non-blank line at the smallest indent", "; a\n\n;   b\n; c");
    rig.keyed(vec![Key::Meta], vec![key_down(kc('/'))]);
    rig.expect("Command-/ again takes the comments off", "a\n\n  b\nc");
    rig.area = "x\n y".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowUp)]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('/'))]);
    rig.plain(vec![Event::TextInput("Q".into())]);
    rig.expect("the caret keeps its place in a commented line", "; Qx\n y");
    rig.area = "one\ntwo\nthree".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowUp)]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowDown)]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowUp)]);
    rig.plain(vec![key_down(Key::End)]);
    rig.keyed(alt.clone(), vec![key_down(Key::ArrowUp)]);
    rig.expect("Option-Up on the first line does nothing", "one\ntwo\nthree");
    rig.keyed(alt.clone(), vec![key_down(Key::ArrowDown)]);
    rig.expect("Option-Down moves the line down", "two\none\nthree");
    rig.plain(vec![Event::TextInput("X".into())]);
    rig.expect("the caret travels with the moved line", "two\noneX\nthree");
    rig.keyed(alt.clone(), vec![key_down(Key::ArrowDown)]);
    rig.keyed(alt.clone(), vec![key_down(Key::ArrowDown)]);
    rig.expect("Option-Down on the last line does nothing", "two\nthree\noneX");
    rig.keyed(alt.clone(), vec![key_down(Key::ArrowUp)]);
    rig.expect("Option-Up moves the line up", "two\noneX\nthree");
    rig.area = "ab".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowDown)]);
    rig.keyed(alt_shift.clone(), vec![key_down(Key::ArrowDown)]);
    rig.plain(vec![Event::TextInput("Z".into())]);
    rig.expect("Shift-Option-Down copies the line and keeps the caret on the copy", "ab\nabZ");
    rig.area = "ab".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowDown)]);
    rig.keyed(alt_shift.clone(), vec![key_down(Key::ArrowUp)]);
    rig.plain(vec![Event::TextInput("Z".into())]);
    rig.expect("Shift-Option-Up copies the line above and keeps the caret on the original", "abZ\nab");
    rig.area = "(a (b c) d)".to_owned();
    rig.plain(vec![]);
    rig.plain(vec![key_down(Key::Home)]);
    rig.plain((0..5).map(|_| key_down(Key::ArrowRight)).collect());
    rig.keyed(vec![Key::Meta, Key::Alt], vec![key_down(Key::ArrowUp)]);
    if rig.selected.get() != (3, 8) {
        fail("Command-Option-Up did not select the innermost form");
    }
    rig.keyed(vec![Key::Meta, Key::Alt], vec![key_down(Key::ArrowUp)]);
    if rig.selected.get() != (0, 11) {
        fail("a second Command-Option-Up did not widen to the next form");
    }
    rig.plain(vec![key_down(Key::Home)]);
    rig.plain((0..5).map(|_| key_down(Key::ArrowRight)).collect());
    rig.keyed(vec![Key::Meta, Key::Shift], vec![key_down(kc('\\'))]);
    if rig.selected.get() != (3, 3) {
        let s = rig.selected.get();
        fail(&format!("the bracket jump did not go to the opener of the pair around the caret {} {}", s.0, s.1));
    }
    rig.keyed(vec![Key::Meta, Key::Shift], vec![key_down(kc('\\'))]);
    if rig.selected.get() != (8, 8) {
        fail("the bracket jump did not cross to the far side of the pair");
    }
    // a text that comes back unchanged takes its own undo steps back
    rig.area = "foo".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.plain(vec![Event::TextInput("x".into())]);
    rig.expect("typing", "foox");
    rig.area = "other".to_owned();
    rig.plain(vec![]);
    rig.area = "foox".to_owned();
    rig.plain(vec![]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.expect("Command-Z after the text returned undoes the typing", "foo");
    rig.language = saved;
}

pub fn text_area_4(rig: &mut Rig) {
    // a number dragged sideways follows the pointer: an integer one per six points, a float a
    // tenth of its last place per point; the host hears Live frames and one Done
    for dx in [-20.0, -6.0, -2.0, 0.0, 2.0, 6.0, 20.0] {
        assert_eq!(scrubbed("10", dx, false), param::number::scrub(param::number::Kind::Int, None, "10", f64::from(dx), false));
    }
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    let origin = rig.region().1;
    rig.area = "10 x".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    let char_w = (rig.region().1 - origin) / 4;
    if char_w <= 0 {
        fail("no glyph width to aim a drag with");
    }
    let nx = origin + char_w / 2;
    rig.scrubs.borrow_mut().clear();
    rig.plain(vec![press((nx, 18))]);
    rig.plain(vec![move_to((nx + 10, 18))]);
    rig.plain(vec![move_to((nx + 20, 18))]);
    rig.expect("a dragged integer moves one per six points", "13 x");
    if rig.scrub_edits.borrow()[0] != ((0, 2), "13".to_owned()) {
        fail("a numeric drag lost its pre-edit token span");
    }
    if !rig.scrubs.borrow().contains(&ScrubPhase::Live) {
        fail("a drag did not report on_scrub Live");
    }
    rig.plain(vec![release((nx + 20, 18))]);
    if rig.scrubs.borrow()[0] != ScrubPhase::Done {
        fail("a drag's end did not report on_scrub Done");
    }
    rig.expect("the release keeps the value", "13 x");
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.expect("Command-Z takes the whole drag back", "10 x");
    rig.area = "0.5 y".to_owned();
    rig.plain(vec![]);
    rig.plain(vec![press((nx, 18))]);
    rig.plain(vec![move_to((nx + 10, 18))]);
    rig.expect("a dragged float moves a tenth of its last place per point", "0.6 y");
    rig.plain(vec![release((nx + 10, 18))]);
    rig.plain(vec![press((nx, 18))]);
    rig.plain(vec![move_to((nx, 32))]);
    rig.plain(vec![release((nx, 32))]);
    rig.expect("a vertical drag is a selection, not a scrub", "0.6 y");
    // only the number's underline (the low part of its row) is a handle: a sideways drag that
    // starts higher selects
    rig.plain(vec![press((nx, 8))]);
    rig.plain(vec![move_to((nx + 20, 8))]);
    rig.plain(vec![release((nx + 20, 8))]);
    rig.expect("a sideways drag from above the underline selects, it does not scrub", "0.6 y");
    // the language's rewrite runs after each edited frame and maps the caret
    let mut l = rig.language.clone().unwrap_or_else(plain_language);
    l.rewrite = Some(Rc::new(|text, caret| (format!("{}!", text), caret)));
    rig.language = Some(l);
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12)), Event::TextInput("a".into())]);
    rig.expect("the rewrite keeps the text as the language wants it", "a!");
    rig.plain(vec![Event::TextInput("b".into())]);
    rig.expect("the caret stays where the rewrite put it", "ab!!");
    rig.language = None;
}

/// Windows conventions: Ctrl moves and deletes by words, Home and End are the lines, Ctrl-Z,
/// Ctrl-Y and Ctrl-Shift-Z undo and redo
pub fn text_area_windows(rig: &mut Rig) {
    rig.area = "one two three".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![Key::Ctrl], vec![key_down(Key::ArrowDown)]);
    rig.keyed(vec![Key::Ctrl], vec![key_down(Key::ArrowLeft)]);
    rig.plain(vec![Event::TextInput("X".into())]);
    rig.expect("Ctrl-Left moves to the start of the word", "one two Xthree");
    rig.plain(vec![key_down(Key::End)]);
    rig.keyed(vec![Key::Ctrl], vec![key_down(Key::Backspace)]);
    rig.expect("Ctrl-Backspace takes the word before the caret", "one two ");
    rig.keyed(vec![Key::Ctrl], vec![key_down(kc('z'))]);
    rig.expect("Ctrl-Z undoes the deletion", "one two Xthree");
    rig.keyed(vec![Key::Ctrl], vec![key_down(kc('y'))]);
    rig.expect("Ctrl-Y redoes it", "one two ");
    rig.keyed(vec![Key::Ctrl], vec![key_down(kc('z'))]);
    rig.keyed(vec![Key::Ctrl, Key::Shift], vec![key_down(kc('z'))]);
    rig.expect("Ctrl-Shift-Z redoes", "one two ");
    rig.plain(vec![key_down(Key::Home), Event::TextInput("_".into())]);
    rig.expect("Home is the start of the line", "_one two ");
    rig.plain(vec![key_down(Key::End), Event::TextInput("$".into())]);
    rig.expect("End is the end of the line", "_one two $");
}

pub fn text_area_5(rig: &mut Rig) {
    // macOS editing: a double click selects the word and a triple the line, Option and Command
    // keys move and delete by words and lines, Command-Z undoes inside the area
    rig.area = "alpha beta\ngamma delta".to_owned();
    // a click past the end of the first row lands after "beta"
    let at = (290, 12);
    rig.plain(vec![press(at), release(at), press(at), release(at)]);
    rig.plain(vec![Event::TextInput("X".into())]);
    rig.expect("a double click selects the word", "alpha X\ngamma delta");
    rig.plain(vec![press(at), release(at), press(at), release(at), press(at), release(at)]);
    rig.plain(vec![Event::TextInput("L".into())]);
    rig.expect("a triple click selects the line with its break", "Lgamma delta");
    rig.area = "one two three".to_owned();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.keyed(vec![Key::Meta], vec![key_down(Key::ArrowDown)]);
    rig.keyed(vec![Key::Alt], vec![key_down(Key::ArrowLeft)]);
    rig.plain(vec![Event::TextInput("X".into())]);
    rig.expect("Option-Left moves to the start of the word", "one two Xthree");
    rig.keyed(vec![Key::Alt], vec![key_down(Key::Backspace)]);
    rig.expect("Option-Backspace takes the word before the caret", "one two three");
    rig.keyed(vec![Key::Alt, Key::Shift], vec![key_down(Key::ArrowLeft)]);
    rig.plain(vec![Event::TextInput("2".into())]);
    rig.expect("Shift-Option-Left extends the selection by a word", "one 2three");
    rig.keyed(vec![Key::Meta], vec![key_down(Key::Backspace)]);
    rig.expect("Command-Backspace takes the line to the caret", "three");
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.expect("Command-Z undoes the last change", "one 2three");
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.expect("Command-Z steps back through the changes", "one two Xthree");
    rig.keyed(vec![Key::Meta, Key::Shift], vec![key_down(kc('z'))]);
    rig.expect("Shift-Command-Z redoes", "one two three");
    rig.area = String::new();
    rig.plain(vec![press((150, 12)), release((150, 12))]);
    rig.plain(vec![Event::TextInput("a".into())]);
    rig.plain(vec![Event::TextInput("b".into())]);
    rig.plain(vec![Event::TextInput("c".into())]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.expect("consecutive typing undoes as one step", "");
    rig.keyed(vec![Key::Meta], vec![key_down(kc('y'))]);
    rig.expect("Command-Y redoes the typing", "abc");
    if rig.ui.passed_undo().is_some() {
        fail("a redo the area took was passed on");
    }
    rig.keyed(vec![Key::Meta], vec![key_down(kc('y'))]);
    if rig.ui.passed_undo() != Some(UndoKind::Redo) {
        fail("a redo with an empty stack was not passed on");
    }
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    rig.keyed(vec![Key::Meta], vec![key_down(kc('z'))]);
    if rig.ui.passed_undo() != Some(UndoKind::Undo) {
        fail("an undo with an empty stack was not passed on");
    }
    rig.expect("the passed undo leaves the text alone", "");
    rig.plain(vec![]);
    if rig.ui.passed_undo().is_some() {
        fail("the passed undo outlived its frame");
    }
    // a row label cut short of its right-hand detail: whole when it fits, else an ellipsis within
    // the limit, never a split multi-byte character
    let cp = |s: &str| 7.0 * s.bytes().filter(|c| c & 0xC0 != 0x80).count() as f32;
    if ellipsis(&mut |s| cp(s), 200.0, "split side by side") != "split side by side" {
        fail("a fitting label was cut");
    }
    if ellipsis(&mut |s| cp(s), 70.0, "split side by side") != "split sid\u{2026}" {
        fail("a long label was not cut to the limit");
    }
    if ellipsis(&mut |s| cp(s), 28.0, "\u{192} petal") != "\u{192} p\u{2026}" {
        fail("the cut split a multi-byte character");
    }
    if ellipsis(&mut |s| cp(s), 7.0, "petal") != "\u{2026}" || !ellipsis(&mut |s| cp(s), 6.0, "petal").is_empty() {
        fail("a label with no room was not the ellipsis alone, then nothing");
    }
    // the cut measures what it shows, not the whole label
    let measured = Cell::new(0usize);
    let long = "a".repeat(4000);
    let cut = ellipsis(
        &mut |s| {
            measured.set(measured.get() + s.len());
            cp(s)
        },
        70.0,
        &long,
    );
    if cut != format!("{}\u{2026}", "a".repeat(9)) || measured.get() > 100 {
        fail("the cut of a long label measured all of it");
    }
}

pub fn window_pxui_text_area() {
    let mut rig = text_area_start(Platform::Apple);
    text_area_1(&mut rig);
    text_area_2(&mut rig);
    text_area_3(&mut rig);
    text_area_4(&mut rig);
    text_area_5(&mut rig);
    rig.ui.destroy();
}

pub fn window_pxui_text_area_windows() {
    let mut rig = text_area_start(Platform::Other);
    text_area_windows(&mut rig);
    rig.ui.destroy();
}

fn ui11() -> Ui {
    new_ui(Some(11))
}

pub fn window_pxui_modal_1() {
    // Modal: centered, and Escape or a press outside dismisses it.
    let mut ui = ui11();
    let shown = Cell::new(false);
    let modal = |ui: &mut Ui, events: Vec<Event>| {
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            shown.set(ui.modal(Some(200.0), "modal", &mut |ui| ui.label("Hello")));
        });
    };
    modal(&mut ui, vec![]);
    modal(&mut ui, vec![]);
    if !shown.get() {
        fail("modal did not build its content");
    }
    modal(&mut ui, vec![press((160, 120)), release((160, 120))]);
    if !shown.get() {
        fail("a press inside dismissed the modal");
    }
    modal(&mut ui, vec![key_down(Key::Escape)]);
    if shown.get() {
        fail("Escape did not dismiss the modal");
    }
    modal(&mut ui, vec![]);
    modal(&mut ui, vec![press((5, 5))]);
    if shown.get() {
        fail("a press outside did not dismiss the modal");
    }
    ui.destroy();
}

pub fn window_pxui_modal_2() {
    let mut ui = ui11();
    let shown = Cell::new(false);
    let popup = |ui: &mut Ui, events: Vec<Event>| {
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            shown.set(ui.popup(None, None, true, false, (20.0, 20.0), 200.0, 10.0, "popup", &mut |ui| {
                ui.label("One");
                ui.label("Two");
            }));
        });
    };
    popup(&mut ui, vec![]);
    popup(&mut ui, vec![]);
    popup(&mut ui, vec![press((50, 58))]);
    if !shown.get() {
        fail("popup dismissed a press inside its laid-out rect");
    }
    popup(&mut ui, vec![press((300, 200))]);
    if shown.get() {
        fail("popup kept a press outside its laid-out rect");
    }
    ui.destroy();
}

pub fn window_pxui_modal_3() {
    let mut ui = ui11();
    let menu = |ui: &mut Ui, events: Vec<Event>| -> MenuResult {
        let mut out = MenuResult::Open;
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            let o = MenuOpts { width: Some(240.0), selected: Some(1), ..MenuOpts::default() };
            out = ui.context_menu((20.0, 20.0), &o, "choices", &[("First".to_owned(), true), ("Second".to_owned(), true)]);
        });
        out
    };
    menu(&mut ui, vec![]);
    menu(&mut ui, vec![]);
    if menu(&mut ui, vec![press((240, 58)), release((240, 58))]) != MenuResult::Pick(1) {
        fail("dropdown rows did not extend to the requested control width");
    }
    ui.destroy();
}

pub fn window_pxui_modal_4() {
    let mut ui = ui11();
    let front_key = Cell::new(0i64);
    let raised = |ui: &mut Ui, events: Vec<Event>| -> (bool, bool) {
        let mut out = (false, false);
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            let front = ui.box_("front", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(60.0)));
            front_key.set(key(&front));
            ui.to_front(0, &front);
            let back = ui.box_("back", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(60.0)));
            for b in [&front, &back] {
                ui.draw(b, |paint, Rect { x, y, w, h }| paint.fill(x, y, w, h, color::white()));
            }
            out = (ui.signal(&front).clicked, ui.signal(&back).clicked);
        });
        out
    };
    raised(&mut ui, vec![]);
    if raised(&mut ui, vec![press((20, 20)), release((20, 20))]) != (true, false) {
        fail("a later body covered a raised floating control");
    }
    let fk = front_key.get();
    let under = move |k: i64| -> Vec<scene::Node> {
        if k == fk { vec![scene::rect((0, 0), 100, 60, Some(rays_math::color::black()), None).unwrap()] } else { Vec::new() }
    };
    let ui = std::rc::Rc::new(std::cell::RefCell::new(ui));
    let nodes = rays_shell::host::scene(&ui, Some(&under));
    let mut ui = std::rc::Rc::try_unwrap(ui).ok().expect("one owner").into_inner();
    match layer::stage_native(320, 240, &nodes) {
        Ok(staged) => {
            let ok = matches!(
                &staged.layers[..],
                [layer::NativeLayer::UiLayer(..), layer::NativeLayer::Scene2Layer(..), layer::NativeLayer::UiLayer(..)]
            );
            if !ok {
                fail("native content was not between the background and floating controls");
            }
        }
        Err(_) => fail("native content was not between the background and floating controls"),
    }
    ui.destroy();
}

pub fn window_pxui_two_widgets() {
    // Two text widgets share nothing: a double click's word in one and its undo stack do not reach
    // the numeric editor opened after it (a drag there stays inside its own text, and Command-Z
    // with nothing typed is the host's).
    let mut ui = ui11();
    let words = RefCell::new("alpha beta gamma".to_owned());
    let number = RefCell::new("240".to_owned());
    let clock = Cell::new(0.0);
    let tick = |ui: &mut Ui, keys: Vec<Key>, dt: f64, events: Vec<Event>| {
        clock.set(clock.get() + dt);
        ui.frame(&frame_at(keys, 1.0, clock.get(), events), &mut |ui| {
            let w = words.borrow().clone();
            *words.borrow_mut() = ui.value_field((20.0, 20.0), 100.0, 21.0, &ValueFieldOpts::default(), &|_| true, "words", &w).0;
            let n = number.borrow().clone();
            let o = ValueFieldOpts { scrub: Some(Rc::new(|origin, _, _| origin.to_owned())), ..ValueFieldOpts::default() };
            *number.borrow_mut() = ui
                .value_field((20.0, 60.0), 100.0, 21.0, &o, &|text| text.parse::<i64>().is_ok(), "number", &n)
                .0;
        });
    };
    tick(&mut ui, vec![], 0.05, vec![]);
    tick(&mut ui, vec![], 0.05, vec![press((100, 30)), release((100, 30))]);
    tick(&mut ui, vec![], 1.0, vec![press((100, 30)), release((100, 30)), press((100, 30))]);
    tick(&mut ui, vec![], 0.05, vec![release((100, 30))]);
    tick(&mut ui, vec![], 0.05, vec![Event::TextInput("X".into())]);
    tick(&mut ui, vec![], 1.0, vec![press((60, 70)), release((60, 70)), press((60, 70))]);
    tick(&mut ui, vec![], 0.05, vec![move_to((61, 70))]);
    tick(&mut ui, vec![], 0.05, vec![release((61, 70))]);
    if !ui.text_input_focused() {
        fail("a double click did not open the numeric editor");
    }
    tick(&mut ui, vec![Key::Meta], 0.05, vec![key_down(kc('z'))]);
    if ui.passed_undo() != Some(UndoKind::Undo) {
        fail("a numeric editor undid another widget's edit");
    }
    tick(&mut ui, vec![], 0.05, vec![key_down(Key::Escape)]);
    if *number.borrow() != "240" {
        fail(&format!("a numeric editor took another widget's text: {}", number.borrow()));
    }
    ui.destroy();
}

pub fn window_pxui_carry() {
    // A carry: held once the pointer has left the 4-point dead zone of the press, seen as hover by
    // a box that does not own the press, put once on release, and gone after a pointer
    // cancellation or a focus loss. A key starts one with no capture: a press is the put.
    let mut ui = ui11();
    let clicked = Cell::new(false);
    let popup = Cell::new(false);
    let carried = |ui: &mut Ui, events: Vec<Event>| -> (Option<Payload>, Option<Drop>, bool) {
        let mut out = (None, None, false);
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            if popup.get() {
                ui.popup(None, None, true, false, (200.0, 0.0), 100.0, 40.0, "over", &mut |_| {});
            }
            let source = ui.box_("source", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(40.0)));
            let target = ui.box_("target", BoxOpts::default().flags(CLICKABLE).at((0.0, 100.0)).w(Size::Px(100.0)).h(Size::Px(40.0)));
            if ui.signal(&source).held {
                ui.carry(Some(&source), "material", "(ref cobalt)");
            }
            clicked.set(clicked.get() || ui.signal(&target).clicked);
            out = (ui.carrying(), ui.drop_target(&target), ui.signal(&target).hovered);
        });
        out
    };
    let payload = Payload { kind: "material".into(), value: "(ref cobalt)".into() };
    carried(&mut ui, vec![]);
    carried(&mut ui, vec![]);
    match carried(&mut ui, vec![press((10, 10))]) {
        (None, None, false) => {}
        _ => fail("a press alone started a carry"),
    }
    match carried(&mut ui, vec![move_to((12, 12))]) {
        (None, None, false) => {}
        _ => fail("a carry started inside the dead zone"),
    }
    match carried(&mut ui, vec![move_to((10, 120))]) {
        (Some(held), Some(Drop::Hover(over)), true) if held == payload && over == payload => {}
        _ => fail("a box that does not own the press did not see the carry hover"),
    }
    match carried(&mut ui, vec![move_to((10, 110))]) {
        (Some(_), Some(Drop::Hover(_)), _) => {}
        _ => fail("the carry did not follow the pointer"),
    }
    match carried(&mut ui, vec![release((10, 110))]) {
        (None, Some(Drop::Dropped(dropped)), _) if dropped == payload => {}
        _ => fail("a release over a target did not deliver the payload"),
    }
    match carried(&mut ui, vec![]) {
        (None, None, _) => {}
        _ => fail("the put was delivered twice"),
    }
    if clicked.get() {
        fail("the release of a carry clicked the target");
    }
    // a release over nothing delivers nothing, and the payload is gone
    carried(&mut ui, vec![press((10, 10))]);
    carried(&mut ui, vec![move_to((10, 120))]);
    match carried(&mut ui, vec![move_to((200, 200)), release((200, 200))]) {
        (None, None, _) => {}
        _ => fail("a release over nothing delivered a payload"),
    }
    // cancellation and focus loss clear it, delivering nothing
    carried(&mut ui, vec![press((10, 10))]);
    carried(&mut ui, vec![move_to((10, 120))]);
    match carried(&mut ui, vec![Event::PointerCancelled(MouseButton::LeftButton)]) {
        (None, None, _) => {}
        _ => fail("a pointer cancellation left the payload held"),
    }
    match carried(&mut ui, vec![release((10, 120))]) {
        (None, None, _) => {}
        _ => fail("a cancelled carry was put on release"),
    }
    carried(&mut ui, vec![press((10, 10))]);
    carried(&mut ui, vec![move_to((10, 120))]);
    match carried(&mut ui, vec![Event::FocusLost]) {
        (None, None, _) => {}
        _ => fail("a focus loss left the payload held"),
    }
    carried(&mut ui, vec![release((10, 120))]);
    // a popup that takes the capture takes the carry: the release after it puts nothing
    carried(&mut ui, vec![press((10, 10))]);
    carried(&mut ui, vec![move_to((10, 120))]);
    popup.set(true);
    carried(&mut ui, vec![]);
    popup.set(false);
    match carried(&mut ui, vec![release((10, 120))]) {
        (None, None, _) => {}
        _ => fail("a carry outlived the capture a popup took"),
    }
    // from a key: no capture, hover from the pointer, the next left press is the put
    ui.carry(None, "sop", "(ref shards)");
    if ui.carrying().is_none() {
        fail("a key did not start a carry");
    }
    match carried(&mut ui, vec![move_to((10, 120))]) {
        (Some(_), Some(Drop::Hover(p)), true) if p.kind == "sop" => {}
        _ => fail("a key carry was not seen hovering"),
    }
    match carried(&mut ui, vec![press((10, 120))]) {
        (None, Some(Drop::Dropped(p)), _) if p.value == "(ref shards)" => {}
        _ => fail("a press did not put a key carry"),
    }
    carried(&mut ui, vec![release((10, 120))]);
    if clicked.get() {
        fail("the press that put a key carry also clicked");
    }
    // the host cancels (Escape)
    ui.carry(None, "sop", "(ref shards)");
    ui.cancel_carry();
    if ui.carrying().is_some() {
        fail("cancel_carry kept the payload");
    }
    // an idle frame after a carry builds nothing new
    carried(&mut ui, vec![]);
    let idle = ui.output().draw.len();
    ui.carry(None, "sop", "(ref shards)");
    carried(&mut ui, vec![move_to((50, 50))]);
    ui.cancel_carry();
    carried(&mut ui, vec![]);
    if ui.output().draw.len() != idle {
        fail("an idle frame painted a ghost");
    }
    ui.destroy();
}

pub fn window_pxui_retired_callbacks() {
    // Published frames retain no callback payloads: each closure releases its last captured owner after painting.
    let mut ui = Ui::create(Platform::Apple, None, Some(font_path()), &rays_shell::host::BACKEND);
    let held: Vec<Rc<[u8; 32]>> = (0..5).map(|_| Rc::new([b'x'; 32])).collect();
    let seen: Rc<RefCell<[bool; 5]>> = Rc::new(RefCell::new([false; 5]));
    let painter = |index: usize| -> Box<dyn Fn(&mut Paint, Rect)> {
        let (bytes, seen) = (held[index].clone(), seen.clone());
        Box::new(move |_, _| {
            seen.borrow_mut()[index] = true;
            let _ = bytes.len();
        })
    };
    let hit = {
        let (bytes, seen) = (held[2].clone(), seen.clone());
        Rc::new(move |rect: Rect| {
            seen.borrow_mut()[2] = true;
            let _ = bytes.len();
            rect
        })
    };
    let relative = |index: usize| -> Size {
        let (bytes, seen) = (held[index].clone(), seen.clone());
        Size::Rel(Rc::new(move |size| {
            seen.borrow_mut()[index] = true;
            let _ = bytes.len();
            size
        }))
    };
    ui.frame(&frame_at(vec![], 1.0, 0.0, vec![]), &mut |ui| {
        let b = ui.box_("callbacks", BoxOpts::default().w(relative(3)).h(relative(4)).hit(hit.clone()));
        ui.draw(&b, painter(0));
        ui.draw_over(&b, painter(1));
    });
    drop(hit);
    if !seen.borrow().iter().all(|s| *s) {
        fail("callback retention fixture did not run");
    }
    if held.iter().any(|h| Rc::strong_count(h) > 1) {
        fail("a published UI frame retained a callback's payload");
    }
    ui.frame(&frame_at(vec![], 1.0, 1.0, vec![]), &mut |_| {});
    if held.iter().any(|h| Rc::strong_count(h) > 1) {
        fail("a smaller UI frame retained a retired paint, overlay, hit or size callback");
    }
    ui.destroy();
}

pub fn window_pxui_retained_keys() {
    // Keys that come and go (150,000 of them, 500 a frame) leave the retained state the size it was.
    let mut ui = ui11();
    let next = Cell::new(0i64);
    let churn = |ui: &mut Ui| {
        ui.frame(&frame_at(vec![], 1.0, 0.0, vec![]), &mut |ui| {
            for _ in 0..500 {
                next.set(next.get() + 1);
                ui.box_(&next.get().to_string(), BoxOpts::default().w(Size::Px(1.0)).h(Size::Px(1.0)));
            }
        });
    };
    churn(&mut ui);
    churn(&mut ui);
    let settled = ui.retained_slots();
    for _ in 0..300 {
        churn(&mut ui);
    }
    if ui.retained_slots() > 2 * settled {
        fail("churned keys grew the UI's retained tables");
    }
    ui.destroy();
}

/// Text undo: one byte cap over the open stacks and the shelf (five stacks of 200 snapshots would
/// otherwise be 100 MB at 100 KB of text), and the ledger follows it. The rows of an unchanged
/// text are not walked again, with several panes on screen, and belong to the handle.
pub fn window_pxui_text_memory() {
    let mut ui = ui11();
    let mut other = ui11();
    let owner = mem::document("pxui.text_undo");
    let at_start = mem::bytes(&owner);
    for key in 1..=5i64 {
        for step in 1..=200i64 {
            let last = format!("{} {} {}", key, step, "x".repeat(100_000));
            let current = if step == 1 { last.clone() } else { ui.edit_value.clone() };
            let mut edit = ui.load_text_edit(key, &current);
            edit.text = last.clone();
            ui.remember(&edit, false);
            ui.edit_value = last;
        }
    }
    let bytes = ui.text_undo_bytes();
    rays_shell::host::charge(&mut ui);
    if bytes > TEXT_UNDO_BYTE_CAP {
        fail("text undo passed its byte cap across stacks");
    }
    if bytes < TEXT_UNDO_BYTE_CAP / 2 {
        fail("text undo kept far less than its cap");
    }
    if mem::bytes(&owner) - at_start != bytes as i64 {
        fail("text undo's charge is not what it holds");
    }
    if ui.edit_undo.is_empty() {
        fail("the open text keeps its newest steps");
    }
    let oversize = "y".repeat(2 * TEXT_UNDO_BYTE_CAP);
    let mut edit = ui.load_text_edit(9, "small");
    edit.text = oversize;
    ui.remember(&edit, false);
    edit.text = "after".to_owned();
    ui.remember(&edit, false);
    if ui.edit_undo.is_empty() {
        fail("a text over the cap still undoes once");
    }
    let (a, b) = ("alpha\nbeta", "gamma\ndelta");
    let first_a = ui.text_rows(None, a);
    let first_b = ui.text_rows(None, b);
    for _ in 0..4 {
        if !Rc::ptr_eq(&ui.text_rows(None, a), &first_a) || !Rc::ptr_eq(&ui.text_rows(None, b), &first_b) {
            fail("rows were walked again for an unchanged text");
        }
    }
    if Rc::ptr_eq(&other.text_rows(None, a), &first_a) {
        fail("the rows belong to the handle");
    }
    if Rc::ptr_eq(&ui.text_rows(Some(3), a), &first_a) {
        fail("rows are per wrap width");
    }
    rays_shell::host::destroy(&mut ui);
    rays_shell::host::destroy(&mut other);
    if mem::bytes(&owner) != at_start {
        fail("destroying the handle releases its charge");
    }
}

pub const CASES: &[(&str, fn())] = &[
    ("window_pxui_text_area", window_pxui_text_area),
    ("window_pxui_text_area_windows", window_pxui_text_area_windows),
    ("window_pxui_modal_1", window_pxui_modal_1),
    ("window_pxui_modal_2", window_pxui_modal_2),
    ("window_pxui_modal_3", window_pxui_modal_3),
    ("window_pxui_modal_4", window_pxui_modal_4),
    ("window_pxui_two_widgets", window_pxui_two_widgets),
    ("window_pxui_carry", window_pxui_carry),
    ("window_pxui_retired_callbacks", window_pxui_retired_callbacks),
    ("window_pxui_retained_keys", window_pxui_retained_keys),
    ("window_pxui_text_memory", window_pxui_text_memory),
];
