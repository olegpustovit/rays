//! Indent-based closer inference. `rewrite` keeps the previous call's per-line records in a thread-local, so a single
//! insert or delete re-scans only from its line until the parser state meets the old one again (one line for a typed
//! character); anything else (paste, newline, another pane's text) re-scans from the first changed line, or all of it.
//! Records are validated against the stored text on every call, so a stale cache can only cost time, never change output.
//! When the rewrite adds or removes nothing, `rewrite` returns `Cow::Borrowed` (no allocation).
use std::borrow::Cow;
use std::cell::RefCell;

type Stack = Vec<(u8, usize)>;

/// One scanned line. `back` is the distance from this line to the last code line before it (relative, so a record
/// stays valid when lines are inserted above it); the closers this line popped go to the line `pop_back` above it.
#[derive(Default)]
struct Line {
    start: usize,
    stop: usize,
    stack: Stack,
    back: Option<usize>,
    in_string: bool,
    code_end: usize,
    removed: Vec<usize>,
    pops: Vec<u8>,
    pop_back: usize,
}

#[derive(Default)]
struct Cache {
    text: String,
    caret: i64,
    lines: Vec<Line>,
    old_tail: Vec<Line>,
    spare_lines: Vec<Line>,
    scan_stack: Stack,
    end_stack: Stack,
    end_back: Option<usize>,
    code: Vec<bool>,
    /// Reused by `finish` so a no-op rewrite (output == input) allocates nothing after warm.
    out: Vec<u8>,
    /// Scratch for `finish` trails / closer bytes (cleared each call; capacity retained).
    trails: Vec<(usize, Vec<u8>)>,
    trail: Vec<u8>,
    tail: Vec<u8>,
}

thread_local!(static CACHE: RefCell<Cache> = RefCell::new(Cache::default()));

pub fn rewrite<'a>(text: &'a str, caret: i64) -> (Cow<'a, str>, i64) {
    CACHE.with(|cache| match cache.try_borrow_mut() {
        Ok(mut cache) => cache.rewrite(text, caret),
        Err(_) => Cache::default().rewrite(text, caret),
    })
}

/// Scan every line from nothing (the reference the incremental path must equal).
pub fn rewrite_full<'a>(text: &'a str, caret: i64) -> (Cow<'a, str>, i64) { Cache::default().rewrite(text, caret) }

fn line_at(lines: &[Line], offset: usize) -> usize { lines.partition_point(|l| l.start <= offset).saturating_sub(1) }

impl Cache {
    fn rewrite<'a>(&mut self, text: &'a str, caret: i64) -> (Cow<'a, str>, i64) {
        let (old, new) = (self.text.as_bytes(), text.as_bytes());
        let n = new.len();
        let limit = old.len().min(n);
        let prefix = old.iter().zip(new).take_while(|(a, b)| a == b).count();
        let suffix = old.iter().rev().zip(new.iter().rev()).take(limit - prefix).take_while(|(a, b)| a == b).count();
        // First line that can differ: the one holding the first changed byte or either caret (a caret inside a line's
        // trailing closers changes that line only).
        let mut mark = prefix;
        for c in [self.caret, caret] { if c >= 0 { mark = mark.min(c as usize); } }
        let first = if self.lines.is_empty() { 0 } else { line_at(&self.lines, mark) };
        let delta = n as isize - old.len() as isize;
        let (old_caret, old_n) = (self.caret, old.len());
        let safe = |c: i64, at: usize, len: usize| c <= at as i64 || c >= len as i64;
        let mut lines = std::mem::take(&mut self.lines);
        let mut old_tail = std::mem::take(&mut self.old_tail);
        old_tail.extend(lines.drain(first..));
        let mut stack = std::mem::take(&mut self.scan_stack);
        stack.clear();
        let (mut last, mut in_string, mut start) = match old_tail.first() {
            Some(line) => {
                stack.extend_from_slice(&line.stack);
                (line.back.map(|back| first - back), line.in_string, line.start)
            }
            None => (None, false, 0),
        };
        loop {
            let index = lines.len();
            let stop = new[start..].iter().position(|&b| b == b'\n').map_or(n, |i| start + i);
            let back = last.map(|l| index - l);
            let record = self.scan(new, caret, index, start, stop, &mut stack, &mut last, &mut in_string, back);
            lines.push(record);
            if stop == n {
                self.end_back = last.map(|l| lines.len() - l);
                std::mem::swap(&mut self.end_stack, &mut stack);
                break;
            }
            start = stop + 1;
            // Meet the old records again: same bytes (inside the common suffix), same parser state, no caret nearby.
            if start + suffix >= n {
                let old_start = (start as isize - delta) as usize;
                if let Ok(at) = old_tail.binary_search_by_key(&old_start, |l| l.start) {
                    let next = lines.len();
                    let o = &old_tail[at];
                    if o.stack == stack && o.in_string == in_string && o.back == last.map(|l| next - l) && safe(caret, start, n) && safe(old_caret, old_start, old_n) {
                        for mut l in old_tail.drain(at..) {
                            l.start = (l.start as isize + delta) as usize;
                            l.stop = (l.stop as isize + delta) as usize;
                            for r in &mut l.removed { *r = (*r as isize + delta) as usize; }
                            lines.push(l);
                        }
                        // The old end state is still the end state.
                        break;
                    }
                }
            }
        }
        self.scan_stack = stack;
        self.spare_lines.truncate(lines.len());
        let room = lines.len() - self.spare_lines.len();
        self.spare_lines.extend(old_tail.drain(..).take(room));
        self.old_tail = old_tail;
        self.finish(lines, text, caret)
    }

    #[allow(clippy::too_many_arguments)]
    fn scan(&mut self, bytes: &[u8], caret: i64, index: usize, start: usize, stop: usize, stack: &mut Stack, last: &mut Option<usize>, in_string: &mut bool, back: Option<usize>) -> Line {
        let Line { stack: mut in_stack, mut removed, mut pops, .. } = self.spare_lines.pop().unwrap_or_default();
        in_stack.clone_from(stack);
        removed.clear();
        pops.clear();
        let opened_in_string = *in_string;
        let code = &mut self.code;
        code.clear();
        code.resize(stop - start, true);
        let mut comment = stop;
        let mut i = start;
        while i < comment {
            let c = bytes[i];
            if *in_string {
                code[i - start] = false;
                if c == b'\\' && i + 1 < stop { code[i + 1 - start] = false; i += 1; }
                else if c == b'"' { *in_string = false; }
            } else if c == b'"' { *in_string = true; code[i - start] = false; }
            else if c == b';' { comment = i; }
            i += 1;
        }
        let is_code = |offset: usize| offset < comment && code[offset - start];
        let space = |c: u8| matches!(c, b' ' | b'\t' | b'\r');
        let mut te = comment;
        while te > start && is_code(te - 1) && space(bytes[te - 1]) { te -= 1; }
        let mut ts = te;
        while ts > start && is_code(ts - 1) && (matches!(bytes[ts - 1], b')' | b']' | b'}') || space(bytes[ts - 1])) { ts -= 1; }
        let mut code_end = ts;
        if caret > ts as i64 && caret < te as i64 { ts = caret as usize; }
        let trail_here = bytes[ts..te].iter().any(|c| matches!(c, b')' | b']' | b'}'));
        if !trail_here { ts = te; }
        if trail_here && ts > code_end { code_end = ts; }
        let mut first = start;
        while first < ts && space(bytes[first]) { first += 1; }
        let is_code_line = first < ts && first < comment && !opened_in_string;
        if is_code_line {
            let column = first - start;
            while stack.last().is_some_and(|(_, col)| *col >= column) {
                let (closer, _) = stack.pop().unwrap();
                if last.is_some() { pops.push(closer); }
            }
        }
        for offset in start..ts {
            if is_code(offset) {
                match bytes[offset] {
                    b'(' => stack.push((b')', offset - start)), b'[' => stack.push((b']', offset - start)), b'{' => stack.push((b'}', offset - start)),
                    c @ (b')' | b']' | b'}') => if stack.last().is_some_and(|(closer, _)| *closer == c) { stack.pop(); } else { removed.push(offset); },
                    _ => (),
                }
            }
        }
        removed.extend(ts..te);
        let pop_back = last.map_or(0, |l| index - l);
        if first < ts && first < comment && !*in_string { *last = Some(index); }
        Line { start, stop, stack: in_stack, back, in_string: opened_in_string, code_end: code_end - start, removed, pops, pop_back }
    }

    /// Store the records and write the output. Borrowed when the rewrite leaves the bytes unchanged.
    fn finish<'a>(&mut self, lines: Vec<Line>, text: &'a str, caret: i64) -> (Cow<'a, str>, i64) {
        let bytes = text.as_bytes();
        let n = bytes.len();
        let count = lines.len();
        // Closers popped by later lines (or left on the stack at the end) land on the line they point back to.
        let mut trail_n = 0usize;
        for (index, l) in lines.iter().enumerate() {
            if l.pops.is_empty() {
                continue;
            }
            if trail_n == self.trails.len() {
                self.trails.push((0, Vec::new()));
            }
            let slot = &mut self.trails[trail_n];
            slot.0 = index - l.pop_back;
            slot.1.clear();
            slot.1.extend_from_slice(&l.pops);
            trail_n += 1;
        }
        self.tail.clear();
        self.tail.extend(self.end_stack.iter().rev().map(|(c, _)| *c));
        if let (Some(b), false) = (self.end_back, self.tail.is_empty()) {
            if trail_n == self.trails.len() {
                self.trails.push((0, Vec::new()));
            }
            let slot = &mut self.trails[trail_n];
            slot.0 = count - b;
            slot.1.clear();
            slot.1.extend_from_slice(&self.tail);
            trail_n += 1;
        }
        self.trails.truncate(trail_n);
        self.out.clear();
        self.out.reserve(n + 8);
        let mut shift: i64 = 0;
        let mut cursor = 0;
        for (index, l) in lines.iter().enumerate() {
            self.trail.clear();
            while cursor < self.trails.len() && self.trails[cursor].0 == index {
                self.trail.extend_from_slice(&self.trails[cursor].1);
                cursor += 1;
            }
            let (mut at, mut pending) = (l.start, !self.trail.is_empty());
            let tp = l.start + l.code_end;
            for r in l.removed.iter().map(|r| *r).chain(std::iter::once(l.stop)) {
                if pending && tp <= r {
                    self.out.extend_from_slice(&bytes[at..tp]);
                    self.out.extend_from_slice(&self.trail);
                    at = tp;
                    pending = false;
                    if (tp as i64) < caret { shift += self.trail.len() as i64; }
                }
                if r < l.stop {
                    self.out.extend_from_slice(&bytes[at..r]);
                    at = r + 1;
                    if (r as i64) < caret { shift -= 1; }
                }
            }
            self.out.extend_from_slice(&bytes[at..l.stop]);
            if l.stop < n { self.out.push(b'\n'); }
        }
        if self.text.capacity() < text.len() {
            self.text.reserve(text.len() - self.text.capacity() + 64);
        }
        self.text.clear();
        self.text.push_str(text);
        self.caret = caret;
        self.lines = lines;
        // Balanced typing rebuilds the same bytes (trailing closers stripped then reattached): borrow.
        if self.out.as_slice() == bytes {
            return (Cow::Borrowed(text), caret);
        }
        let out = std::mem::take(&mut self.out);
        (Cow::Owned(String::from_utf8(out).expect("inserting ASCII closers preserves UTF-8")), caret.saturating_add(shift))
    }
}
#[cfg(test)]
mod tests {
    use super::{rewrite, rewrite_full};
    use std::borrow::Cow;
    #[test]
    fn full_width_caret_preserves_negative_sentinels_and_saturates() {
        for caret in [i64::MIN, -1, 1i64 << 62] {
            let (text, at) = rewrite("(a", caret);
            assert_eq!((text.as_ref(), at), ("(a)", caret.saturating_add(i64::from(caret > 2))));
        }
        let (text, at) = rewrite("(a", i64::MAX);
        assert_eq!((text.as_ref(), at), ("(a)", i64::MAX));
    }
    #[test]
    fn infers_closers_and_preserves_utf8() {
        assert_eq!(rewrite("(a\n  (b\nc", 0).0.as_ref(), "(a\n  (b))\nc");
        assert_eq!(rewrite("(λ", 0).0.as_ref(), "(λ)");
        assert_eq!(rewrite("(a", -3).0.as_ref(), "(a)");
        assert_eq!(rewrite("(a", -3).1, -3);
    }
    #[test]
    fn closers_come_off_the_stack_innermost_first() {
        assert_eq!(rewrite_full("(a [b {c", 0).0.as_ref(), "(a [b {c}])");
    }
    #[test]
    fn balanced_typing_borrows() {
        let (text, caret) = rewrite("(a)", 2);
        assert!(matches!(text, Cow::Borrowed(_)), "no-op rewrite must not allocate");
        assert_eq!((text.as_ref(), caret), ("(a)", 2));
    }
    #[test]
    fn incremental_equals_full_over_every_sketch() {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/../../sketches");
        let mut files: Vec<_> = std::fs::read_dir(dir).unwrap().map(|e| e.unwrap().path()).filter(|p| p.extension().is_some_and(|e| e == "rays")).collect();
        files.sort();
        assert!(files.len() > 10);
        let mut seed = 0x2545F491_u64;
        let mut next = move |m: usize| { seed ^= seed << 13; seed ^= seed >> 7; seed ^= seed << 17; (seed % m as u64) as usize };
        for file in files {
            let base = std::fs::read_to_string(&file).unwrap();
            let mut text = base.clone();
            let mut caret = 0_i64;
            rewrite(&text, caret);
            for step in 0..60 {
                let mut at = next(text.len() + 1);
                while !text.is_char_boundary(at) { at -= 1; }
                let before = text.clone();
                match step % 6 {
                    0 => text.insert(at, ')'),
                    1 => text.insert(at, '\n'),
                    2 => text.insert_str(at, "(λ \"x;"),
                    3 => if at < text.len() { text.remove(at); },
                    4 => { let end = (at + 1 + next(300)).min(text.len()); let mut end = end; while !text.is_char_boundary(end) { end -= 1; } text.replace_range(at..end, ""); }
                    _ => text = base.clone(),
                }
                caret = if step % 11 == 7 { -1 } else if step % 5 == 0 { next(text.len() + 1) as i64 } else { at as i64 + 1 };
                let got = rewrite(&text, caret);
                assert_eq!(got, rewrite_full(&text, caret), "{} step {step} at {at} after {:?}", file.display(), &before[at.min(before.len())..(at + 8).min(before.len())]);
            }
        }
    }
}
