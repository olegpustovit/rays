pub fn rewrite(text: &str, caret: i64) -> (String, i64) {
    let bytes = text.as_bytes();
    let n = bytes.len();
    let lines: Vec<(usize, usize)> = {
        let mut lines = Vec::new();
        let mut start = 0;
        loop {
            let stop = bytes[start..].iter().position(|&b| b == b'\n').map_or(n, |i| start + i);
            lines.push((start, stop));
            if stop == n { break; }
            start = stop + 1;
        }
        lines
    };
    let mut code_end = vec![0; lines.len()];
    let mut trails = vec![String::new(); lines.len()];
    let mut removed = Vec::new();
    let mut stack: Vec<(u8, usize)> = Vec::new();
    let mut last_code: Option<usize> = None;
    let mut in_string = false;
    for (index, &(start, stop)) in lines.iter().enumerate() {
        let mut code = vec![true; stop - start];
        let opened_in_string = in_string;
        let mut comment = stop;
        let mut i = start;
        while i < comment {
            let c = bytes[i];
            if in_string {
                code[i - start] = false;
                if c == b'\\' && i + 1 < stop { code[i + 1 - start] = false; i += 1; }
                else if c == b'"' { in_string = false; }
            } else if c == b'"' { in_string = true; code[i - start] = false; }
            else if c == b';' { comment = i; }
            i += 1;
        }
        let is_code = |offset: usize| offset < comment && code[offset - start];
        let space = |c: u8| matches!(c, b' ' | b'\t' | b'\r');
        let mut te = comment;
        while te > start && is_code(te - 1) && space(bytes[te - 1]) { te -= 1; }
        let mut ts = te;
        while ts > start && is_code(ts - 1) && (matches!(bytes[ts - 1], b')' | b']' | b'}') || space(bytes[ts - 1])) { ts -= 1; }
        code_end[index] = ts;
        if caret > ts as i64 && caret < te as i64 { ts = caret as usize; }
        let trail_here = bytes[ts..te].iter().any(|c| matches!(c, b')' | b']' | b'}'));
        if !trail_here { ts = te; }
        if trail_here && ts > code_end[index] { code_end[index] = ts; }
        let mut first = start;
        while first < ts && space(bytes[first]) { first += 1; }
        let is_code_line = first < ts && first < comment && !opened_in_string;
        if is_code_line {
            let column = first - start;
            while stack.last().is_some_and(|(_, col)| *col >= column) {
                let (closer, _) = stack.pop().unwrap();
                if let Some(line) = last_code { trails[line].push(closer as char); }
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
        if first < ts && first < comment && !in_string { last_code = Some(index); }
    }
    for (closer, _) in stack { if let Some(line) = last_code { trails[line].push(closer as char); } }
    removed.sort_unstable(); removed.dedup();
    let mut out = Vec::with_capacity(n + 8);
    let mut shift: isize = 0;
    let mut removal = 0;
    for (index, &(start, stop)) in lines.iter().enumerate() {
        for i in start..=stop {
            if i == code_end[index] && !trails[index].is_empty() {
                out.extend_from_slice(trails[index].as_bytes());
                if (i as i64) < caret { shift += trails[index].len() as isize; }
            }
            if i < stop {
                if removal < removed.len() && removed[removal] == i { removal += 1; if (i as i64) < caret { shift -= 1; } }
                else { out.push(bytes[i]); }
            } else if i < n { out.push(b'\n'); }
        }
    }
    (String::from_utf8(out).expect("inserting ASCII closers preserves UTF-8"), caret.saturating_add(shift as i64))
}
#[cfg(test)]
mod tests {
    use super::rewrite;
    #[test]
    fn full_width_caret_preserves_negative_sentinels_and_saturates() {
        for caret in [i64::MIN, -1, 1i64 << 62] {
            assert_eq!(rewrite("(a", caret), ("(a)".into(), caret.saturating_add(i64::from(caret > 2))));
        }
        assert_eq!(rewrite("(a", i64::MAX), ("(a)".into(), i64::MAX));
    }
    #[test]
    fn infers_closers_and_preserves_utf8() {
        assert_eq!(rewrite("(a\n  (b\nc", 0), ("(a\n  (b))\nc".into(), 0));
        assert_eq!(rewrite("(λ", 0), ("(λ)".into(), 0));
        assert_eq!(rewrite("(a", -3), ("(a)".into(), -3));
    }
}
