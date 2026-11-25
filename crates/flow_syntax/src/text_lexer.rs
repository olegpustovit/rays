use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Word {
    Symbol,
    Keyword,
    Number,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Open(char),
    Close(char),
    Prefix(char),
    Meta(String),
    Comment(String),
    Word(Word),
    String(Vec<u8>),
    End,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Issue {
    pub start: i64,
    pub finish: i64,
    pub code: String,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: Kind,
    pub start: i64,
    pub stop: i64,
    pub issue: Option<Issue>,
}

pub fn separator(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n' | '(' | ')' | '[' | ']' | '{' | '}' | ';' | '"')
}

pub fn number(text: &str) -> bool {
    if matches!(text, "NaN" | "Infinity" | "-Infinity") {
        return true;
    }
    let bytes = text.as_bytes();
    let mut i = if bytes.first() == Some(&b'-') { 1 } else { 0 };
    let mut before = 0;
    let mut after = 0;
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        before += 1;
        i += 1;
    }
    if i < bytes.len() && bytes[i] == b'.' {
        i += 1;
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            after += 1;
            i += 1;
        }
    }
    let mantissa = before + after > 0;
    if mantissa && i < bytes.len() && matches!(bytes[i], b'e' | b'E') {
        i += 1;
        if i < bytes.len() && matches!(bytes[i], b'+' | b'-') {
            i += 1;
        }
        let exponent = i;
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            i += 1;
        }
        i == bytes.len() && i > exponent
    } else {
        i == bytes.len() && mantissa
    }
}

fn issue(start: usize, finish: usize, code: &str, message: &str) -> Issue {
    Issue { start: start as i64, finish: finish as i64, code: code.to_owned(), message: message.to_owned() }
}

fn token(kind: Kind, start: usize, stop: usize, issue: Option<Issue>) -> Token {
    Token { kind, start: start as i64, stop: stop as i64, issue }
}

fn word_end(bytes: &[u8], mut i: usize) -> usize {
    while i < bytes.len() && !matches!(bytes[i], b' ' | b'\t' | b'\r' | b'\n' | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b';' | b'"') {
        i += 1;
    }
    i
}

fn trim_comment(source: &str, mut start: usize, mut end: usize) -> String {
    let bytes = source.as_bytes();
    while start < end && matches!(bytes[start], b' ' | b'\t' | b'\r' | b'\n') {
        start += 1;
    }
    while end > start && matches!(bytes[end - 1], b' ' | b'\t' | b'\r' | b'\n') {
        end -= 1;
    }
    source[start..end].to_owned()
}

fn string_token(source: &str, start: usize) -> (Token, usize) {
    let bytes = source.as_bytes();
    let mut decoded = Vec::with_capacity(32);
    let mut first_issue = None;
    let mut j = start + 1;
    loop {
        if j >= bytes.len() {
            if first_issue.is_none() {
                first_issue = Some(issue(start, bytes.len(), "E_UNCLOSED", "This string is never closed"));
            }
            break;
        }
        match bytes[j] {
            b'"' => {
                j += 1;
                break;
            }
            b'\\' if j + 1 >= bytes.len() => {
                if first_issue.is_none() {
                    first_issue = Some(issue(start, bytes.len(), "E_UNCLOSED", "This string is never closed"));
                }
                j = bytes.len();
                break;
            }
            b'\\' => {
                let escaped = bytes[j + 1];
                match escaped {
                    b'"' | b'\\' => decoded.push(escaped),
                    b'n' => decoded.push(b'\n'),
                    b't' => decoded.push(b'\t'),
                    b'r' => decoded.push(b'\r'),
                    b'0'..=b'9' if j + 3 < bytes.len() && bytes[j + 2].is_ascii_digit() && bytes[j + 3].is_ascii_digit() => {
                        let value = (escaped - b'0') as u16 * 100
                            + (bytes[j + 2] - b'0') as u16 * 10
                            + (bytes[j + 3] - b'0') as u16;
                        if value > 255 {
                            if first_issue.is_none() {
                                first_issue = Some(issue(j, j + 4, "E_UNEXPECTED", "A decimal byte escape must be 000 to 255."));
                            }
                        } else {
                            decoded.push(value as u8);
                        }
                        j += 4;
                        continue;
                    }
                    _ => {
                        if first_issue.is_none() {
                            first_issue = Some(issue(j, j + 2, "E_UNEXPECTED", "Unknown string escape"));
                        }
                        j += 2;
                        continue;
                    }
                }
                j += 2;
            }
            _ => {
                decoded.push(bytes[j]);
                j += 1;
            }
        }
    }
    (token(Kind::String(decoded), start, j, first_issue), j)
}


struct LineCk { start: usize, from: usize }

#[derive(Default)]
struct LexCache { text: String, tokens: Vec<Token>, lines: Vec<LineCk> }

thread_local!(static LEX_CACHE: RefCell<LexCache> = RefCell::new(LexCache::default()));

#[cfg(test)]
fn reset_lex_cache() { LEX_CACHE.with(|c| *c.borrow_mut() = LexCache::default()); }

fn build_lines(text: &str, tokens: &[Token]) -> Vec<LineCk> {
    let bytes = text.as_bytes();
    let mut starts = vec![0usize];
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'\n' && i + 1 < bytes.len() { starts.push(i + 1); }
    }
    let mut lines = Vec::with_capacity(starts.len());
    let mut ti = 0;
    let n = tokens.len();
    for (li, &start) in starts.iter().enumerate() {
        let stop = starts.get(li + 1).copied().unwrap_or(text.len());
        let from = ti;
        while ti < n {
            let tok = &tokens[ti];
            if matches!(tok.kind, Kind::End) && tok.issue.is_none() { break; }
            if (tok.start as usize) >= stop { break; }
            ti += 1;
        }
        lines.push(LineCk { start, from });
    }
    lines
}

/// Incremental lex: reuse tokens before the first changed line (parinfer-style line checkpoints).
pub fn lex(source: &str) -> Vec<Token> {
    LEX_CACHE.with(|cell| {
        let mut cache = cell.borrow_mut();
        if cache.text.as_str() == source { return cache.tokens.clone(); }
        let prefix = cache.text.as_bytes().iter().zip(source.as_bytes()).take_while(|(a, b)| a == b).count();
        let first_line = if cache.lines.is_empty() { 0 } else { cache.lines.partition_point(|l| l.start <= prefix).saturating_sub(1) };
        if first_line > 0 && first_line < cache.lines.len() {
            let cut = cache.lines[first_line].start;
            let tok_from = cache.lines[first_line].from;
            let open_string = cache.tokens[..tok_from].iter().any(|t| matches!(t.kind, Kind::String(_)) && (t.stop as usize) > cut);
            if !open_string {
                let mut tokens: Vec<_> = cache.tokens[..tok_from].iter().filter(|t| !(matches!(t.kind, Kind::End) && t.issue.is_none())).cloned().collect();
                let mut rest = lex_full(&source[cut..]);
                for t in &mut rest {
                    t.start += cut as i64; t.stop += cut as i64;
                    if let Some(issue) = &mut t.issue { issue.start += cut as i64; issue.finish += cut as i64; }
                }
                tokens.extend(rest);
                cache.lines = build_lines(source, &tokens);
                cache.text = source.to_owned();
                cache.tokens.clone_from(&tokens);
                return tokens;
            }
        }
        let tokens = lex_full(source);
        cache.lines = build_lines(source, &tokens);
        cache.text = source.to_owned();
        cache.tokens.clone_from(&tokens);
        tokens
    })
}

pub fn lex_full(source: &str) -> Vec<Token> {
    let bytes = source.as_bytes();
    let n = bytes.len();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < n {
        match bytes[i] {
            b' ' | b'\t' | b'\r' | b'\n' => i += 1,
            b';' => {
                let start = i;
                while i < n && bytes[i] != b'\n' { i += 1; }
                let finish = i;
                let mut content = start;
                while content < finish && bytes[content] == b';' { content += 1; }
                if content < finish && bytes[content] == b' ' { content += 1; }
                let text = trim_comment(source, content, finish);
                tokens.push(token(Kind::Comment(text), start, finish, None));
            }
            b'(' | b'[' | b'{' => {
                tokens.push(token(Kind::Open(bytes[i] as char), i, i + 1, None));
                i += 1;
            }
            b')' | b']' | b'}' => {
                tokens.push(token(Kind::Close(bytes[i] as char), i, i + 1, None));
                i += 1;
            }
            b'\'' | b'`' => {
                tokens.push(token(Kind::Prefix(bytes[i] as char), i, i + 1, None));
                i += 1;
            }
            b'~' if i + 1 < n && bytes[i + 1] == b'@' => {
                tokens.push(token(Kind::Prefix('@'), i, i + 2, None));
                i += 2;
            }
            b'~' => {
                tokens.push(token(Kind::Prefix('~'), i, i + 1, None));
                i += 1;
            }
            b'^' => {
                let finish = word_end(bytes, i + 1);
                let valid = finish > i + 2 && bytes[i + 1] == b':';
                let problem = if valid { None } else {
                    Some(issue(i, finish.max(i + 1), "E_UNEXPECTED", "^ is followed by a keyword, as in ^:bypass"))
                };
                let name = if valid { source[i + 2..finish].to_owned() } else { String::new() };
                tokens.push(token(Kind::Meta(name), i, finish, problem));
                i = finish;
            }
            b'"' => {
                let (lexed, finish) = string_token(source, i);
                tokens.push(lexed);
                i = finish;
            }
            _ => {
                let start = i;
                let finish = word_end(bytes, i);
                let text = &source[start..finish];
                let word = if text.len() > 1 && text.starts_with(':') {
                    Word::Keyword
                } else if number(text) {
                    Word::Number
                } else {
                    Word::Symbol
                };
                tokens.push(token(Kind::Word(word), start, finish, None));
                i = finish;
            }
        }
    }
    tokens.push(token(Kind::End, n, n, None));

    let mut controls = Vec::new();
    for (at, &byte) in bytes.iter().enumerate() {
        if (1..=3).contains(&byte) {
            controls.push(token(
                Kind::End,
                at,
                at + 1,
                Some(issue(at, at + 1, "E_UNEXPECTED", &format!("Control byte {} is not allowed in a workspace", byte))),
            ));
        }
    }
    if !controls.is_empty() {
        tokens.extend(controls);
        tokens.sort_by_key(|item| item.start);
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::{lex, lex_full, number, reset_lex_cache, Kind, Word};

    #[test]
    fn numbers_and_tolerant_lexing() {
        for value in ["1e-14", "2e+06", "6.1E-17", "-.5", "1.e2", "Infinity"] {
            assert!(number(value));
        }
        for value in ["e5", "1e", "1e+", "1e5x", "-", "."] {
            assert!(!number(value));
        }
        let tokens = lex("(a \"bad\\q\" b)");
        assert_eq!(tokens.iter().find_map(|token| token.issue.as_ref()).unwrap().start, 7);
        assert!(tokens.iter().any(|token| {
            matches!(token.kind, Kind::Word(Word::Symbol)) && token.start == 11
        }));
    }

    #[test]
    fn incremental_matches_full_over_sketch_edits() {
        let root = format!("{}/../../sketches", env!("CARGO_MANIFEST_DIR"));
        let mut sketches = Vec::new();
        if let Ok(rd) = std::fs::read_dir(&root) {
            for e in rd.flatten() {
                let path = e.path();
                if path.extension().and_then(|s| s.to_str()) == Some("rays") {
                    if let Ok(text) = std::fs::read_to_string(&path) { sketches.push(text); }
                }
            }
        }
        assert!(!sketches.is_empty(), "expected sketches/*.rays under {root}");
        let mut seed = 0x9e37_79b9_u64;
        let mut next = || { seed = seed.wrapping_mul(0x5851_f42d_4c95_7f2d).wrapping_add(1); seed };
        for text in &sketches {
            reset_lex_cache();
            let _ = lex(text);
            for _ in 0..24 {
                let mut s = text.clone();
                let op = next() % 3;
                if s.is_empty() { s.push('x'); }
                else {
                    let at = (next() as usize) % s.len();
                    let at = (0..=at).rev().find(|&i| s.is_char_boundary(i)).unwrap_or(0);
                    match op {
                        0 => s.insert_str(at, "x"),
                        1 => { let end = (at + 1..=s.len()).find(|&i| s.is_char_boundary(i)).unwrap_or(s.len()); s.replace_range(at..end, ""); }
                        _ => { let end = (at + 1..=s.len()).find(|&i| s.is_char_boundary(i)).unwrap_or(s.len()); s.replace_range(at..end, "y"); }
                    }
                }
                assert_eq!(lex(&s), lex_full(&s), "incremental != full after edit");
            }
        }
    }
}
