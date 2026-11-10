//! Invalid native UTF-8 bytes use isolated low surrogates.
//! Valid Unicode stays Unicode; runtime textBytes reverses the exceptional byte representation.
use std::fmt::Write;

fn push_valid(out: &mut String, text: &str) {
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 32 || c as u32 == 127 => {
                let _ = write!(out, "\\u{:04x}", c as u32);
            }
            c => out.push(c),
        }
    }
}

pub fn quote(s: &[u8]) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    let mut rest = s;
    while !rest.is_empty() {
        match std::str::from_utf8(rest) {
            Ok(text) => {
                push_valid(&mut out, text);
                break;
            }
            Err(e) => {
                let (valid, tail) = rest.split_at(e.valid_up_to());
                push_valid(&mut out, std::str::from_utf8(valid).expect("valid prefix"));
                let _ = write!(out, "\\udc{:02x}", tail[0]);
                rest = &tail[1..];
            }
        }
    }
    out.push('"');
    out
}
