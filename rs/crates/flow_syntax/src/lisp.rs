use crate::syntax::{self, Node, QuoteKind, Syntax};
use rays_diagnostic::Span;
use std::collections::HashMap;

pub type Spans = Vec<(syntax::Id, Span)>;

const WIDTH: usize = 84;

fn wrap(form: &Syntax, text: &str) -> String {
    format!("\u{1}{}\u{2}{}\u{3}", form.id, text)
}

fn visible_width(text: &str) -> usize {
    let mut count = 0;
    let mut skipping = false;
    for byte in text.bytes() {
        match byte {
            1 => skipping = true,
            2 => skipping = false,
            3 => (),
            _ if skipping => (),
            c if c & 0xC0 != 0x80 => count += 1,
            _ => (),
        }
    }
    count
}

fn spaces(count: usize) -> String {
    " ".repeat(count)
}

pub fn float(value: f64) -> String {
    param::float_text_with_decimal(value, true)
}

fn number(raw: &str) -> String {
    if matches!(raw, "NaN" | "Infinity" | "-Infinity") || raw.contains('e') || raw.contains('E') {
        raw.to_owned()
    } else if raw.contains('.') {
        syntax::number_value(raw).map(float).unwrap_or_else(|| raw.to_owned())
    } else {
        raw.to_owned()
    }
}

fn append_string(result: &mut String, text: &str) {
    for character in text.chars() {
        match character {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            '\u{1}'..='\u{3}' => result.push_str(&format!("\\{:03}", character as u32)),
            other => result.push(other),
        }
    }
}

fn quote_string(bytes: &[u8]) -> String {
    let mut result = String::with_capacity(bytes.len() + 2);
    result.push('"');
    let mut i = 0;
    while i < bytes.len() {
        match std::str::from_utf8(&bytes[i..]) {
            Ok(text) => {
                append_string(&mut result, text);
                break;
            }
            Err(error) if error.valid_up_to() > 0 => {
                let valid = std::str::from_utf8(&bytes[i..i + error.valid_up_to()]).expect("valid UTF-8 prefix");
                append_string(&mut result, valid);
                i += error.valid_up_to();
            }
            Err(error) => {
                let invalid = error.error_len().unwrap_or(bytes.len() - i);
                for byte in &bytes[i..i + invalid] {
                    result.push_str(&format!("\\{:03}", byte));
                }
                i += invalid;
            }
        }
    }
    result.push('"');
    result
}

fn prefix(kind: QuoteKind) -> &'static str {
    match kind {
        QuoteKind::Plain => "'",
        QuoteKind::Quasi => "`",
        QuoteKind::Unquote => "~",
        QuoteKind::Splice => "~@",
    }
}

fn meta_pre(form: &Syntax) -> String {
    let mut text = String::new();
    for meta in &form.meta {
        text.push_str("^:");
        text.push_str(meta);
        text.push(' ');
    }
    text
}

fn kids(form: &Syntax) -> &[Syntax] {
    match &form.node {
        Node::List(forms) | Node::Vec(forms) | Node::Map(forms) => forms,
        Node::Quote(_, child) => std::slice::from_ref(child.as_ref()),
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) => &[],
    }
}

fn is_keyword(form: &Syntax) -> bool {
    matches!(&form.node, Node::Kw(_))
}

fn head(form: &Syntax) -> &str {
    syntax::head(form).unwrap_or("")
}

fn core_flat(form: &Syntax) -> String {
    match &form.node {
        Node::Sym(symbol) => symbol.clone(),
        Node::Kw(keyword) => format!(":{keyword}"),
        Node::Num(raw) => number(raw),
        Node::Str(text) => quote_string(text),
        Node::List(_) => join_form(form, "(", ")"),
        Node::Vec(_) => join_form(form, "[", "]"),
        Node::Map(_) => join_form(form, "{", "}"),
        Node::Quote(kind, child) => format!("{}{}", prefix(*kind), flat_(child)),
    }
}

fn join_form(form: &Syntax, open: &str, close: &str) -> String {
    let mut text = String::from(open);
    for (index, child) in kids(form).iter().enumerate() {
        if index > 0 { text.push(' '); }
        text.push_str(&flat_(child));
    }
    text.push_str(close);
    text
}

fn flat_(form: &Syntax) -> String {
    let mut text = meta_pre(form);
    text.push_str(&core_flat(form));
    wrap(form, &text)
}

fn newline_comments(lines: &[String], column: usize) -> String {
    let mut text = String::new();
    for line in lines {
        if line.is_empty() { text.push(';'); } else { text.push_str("; "); text.push_str(line); }
        text.push('\n');
        text.push_str(&spaces(column));
    }
    text
}

fn note_at(form: &Syntax, column: usize) -> String {
    newline_comments(&form.notes, column)
}

fn with_notes(form: &Syntax, column: usize, text: String) -> String {
    let mut result = note_at(form, column);
    result.push_str(&text);
    result
}

fn own(form: &Syntax) -> bool {
    !form.tail.is_empty() || kids(form).iter().any(|child| !child.notes.is_empty())
}

fn deep(form: &Syntax) -> bool {
    own(form) || kids(form).iter().any(|child| deep(child))
}

fn end_notes(form: &Syntax, column: usize) -> String {
    if form.tail.is_empty() { String::new() }
    else { format!("\n{}{}", spaces(column), newline_comments(&form.tail, column)) }
}

fn threadable(form: &Syntax) -> bool {
    match &form.node {
        Node::List(forms) if forms.len() >= 2 => {
            let Node::Sym(head) = &forms[0].node else { return false; };
            head.contains('/') && !head.starts_with("ui/") && !is_keyword(&forms[1])
        }
        _ => false,
    }
}

fn spine(form: &Syntax) -> (Syntax, Vec<Syntax>) {
    if let Node::List(forms) = &form.node {
        if forms.len() >= 2 && threadable(form) {
            let (start, mut steps) = spine(&forms[1]);
            let mut step = form.clone();
            step.node = Node::List(std::iter::once(forms[0].clone()).chain(forms[2..].iter().cloned()).collect());
            steps.push(step);
            return (start, steps);
        }
    }
    (form.clone(), Vec::new())
}

fn wide(form: &Syntax) -> bool {
    fn visit(form: &Syntax, remaining: &mut usize) -> bool {
        if *remaining == 0 { return false; }
        *remaining -= 1;
        kids(form).iter().all(|child| visit(child, remaining))
    }
    let mut remaining = WIDTH;
    !visit(form, &mut remaining)
}

fn pp(form: &Syntax, indent: usize) -> String {
    let pre = meta_pre(form);
    let mut text = pre.clone();
    text.push_str(&core(form, indent + pre.len()));
    wrap(form, &text)
}

fn core(form: &Syntax, indent: usize) -> String {
    let flat = core_flat(form);
    let narrow = !wide(form);
    let fits = |column: usize, limit: usize| narrow && visible_width(&flat) + column <= limit;
    let forms = kids(form);
    let len = forms.len();
    let is_vector = matches!(&form.node, Node::Vec(_));
    let is_map = matches!(&form.node, Node::Map(_));
    let deeply_nested = deep(form);
    let line = |index: usize, column: usize| {
        let child = &forms[index];
        format!("\n{}{}", spaces(column), with_notes(child, column, pp(child, column)))
    };
    match &form.node {
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) => flat,
        Node::Quote(kind, child) => format!("{}{}", prefix(*kind), pp(child, indent + prefix(*kind).len())),
        Node::Vec(_) | Node::Map(_) => {
            if !deeply_nested && (is_vector || fits(indent, WIDTH)) {
                flat
            } else {
                let column = indent + 1;
                let mut pieces = Vec::new();
                let mut i = 0;
                while i < len {
                    let key = &forms[i];
                    if is_map && i + 1 < len {
                        let value = &forms[i + 1];
                        let key_text = pp(key, column);
                        let mut pair = note_at(key, column);
                        pair.push_str(&key_text);
                        if !value.notes.is_empty() {
                            pair.push('\n');
                            pair.push_str(&spaces(column + 2));
                            pair.push_str(&note_at(value, column + 2));
                            pair.push_str(&pp(value, column + 2));
                        } else {
                            pair.push(' ');
                            pair.push_str(&pp(value, column + visible_width(&key_text) + 1));
                        }
                        pieces.push(pair);
                        i += 2;
                    } else {
                        pieces.push(format!("{}{}", note_at(key, column), pp(key, column)));
                        i += 1;
                    }
                }
                let (open, close) = if is_vector { ("[", "]") } else { ("{", "}") };
                let mut text = String::from(open);
                if forms.first().is_some_and(|child| !child.notes.is_empty()) {
                    text.push('\n');
                    text.push_str(&spaces(column));
                }
                text.push_str(&pieces.join(&format!("\n{}", spaces(column))));
                text.push_str(&end_notes(form, column));
                text.push_str(close);
                text
            }
        }
        Node::List(_) if len == 0 => flat,
        Node::List(_) => {
            let call_head = head(form);
            let bind_forms = ["let*", "for", "sum"];
            let is_vec_at = |index: usize| index < len && matches!(&forms[index].node, Node::Vec(_));
            let vec_len = |index: usize| match &forms[index].node { Node::Vec(values) => values.len(), _ => 0 };
            let slots_plain = |n: usize| forms.get(1..n).unwrap_or(&[]).iter().all(|child| child.notes.is_empty());
            let slots_flat = |n: usize| slots_plain(n) && forms.get(1..n).unwrap_or(&[]).iter().all(|child| !deep(child));
            let special = ["graph", "defn", "defmacro"].contains(&call_head);
            let (start, steps) = spine(form);
            if steps.len() >= 3 {
                let column = indent + 4;
                let last = steps.len() - 1;
                let mut text = if start.notes.is_empty() { "(-> ".to_owned() } else { format!("(->\n{}{}", spaces(column), note_at(&start, column)) };
                text.push_str(&pp(&start, column));
                for (i, step) in steps.iter().enumerate() {
                    text.push('\n');
                    text.push_str(&spaces(column));
                    if i == last { text.push_str(&core(step, column)); }
                    else { text.push_str(&note_at(step, column)); text.push_str(&pp(step, column)); }
                }
                text.push(')');
                text
            } else if call_head == "workspace" && len >= 2 && slots_plain(2) {
                let mut text = format!("(workspace {}", flat_(&forms[1]));
                for child in &forms[2..] {
                    text.push_str("\n\n");
                    text.push_str(&spaces(indent + 2));
                    text.push_str(&note_at(child, indent + 2));
                    text.push_str(&pp(child, indent + 2));
                }
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if !deeply_nested
                && (fits(indent, WIDTH) || (fits(0, 44) && !bind_forms.contains(&call_head) && call_head != "fold" && call_head != "scan"))
                && !special
                && !(call_head == "let*" && len > 1 && is_vec_at(1) && vec_len(1) > 2)
            {
                flat
            } else if bind_forms.contains(&call_head) && is_vec_at(1) && len == 3 && slots_plain(2) {
                let column = indent + call_head.len() + 2;
                let mut text = format!("({call_head} {}", binding_vector(&forms[1], column));
                text.push_str(&line(2, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "for" && is_vec_at(1) && len == 5 && is_keyword(&forms[2]) && slots_plain(4) {
                let column = indent + call_head.len() + 2;
                let key = flat_(&forms[2]);
                let mut text = format!("(for {}\n{}{} {}", binding_vector(&forms[1], column), spaces(indent + 2), key, pp(&forms[3], indent + 2 + visible_width(&key) + 1));
                text.push_str(&line(4, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if (call_head == "fold" || call_head == "scan") && is_vec_at(1) && is_vec_at(2) && len == 4 && slots_plain(3) {
                let column = indent + call_head.len() + 2;
                let mut text = format!("({call_head} {}\n{}{}", binding_vector(&forms[1], column), spaces(column), binding_vector(&forms[2], column));
                text.push_str(&line(3, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "if" && len == 4 && forms[0].notes.is_empty() {
                let column = indent + 4;
                let mut text = String::from("(if");
                if !forms[1].notes.is_empty() {
                    text.push('\n'); text.push_str(&spaces(column)); text.push_str(&note_at(&forms[1], column));
                } else { text.push(' '); }
                text.push_str(&pp(&forms[1], column));
                text.push_str(&line(2, column));
                text.push_str(&line(3, column));
                text.push_str(&end_notes(form, column));
                text.push(')');
                text
            } else if (call_head == "graph" || call_head == "defn")
                && (len == 5 || (len == 6 && is_vec_at(4))) && slots_flat(len - 1)
            {
                let mut text = format!("({call_head} {} {} {}", flat_(&forms[1]), flat_(&forms[2]), flat_(&forms[3]));
                if len == 6 { text.push(' '); text.push_str(&flat_(&forms[4])); }
                text.push_str(&line(len - 1, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "defmacro" && len == 4 && slots_flat(3) {
                let mut text = format!("(defmacro {} {}", flat_(&forms[1]), flat_(&forms[2]));
                text.push_str(&line(3, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "fn" && len == 3 && is_vec_at(1) && slots_flat(2) {
                let mut text = format!("(fn {}", flat_(&forms[1]));
                text.push_str(&line(2, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if ((call_head == "cond" && len >= 1) || (call_head == "case" && len >= 2)) && forms[0].notes.is_empty() {
                let column = indent + 2;
                let first = if call_head == "case" { 2 } else { 1 };
                let mut text = if call_head == "case" {
                    let selector = &forms[1];
                    let mut prefix_text = String::from("(case");
                    if !selector.notes.is_empty() {
                        prefix_text.push('\n'); prefix_text.push_str(&spaces(column)); prefix_text.push_str(&note_at(selector, column));
                        prefix_text.push_str(&pp(selector, column));
                    } else { prefix_text.push(' '); prefix_text.push_str(&pp(selector, indent + 6)); }
                    prefix_text
                } else { "(cond".to_owned() };
                let mut i = first;
                while i < len {
                    let test = &forms[i];
                    let test_text = pp(test, column);
                    let mut row = note_at(test, column);
                    row.push_str(&test_text);
                    if i + 1 < len {
                        let value = &forms[i + 1];
                        if !value.notes.is_empty() || test_text.contains('\n') {
                            row.push('\n'); row.push_str(&spaces(column + 2)); row.push_str(&note_at(value, column + 2)); row.push_str(&pp(value, column + 2));
                        } else {
                            row.push(' '); row.push_str(&pp(value, column + visible_width(&test_text) + 1));
                        }
                        i += 2;
                    } else { i += 1; }
                    text.push('\n'); text.push_str(&spaces(column)); text.push_str(&row);
                }
                text.push_str(&end_notes(form, column));
                text.push(')');
                text
            } else {
                let mut units: Vec<Vec<&Syntax>> = Vec::new();
                let mut i = 1;
                while i < len {
                    if i + 1 < len && is_keyword(&forms[i]) {
                        units.push(vec![&forms[i], &forms[i + 1]]);
                        i += 2;
                    } else {
                        units.push(vec![&forms[i]]);
                        i += 1;
                    }
                }
                let head_text = flat_(&forms[0]);
                if units.is_empty() && form.tail.is_empty() {
                    flat
                } else if own(form) {
                    let column = indent + 2;
                    let mut text = String::from("(");
                    if !forms[0].notes.is_empty() { text.push_str(&note_at(&forms[0], indent + 1)); }
                    text.push_str(&head_text);
                    for unit in &units {
                        text.push('\n'); text.push_str(&spaces(column));
                        if unit.len() == 2 {
                            let key = unit[0]; let value = unit[1];
                            text.push_str(&note_at(key, column)); text.push_str(&flat_(key));
                            if !value.notes.is_empty() {
                                text.push('\n'); text.push_str(&spaces(column + 2)); text.push_str(&note_at(value, column + 2)); text.push_str(&pp(value, column + 2));
                            } else {
                                text.push(' '); text.push_str(&pp(value, column + visible_width(&flat_(key)) + 1));
                            }
                        } else {
                            let child = unit[0]; text.push_str(&note_at(child, column)); text.push_str(&pp(child, column));
                        }
                    }
                    text.push_str(&end_notes(form, column)); text.push(')'); text
                } else {
                    let mut column = indent + 1 + visible_width(&head_text) + 1;
                    if column > indent + 18 { column = indent + 4; }
                    let render_unit = |unit: &[&Syntax]| {
                        if unit.len() == 2 {
                            format!("{} {}", flat_(unit[0]), pp(unit[1], column + visible_width(&flat_(unit[0])) + 1))
                        } else { pp(unit[0], column) }
                    };
                    let first = render_unit(&units[0]);
                    let mut text = format!("({head_text}");
                    if column == indent + 4 && units.len() > 1 && visible_width(&first) > 30 { text.push('\n'); text.push_str(&spaces(column)); }
                    else { text.push(' '); }
                    text.push_str(&first);
                    for unit in &units[1..] {
                        text.push('\n'); text.push_str(&spaces(column)); text.push_str(&render_unit(unit));
                    }
                    text.push(')');
                    text
                }
            }
        }
    }
}

fn binding_vector(form: &Syntax, column: usize) -> String {
    let values = kids(form);
    let mut rows = Vec::new();
    let mut i = 0;
    while i < values.len() {
        let name = &values[i];
        let name_text = if deep(name) { pp(name, column + 1) } else { flat_(name) };
        let mut row = note_at(name, column + 1);
        row.push_str(&name_text);
        if i + 1 < values.len() {
            let value = &values[i + 1];
            if !value.notes.is_empty() {
                row.push('\n'); row.push_str(&spaces(column + 3)); row.push_str(&note_at(value, column + 3)); row.push_str(&pp(value, column + 3));
            } else {
                row.push(' '); row.push_str(&pp(value, column + 2 + visible_width(&name_text)));
            }
            i += 2;
        } else { i += 1; }
        rows.push(row);
    }
    let mut text = String::from("[");
    if values.first().is_some_and(|value| !value.notes.is_empty()) {
        text.push('\n'); text.push_str(&spaces(column + 1));
    }
    text.push_str(&rows.join(&format!("\n{}", spaces(column + 1))));
    if !form.tail.is_empty() {
        text.push('\n'); text.push_str(&spaces(column + 1)); text.push_str(&newline_comments(&form.tail, column + 1));
    }
    text.push(']');
    wrap(form, &text)
}

fn finish(text: &str) -> (String, Spans) {
    let bytes = text.as_bytes();
    let mut output = Vec::with_capacity(bytes.len());
    let mut stack: Vec<(syntax::Id, usize)> = Vec::new();
    let mut spans = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            1 => {
                let close = bytes[i..].iter().position(|byte| *byte == 2).map(|offset| i + offset).expect("unterminated printer marker");
                let id = text[i + 1..close].parse::<syntax::Id>().expect("invalid printer form id");
                stack.push((id, output.len()));
                i = close;
            }
            3 => {
                if let Some((id, start)) = stack.pop() {
                    spans.push((id, Span { start: start as i64, finish: output.len() as i64 }));
                }
            }
            byte => output.push(byte),
        }
        i += 1;
    }
    spans.sort_by_key(|(id, _)| *id);
    (String::from_utf8(output).expect("printer produced invalid UTF-8"), spans)
}

pub fn print(forms: &[Syntax]) -> (String, Spans) {
    let _timer = crate::phase_timer::measure("print");
    let mut text = String::new();
    for (index, form) in forms.iter().enumerate() {
        if index > 0 { text.push_str("\n\n"); }
        text.push_str(&note_at(form, 0));
        text.push_str(&pp(form, 0));
    }
    text.push('\n');
    finish(&text)
}

pub fn flat(form: &Syntax) -> String {
    finish(&flat_(form)).0
}

pub fn patch_atoms(current: &(String, Spans), values: &[Syntax]) -> Option<(String, Spans)> {
    let (text, spans) = current;
    let mut by_id = HashMap::with_capacity(spans.len());
    for (id, span) in spans { by_id.insert(*id, span); }
    let mut edits = Vec::with_capacity(values.len());
    for value in values {
        let atom = match &value.node {
            Node::Num(raw) => syntax::number(raw) && syntax::number_value(raw).is_some(),
            Node::Str(_) => true,
            Node::Sym(name) => matches!(name.as_str(), "true" | "false"),
            _ => false,
        };
        let span = if atom && value.meta.is_empty() && value.id != 0 { by_id.get(&value.id).copied() } else { None }?;
        edits.push(((*span).clone(), flat(value)));
    }
    edits.sort_by_key(|(span, _)| span.start);
    let mut valid = true;
    let mut at = 0_i64;
    let mut delta = 0_i64;
    let mut shifts = Vec::with_capacity(edits.len());
    for (span, replacement) in &edits {
        if span.start < at || span.finish <= span.start || span.finish > text.len() as i64 {
            valid = false;
        }
        if span.start < 0 || span.finish < 0 || span.start as usize > text.len() || span.finish as usize > text.len() {
            valid = false;
        } else if text.get(span.start as usize..span.finish as usize).is_none() {
            valid = false;
        }
        at = span.finish;
        delta += replacement.len() as i64 - (span.finish - span.start);
        shifts.push((span.finish, delta));
    }
    if !valid { return None; }

    let mut output = Vec::with_capacity((text.len() as i64 + delta).max(0) as usize);
    at = 0;
    for (span, replacement) in &edits {
        let start = span.start as usize;
        let finish = span.finish as usize;
        output.extend_from_slice(&text.as_bytes()[at as usize..start]);
        output.extend_from_slice(replacement.as_bytes());
        at = finish as i64;
    }
    output.extend_from_slice(&text.as_bytes()[at as usize..]);
    let output = String::from_utf8(output).ok()?;
    let shift = |position: i64| {
        let mut low = 0;
        let mut high = shifts.len();
        while low < high {
            let middle = (low + high) / 2;
            if shifts[middle].0 <= position { low = middle + 1; } else { high = middle; }
        }
        position + if low == 0 { 0 } else { shifts[low - 1].1 }
    };
    let repaired = spans.iter().map(|(id, span)| {
        (*id, Span { start: shift(span.start), finish: shift(span.finish) })
    }).collect();
    Some((output, repaired))
}

#[cfg(test)]
mod tests {
    use super::{flat, float, patch_atoms, print};
    use crate::syntax::{self, parse, Node, Syntax};

    fn printed(source: &str) -> String {
        print(&parse(source).unwrap()).0
    }

    #[test]
    fn printer_golden_layout_and_fixed_points() {
        let cases = [
            ("(sop/box ; the size\n :size [1 2 3] :center [0 0 0])", "(sop/box\n  ; the size\n  :size [1 2 3]\n  :center [0 0 0])\n"),
            ("(let* [a 1 b 2 c 3] (+ a b c))", "(let* [a 1\n       b 2\n       c 3]\n  (+ a b c))\n"),
            ("{:a 1 :b 2}", "{:a 1 :b 2}\n"),
            ("(cond ; first\n (= f 0) big.frame (= f 1) small.pane\n ; otherwise\n :else small.frame)", "(cond\n  ; first\n  (= f 0) big.frame\n  (= f 1) small.pane\n  ; otherwise\n  :else small.frame)\n"),
            ("(case (mod f 3) 0 \"a\" :else \"b\")", "(case (mod f 3) 0 \"a\" :else \"b\")\n"),
            ("(graph g :context sop (sop/box))", "(graph g :context sop\n  (sop/box))\n"),
            ("(defmacro twice [x] `(+ ~x ~x))", "(defmacro twice [x]\n  `(+ ~x ~x))\n"),
            ("(nums 007 0.0000001 1e3 -.5)", "(nums 007 0.0000001 1e3 -0.5)\n"),
            ("(str \"a\\\"b\\\\c\\n\\td\" \"é · ü\")", "(str \"a\\\"b\\\\c\\n\\td\" \"é · ü\")\n"),
        ];
        for (source, expected) in cases {
            let text = printed(source);
            assert_eq!(text, expected, "source: {source}");
            assert_eq!(printed(&text), text);
        }
        assert_eq!(printed("x ; note\n(a)\n"), "x\n\n; note\n(a)\n");
        assert_eq!(printed("(a\n ;\n b)"), "(a\n  ;\n  b)\n");
        assert_eq!(printed("(quote ('a `b ~c ~@d))"), "(quote ('a `b ~c ~@d))\n");
        assert_eq!(printed("(a ^:bypass (sop/subdivide x :iterations 1) ^:x ^:y z)"), "(a ^:bypass (sop/subdivide x :iterations 1) ^:x ^:y z)\n");
        assert_eq!(printed("(if ; test\n (> x 1) (sop/box) (sop/uv_sphere))"), "(if\n    ; test\n    (> x 1)\n    (sop/box)\n    (sop/uv_sphere))\n");
        assert_eq!(printed("(case ; why\n x 0 \"a\" :else \"b\")"), "(case\n  ; why\n  x\n  0 \"a\"\n  :else \"b\")\n");
        assert_eq!(printed("(for [i (range 4)] :skip [1 3] (scene/light :name \"a\"))"), "(for [i (range 4)] :skip [1 3] (scene/light :name \"a\"))\n");
        let long_skip = printed("(for [i (range 4) j (range 3) k (range 5)] :skip [[0 1 2] [1 1 1] [3 2 4]] (scene/light :name \"lamp\" :translate [i j k] :intensity 30))");
        assert!(long_skip.contains('\n'));
        assert_eq!(printed(&long_skip), long_skip);
        assert_eq!(printed("(fold [acc 0] [i (range 10)] ; step\n (+ acc i))"), "(fold [acc 0]\n      [i (range 10)]\n  ; step\n  (+ acc i))\n");
        assert_eq!(printed("(fn [i] ; scale\n (* i 2))"), "(fn [i]\n  ; scale\n  (* i 2))\n");
        assert_eq!(
            printed("(workspace w ; a\n (graph g :context sop (sop/box)) (defn h :context value [(x : float)] x)\n ; trailing\n)"),
            "(workspace w\n\n  ; a\n  (graph g :context sop\n    (sop/box))\n\n  (defn h :context value [(x : float)]\n    x)\n  ; trailing\n  )\n"
        );
        let control = Syntax { node: Node::Str(vec![1, 2, 3]), id: 0, span: rays_diagnostic::Span { start: 0, finish: 0 }, notes: Vec::new(), meta: Vec::new(), tail: Vec::new() };
        let text = flat(&control);
        assert!(!text.bytes().any(|byte| (1..=3).contains(&byte)));
        assert_eq!(parse(&text).unwrap()[0].node, Node::Str(vec![1, 2, 3]));
        let every_byte: Vec<u8> = (0u8..=u8::MAX).collect();
        let every_byte_form = Syntax { node: Node::Str(every_byte.clone()), ..control.clone() };
        let flat_text = flat(&every_byte_form);
        let printed_text = print(&[every_byte_form]).0;
        for text in [flat_text, printed_text] {
            assert!(std::str::from_utf8(text.as_bytes()).is_ok());
            assert!(!text.bytes().any(|byte| (1..=3).contains(&byte)));
            let parsed = parse(&text).unwrap();
            assert!(matches!(&parsed[0].node, Node::Str(bytes) if bytes == &every_byte));
        }
        let escaped = Syntax { node: Node::Str(vec![255, 0]), ..control };
        assert_eq!(flat(&escaped), "\"\\255\0\"");
        assert!(matches!(&parse(&flat(&escaped)).unwrap()[0].node, Node::Str(bytes) if bytes.as_slice() == &[255_u8, 0][..]));
    }

    #[test]
    fn thread_layout_and_spans() {
        let nested = "(sop/c (sop/b (sop/a x :k 1) :j 2))";
        let threaded = "(-> x\n    (sop/a :k 1)\n    (sop/b :j 2)\n    (sop/c))\n";
        assert_eq!(printed(nested), threaded);
        assert_eq!(printed(threaded), threaded);
        assert_eq!(flat(&parse(threaded).unwrap()[0]), nested);
        assert_eq!(printed("(sop/b (sop/a x))"), "(sop/b (sop/a x))\n");
        assert_eq!(printed("(-> x (sop/a) (sop/b))"), "(sop/b (sop/a x))\n");
        assert_eq!(printed("(-> x (sop/a) ; why\n (sop/b) (sop/c))"), "(-> x\n    (sop/a)\n    ; why\n    (sop/b)\n    (sop/c))\n");
        assert_eq!(printed("(ui/floating (ui/tile (ui/viewport (ref scene))))"), "(ui/floating (ui/tile (ui/viewport (ref scene))))\n");

        let (text, spans) = print(&parse("; hi\n(a ^:bypass (b [1 2]) \"s\")").unwrap());
        assert_eq!(text, "; hi\n(a ^:bypass (b [1 2]) \"s\")\n");
        let at = |id| {
            let span = spans.iter().find(|(found, _)| *found == id).unwrap().1.clone();
            text[span.start as usize..span.finish as usize].to_owned()
        };
        assert_eq!(at(0), "(a ^:bypass (b [1 2]) \"s\")");
        assert_eq!(at(1), "a");
        assert_eq!(at(2), "^:bypass (b [1 2])");
        assert_eq!(at(4), "[1 2]");
        assert_eq!(at(7), "\"s\"");
    }

    #[test]
    fn floats_and_patch_atoms() {
        for (value, expected) in [
            (1.0, "1.0"), (-0.5, "-0.5"), (1e-14, "0.00000000000001"), (2e6, "2000000.0"),
            (6.1e-17, "0.000000000000000061"), (1234567.89, "1234567.89"), (0.1, "0.1"),
            (-3.3e-7, "-0.00000033"), (1e21, "1000000000000000000000.0"),
            (f64::NAN, "NaN"), (f64::INFINITY, "Infinity"), (f64::NEG_INFINITY, "-Infinity"),
        ] { assert_eq!(float(value), expected); }
        for value in [1e-14, 2e6, 6.1e-17, 0.30000000000000004, f64::MAX, f64::from_bits(1), -123.456e-9] {
            let text = float(value);
            assert!(syntax::number(&text));
            assert_eq!(syntax::number_value(&text), Some(value));
        }
        assert_eq!(printed("[1e-14 2E+06]"), "[1e-14 2E+06]\n");

        let source = parse("[1 ; a note\n \"é\" true -3]").unwrap();
        let (text, spans) = print(&source);
        let root = &source[0];
        let Node::Vec(kids) = &root.node else { panic!("expected vector"); };
        let number = &kids[0];
        let string = &kids[1];
        let a = Syntax { node: Node::Num("1234".into()), ..number.clone() };
        let b = Syntax { node: Node::Str("λ\n\"".as_bytes().to_vec()), ..string.clone() };
        let patched = patch_atoms(&(text.clone(), spans.clone()), &[b.clone(), a.clone()]).unwrap();
        let expected_root = Syntax { node: Node::Vec(vec![a.clone(), b.clone(), kids[2].clone(), kids[3].clone()]), ..root.clone() };
        let (canonical, expected_spans) = print(&[expected_root]);
        assert_eq!(patched, (canonical, expected_spans));
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[a.clone(), a.clone()]).is_none());
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[Syntax { id: 0, node: Node::Num("2".into()), ..a.clone() }]).is_none());
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[Syntax { node: Node::Num("1e999".into()), ..a.clone() }]).is_none());
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[Syntax { meta: vec!["bypass".into()], ..a.clone() }]).is_none());
        assert!(patch_atoms(&(text, spans), &[root.clone()]).is_none());
    }

    const GOLDEN: &[(&str, &str)] = &[
    ( r####"(sop/box ; the size
 :size [1 2 3] :center [0 0 0])"####,
      r####"(sop/box
  ; the size
  :size [1 2 3]
  :center [0 0 0])"#### ),
    ( r####"(sop/box :size [1 2 3]
 ; where it sits
 :center [0 0 0])"####,
      r####"(sop/box
  :size [1 2 3]
  ; where it sits
  :center [0 0 0])"#### ),
    ( r####"(sop/box :size [1 2 3]
 :center ; on top
 [0 0 0])"####,
      r####"(sop/box
  :size [1 2 3]
  :center
    ; on top
    [0 0 0])"#### ),
    ( r####"(let* [a 1 ; one
 b 2
 ; last
 ] (+ a b))"####,
      r####"(let* [a 1
       ; one
       b 2
       ; last
       ]
  (+ a b))"#### ),
    ( r####"(let* [a 1
 b
 ; the value
 2] (+ a b))"####,
      r####"(let* [a 1
       b
         ; the value
         2]
  (+ a b))"#### ),
    ( r####"(let* [a 1 b 2 c 3] (+ a b c))"####,
      r####"(let* [a 1
       b 2
       c 3]
  (+ a b c))"#### ),
    (r####"[1 ; first
 2 3]"####, r####"[1
 ; first
 2
 3]"####),
    (r####"[1 2 3
 ; done
 ]"####, r####"[1
 2
 3
 ; done
 ]"####),
    ( r####"{:a 1 ; note on b
 :b 2 :c
 ; on value
 3}"####,
      r####"{:a 1
 ; note on b
 :b 2
 :c
   ; on value
   3}"#### ),
    (r####"{:a 1 :b 2}"####, r####"{:a 1 :b 2}"####),
    ( r####"(cond ; first
 (= f 0) big.frame (= f 1) small.pane
 ; otherwise
 :else small.frame)"####,
      r####"(cond
  ; first
  (= f 0) big.frame
  (= f 1) small.pane
  ; otherwise
  :else small.frame)"####
    ),
    ( r####"(case (mod f 3) 0 "#b0680f" 1 "#285f77" :else "#6b50ae")"####,
      r####"(case (mod f 3) 0 "#b0680f" 1 "#285f77" :else "#6b50ae")"#### ),
    ( r####"(case ; why
 x 0 "a" :else "b")"####,
      r####"(case
  ; why
  x
  0 "a"
  :else "b")"#### ),
    ( r####"(if ; test
 (> x 1) (sop/box) (sop/uv_sphere))"####,
      r####"(if
    ; test
    (> x 1)
    (sop/box)
    (sop/uv_sphere))"#### ),
    ( r####"(if (> x 1) (sop/box :size [1 1 1] :center [0 0 0] :rotation [0 0.5 0]) (sop/uv_sphere :radius 2 :segments 12 :rings 8))"####,
      r####"(if (> x 1)
    (sop/box :size [1 1 1] :center [0 0 0] :rotation [0 0.5 0])
    (sop/uv_sphere :radius 2 :segments 12 :rings 8))"####
    ),
    ( r####"(fold [acc 0] [i (range 10)] ; step
 (+ acc i))"####,
      r####"(fold [acc 0]
      [i (range 10)]
  ; step
  (+ acc i))"#### ),
    ( r####"(fold [shape (sop/box)] [i (range steps)] (sop/merge shape (sop/transform shape :rotate [0 0.1 0] :uniform_scale 0.9)))"####,
      r####"(fold [shape (sop/box)]
      [i (range steps)]
  (sop/merge shape (sop/transform shape :rotate [0 0.1 0] :uniform_scale 0.9)))"####
    ),
    ( r####"(for [i (range 3) j (range 4)] (sop/uv_sphere :radius 0.1 :center [i j 0] :segments 8 :rings 4 :rotation [0 1 0]))"####,
      r####"(for [i (range 3)
      j (range 4)]
  (sop/uv_sphere :radius 0.1 :center [i j 0] :segments 8 :rings 4 :rotation [0 1 0]))"####
    ),
    ( r####"(sum [k (range 5)] (/ 1 (+ k 1)))"####,
      r####"(sum [k (range 5)] (/ 1 (+ k 1)))"#### ),
    (r####"(fn [x y] (* x y))"####, r####"(fn [x y] (* x y))"####),
    (r####"(fn [i] ; scale
 (* i 2))"####, r####"(fn [i]
  ; scale
  (* i 2))"####),
    ( r####"(defmacro twice [x] `(+ ~x ~x))"####,
      r####"(defmacro twice [x]
  `(+ ~x ~x))"#### ),
    ( r####"(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))"####,
      r####"(defmacro radial [i n body]
  `(sop/merge (for [~i (range ~n)]
                (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))"####
    ),
    ( r####"(defmacro spl [xs] `(sop/merge ~@xs))"####,
      r####"(defmacro spl [xs]
  `(sop/merge ~@xs))"#### ),
    ( r####"(graph g :context sop (sop/box))"####,
      r####"(graph g :context sop
  (sop/box))"#### ),
    ( r####"(graph g :context sop [(n : int 3)] ; body
 (sop/box))"####,
      r####"(graph g :context sop [(n : int 3)]
  ; body
  (sop/box))"#### ),
    ( r####"(graph g :context sop [(n : int 3)] (sop/box)
 ; end
)"####,
      r####"(graph g :context sop [(n : int 3)]
  (sop/box)
  ; end
  )"#### ),
    ( r####"(nums 2.0 .5 1. 0.50 -3 -0.25 1.500000)"####,
      r####"(nums 2.0 0.5 1.0 0.5 -3 -0.25 1.5)"#### ),
    (r####"(str "a\"b\\c\n\td" "é · ü")"####, r####"(str "a\"b\\c\n\td" "é · ü")"####),
    ( r####"(sop/merge aaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccc ddddddddddddddddddddddd)"####,
      r####"(sop/merge aaaaaaaaaaaaaaaaaaaaaaaa
           bbbbbbbbbbbbbbbbbbbbbbbbbbbbb
           cccccccccccccccccccccccccc
           ddddddddddddddddddddddd)"####
    ),
    ( r####"(some-long-function-name-here-x aaaaaaaaaaaaaaaaaaaaa :key bbbbbbbbbbbbbbbbbbbbbbbb :other (ccccccccccccccc dddddddddddddd))"####,
      r####"(some-long-function-name-here-x aaaaaaaaaaaaaaaaaaaaa
    :key bbbbbbbbbbbbbbbbbbbbbbbb
    :other (ccccccccccccccc dddddddddddddd))"####
    ),
    ( r####"(sop/uv_sphere :radius [0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45] :segments 12)"####,
      r####"(sop/uv_sphere :radius [0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45]
               :segments 12)"####
    ),
    ( r####"(let* [{:keys [shape y]} st [l m r] xs
 p (sop/transform shape :translate [0 y 0] :rotate [0 (* f 0.3) 0] :uniform_scale 0.5)] p)"####,
      r####"(let* [{:keys [shape y]} st
       [l m r] xs
       p (sop/transform shape
                        :translate [0 y 0]
                        :rotate [0 (* f 0.3) 0]
                        :uniform_scale 0.5)]
  p)"####
    ),
    ( r####"(workspace w
 ; a
 ; b

 ;; c
 (graph g :context sop (sop/box)) (defn h :context value [(x : float)] x)
 ; trailing
)"####,
      r####"(workspace w

  ; a
  ; b
  ; c
  (graph g :context sop
    (sop/box))

  (defn h :context value [(x : float)]
    x)
  ; trailing
  )"####
    ),
    (r####"(vec3 1 2 3)"####, r####"(vec3 1 2 3)"####),
    (r####"()"####, r####"()"####),
    ( r####"(sop/merge (for [r (range rings) m (range moons)] (let* [radius (+ 0.6 (* r 0.35))] (sop/uv_sphere :radius radius))))"####,
      r####"(sop/merge (for [r (range rings)
                 m (range moons)]
             (let* [radius (+ 0.6 (* r 0.35))] (sop/uv_sphere :radius radius))))"####
    ),
    ];

    fn root() -> std::path::PathBuf { std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..") }

    #[test]
    fn ocaml_goldens_are_fixed_points() {
        for (source, expected) in GOLDEN {
            let text = printed(source);
            assert_eq!(text, format!("{expected}\n"), "source: {source}");
            assert_eq!(printed(&text), text);
        }
    }

    #[test]
    fn checked_in_workspaces_print_as_fixed_points() {
        let cases = root().join("specification/workspace/cases");
        let mut count = 0;
        for entry in std::fs::read_dir(&cases).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "lisp") {
                let source = std::fs::read_to_string(&path).unwrap();
                assert_eq!(printed(&source), source, "{path:?}");
                count += 1;
            }
        }
        assert!(count > 0);
        for entry in std::fs::read_dir(root().join("sketches")).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "rays") {
                let source = std::fs::read_to_string(&path).unwrap();
                let once = printed(&source);
                assert_eq!(printed(&once), once, "{path:?}");
            }
        }
    }
}
