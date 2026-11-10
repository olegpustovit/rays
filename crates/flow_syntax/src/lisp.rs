use crate::syntax::{self, Node, QuoteKind, Syntax};
use crate::arena::{SyntaxBuilder, SyntaxId};
use rays_diagnostic::{self as diagnostic, Span};
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

pub fn quote_string(bytes: &[u8]) -> String {
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
    for meta in form.meta() {
        text.push_str("^:");
        text.push_str(meta);
        text.push(' ');
    }
    text
}

fn kids(form: &Syntax) -> &[SyntaxId] {
    form.children()
}

fn is_keyword(form: &Syntax) -> bool {
    matches!(&form.node, Node::Kw(_))
}

fn head(form: &Syntax) -> &str {
    syntax::head(form).unwrap_or("")
}

fn core_flat(form: &Syntax) -> String {
    match &form.node {
        Node::Sym(symbol) => form.name(*symbol).to_owned(),
        Node::Kw(keyword) => format!(":{}", form.name(*keyword)),
        Node::Num(raw) => number(form.name(*raw)),
        Node::Str(text) => quote_string(form.bytes(*text)),
        Node::List(_) => join_form(form, "(", ")"),
        Node::Vec(_) => join_form(form, "[", "]"),
        Node::Map(_) => join_form(form, "{", "}"),
        Node::Quote(kind, child) => format!("{}{}", prefix(*kind), flat_(&form.child(*child))),
    }
}

fn join_form(form: &Syntax, open: &str, close: &str) -> String {
    let mut text = String::from(open);
    for (index, child) in kids(form).iter().enumerate() {
        if index > 0 { text.push(' '); }
        text.push_str(&flat_(&form.child(*child)));
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
    newline_comments(&form.notes(), column)
}

fn with_notes(form: &Syntax, column: usize, text: String) -> String {
    let mut result = note_at(form, column);
    result.push_str(&text);
    result
}

fn own(form: &Syntax) -> bool {
    !form.tail().is_empty() || kids(form).iter().any(|child| !form.arena().notes(*child).is_empty())
}

fn deep(form: &Syntax) -> bool {
    own(form) || kids(form).iter().any(|child| deep(&form.child(*child)))
}

fn end_notes(form: &Syntax, column: usize) -> String {
    if form.tail().is_empty() { String::new() }
    else { format!("\n{}{}", spaces(column), newline_comments(&form.tail(), column)) }
}

fn threadable(form: &Syntax) -> bool {
    match &form.node {
        Node::List(range) if form.range(*range).len() >= 2 => {
            let forms = form.range(*range);
            let Node::Sym(head) = form.arena().row(forms[0]).node else { return false; };
            let head = form.name(head);
            head.contains('/') && !head.starts_with("ui/") && !is_keyword(&form.child(forms[1]))
        }
        _ => false,
    }
}

fn spine(form: &Syntax) -> (Syntax, Vec<Syntax>) {
    let mut cursor = form.clone();
    let mut depth = 0;
    while threadable(&cursor) {
        depth += 1;
        cursor = cursor.child(cursor.children()[1]);
    }
    if depth < 3 { return (form.clone(), Vec::new()); }
    fn collect(form: &Syntax, builder: &mut SyntaxBuilder, steps: &mut Vec<SyntaxId>) -> SyntaxId {
        let forms = form.children();
        if forms.len() >= 2 && threadable(form) {
            let start = collect(&form.child(forms[1]), builder, steps);
            let children: Vec<_> = std::iter::once(forms[0]).chain(forms[2..].iter().copied())
                .map(|id| builder.imported(&form.child(id)).expect("printer source imported")).collect();
            let range = builder.range(&children);
            steps.push(builder.push(form.id, Node::List(range), form.span.clone(), form.trivia().cloned()));
            start
        } else {
            builder.imported(form).expect("printer source imported")
        }
    }
    let mut builder = SyntaxBuilder::new();
    builder.import(form);
    let mut steps = Vec::new();
    let start = collect(form, &mut builder, &mut steps);
    let arena = builder.freeze();
    (arena.handle(start), steps.into_iter().map(|id| arena.handle(id)).collect())
}

fn wide(form: &Syntax) -> bool {
    fn visit(form: &Syntax, remaining: &mut usize) -> bool {
        if *remaining == 0 { return false; }
        *remaining -= 1;
        kids(form).iter().all(|child| visit(&form.child(*child), remaining))
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
        let child = &form.child(forms[index]);
        format!("\n{}{}", spaces(column), with_notes(child, column, pp(child, column)))
    };
    match &form.node {
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) => flat,
        Node::Quote(kind, child) => format!("{}{}", prefix(*kind), pp(&form.child(*child), indent + prefix(*kind).len())),
        Node::Vec(_) | Node::Map(_) => {
            if !deeply_nested && (is_vector || fits(indent, WIDTH)) {
                flat
            } else {
                let column = indent + 1;
                let mut pieces = Vec::new();
                let mut i = 0;
                while i < len {
                    let key = &form.child(forms[i]);
                    if is_map && i + 1 < len {
                        let value = &form.child(forms[i + 1]);
                        let key_text = pp(key, column);
                        let mut pair = note_at(key, column);
                        pair.push_str(&key_text);
                        if !value.notes().is_empty() {
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
                if forms.first().is_some_and(|child| !form.arena().notes(*child).is_empty()) {
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
            let is_vec_at = |index: usize| index < len && matches!(&form.arena().row(forms[index]).node, Node::Vec(_));
            let vec_len = |index: usize| match &form.arena().row(forms[index]).node { Node::Vec(values) => form.range(*values).len(), _ => 0 };
            let slots_plain = |n: usize| forms.get(1..n).unwrap_or(&[]).iter().all(|child| form.arena().notes(*child).is_empty());
            let slots_flat = |n: usize| slots_plain(n) && forms.get(1..n).unwrap_or(&[]).iter().all(|child| !deep(&form.child(*child)));
            let special = ["graph", "defn", "defmacro"].contains(&call_head);
            let (start, steps) = spine(form);
            if steps.len() >= 3 {
                let column = indent + 4;
                let last = steps.len() - 1;
                let mut text = if start.notes().is_empty() { "(-> ".to_owned() } else { format!("(->\n{}{}", spaces(column), note_at(&start, column)) };
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
                let mut text = format!("(workspace {}", flat_(&form.child(forms[1])));
                for id in &forms[2..] {
                    let child = &form.child(*id);
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
                let mut text = format!("({call_head} {}", binding_vector(&form.child(forms[1]), column));
                text.push_str(&line(2, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "for" && is_vec_at(1) && len == 5 && is_keyword(&form.child(forms[2])) && slots_plain(4) {
                let column = indent + call_head.len() + 2;
                let key = flat_(&form.child(forms[2]));
                let mut text = format!("(for {}\n{}{} {}", binding_vector(&form.child(forms[1]), column), spaces(indent + 2), key, pp(&form.child(forms[3]), indent + 2 + visible_width(&key) + 1));
                text.push_str(&line(4, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if (call_head == "fold" || call_head == "scan") && is_vec_at(1) && is_vec_at(2) && len == 4 && slots_plain(3) {
                let column = indent + call_head.len() + 2;
                let mut text = format!("({call_head} {}\n{}{}", binding_vector(&form.child(forms[1]), column), spaces(column), binding_vector(&form.child(forms[2]), column));
                text.push_str(&line(3, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "if" && len == 4 && form.arena().notes(forms[0]).is_empty() {
                let column = indent + 4;
                let mut text = String::from("(if");
                if !form.arena().notes(forms[1]).is_empty() {
                    text.push('\n'); text.push_str(&spaces(column)); text.push_str(&note_at(&form.child(forms[1]), column));
                } else { text.push(' '); }
                text.push_str(&pp(&form.child(forms[1]), column));
                text.push_str(&line(2, column));
                text.push_str(&line(3, column));
                text.push_str(&end_notes(form, column));
                text.push(')');
                text
            } else if (call_head == "graph" || call_head == "defn")
                && (len == 5 || (len == 6 && is_vec_at(4))) && slots_flat(len - 1)
            {
                let mut text = format!("({call_head} {} {} {}", flat_(&form.child(forms[1])), flat_(&form.child(forms[2])), flat_(&form.child(forms[3])));
                if len == 6 { text.push(' '); text.push_str(&flat_(&form.child(forms[4]))); }
                text.push_str(&line(len - 1, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "defmacro" && len == 4 && slots_flat(3) {
                let mut text = format!("(defmacro {} {}", flat_(&form.child(forms[1])), flat_(&form.child(forms[2])));
                text.push_str(&line(3, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if call_head == "fn" && len == 3 && is_vec_at(1) && slots_flat(2) {
                let mut text = format!("(fn {}", flat_(&form.child(forms[1])));
                text.push_str(&line(2, indent + 2));
                text.push_str(&end_notes(form, indent + 2));
                text.push(')');
                text
            } else if ((call_head == "cond" && len >= 1) || (call_head == "case" && len >= 2)) && form.arena().notes(forms[0]).is_empty() {
                let column = indent + 2;
                let first = if call_head == "case" { 2 } else { 1 };
                let mut text = if call_head == "case" {
                    let selector = &form.child(forms[1]);
                    let mut prefix_text = String::from("(case");
                    if !selector.notes().is_empty() {
                        prefix_text.push('\n'); prefix_text.push_str(&spaces(column)); prefix_text.push_str(&note_at(selector, column));
                        prefix_text.push_str(&pp(selector, column));
                    } else { prefix_text.push(' '); prefix_text.push_str(&pp(selector, indent + 6)); }
                    prefix_text
                } else { "(cond".to_owned() };
                let mut i = first;
                while i < len {
                    let test = &form.child(forms[i]);
                    let test_text = pp(test, column);
                    let mut row = note_at(test, column);
                    row.push_str(&test_text);
                    if i + 1 < len {
                        let value = &form.child(forms[i + 1]);
                        if !value.notes().is_empty() || test_text.contains('\n') {
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
                let mut units: Vec<&[SyntaxId]> = Vec::new();
                let mut i = 1;
                while i < len {
                    if i + 1 < len && is_keyword(&form.child(forms[i])) {
                        units.push(&forms[i..i + 2]);
                        i += 2;
                    } else {
                        units.push(&forms[i..i + 1]);
                        i += 1;
                    }
                }
                let head_text = flat_(&form.child(forms[0]));
                if units.is_empty() && form.tail().is_empty() {
                    flat
                } else if own(form) {
                    let column = indent + 2;
                    let mut text = String::from("(");
                    if !form.arena().notes(forms[0]).is_empty() { text.push_str(&note_at(&form.child(forms[0]), indent + 1)); }
                    text.push_str(&head_text);
                    for unit in &units {
                        text.push('\n'); text.push_str(&spaces(column));
                        if unit.len() == 2 {
                            let key = &form.child(unit[0]); let value = &form.child(unit[1]);
                            text.push_str(&note_at(key, column)); text.push_str(&flat_(key));
                            if !value.notes().is_empty() {
                                text.push('\n'); text.push_str(&spaces(column + 2)); text.push_str(&note_at(value, column + 2)); text.push_str(&pp(value, column + 2));
                            } else {
                                text.push(' '); text.push_str(&pp(value, column + visible_width(&flat_(key)) + 1));
                            }
                        } else {
                            let child = &form.child(unit[0]); text.push_str(&note_at(child, column)); text.push_str(&pp(child, column));
                        }
                    }
                    text.push_str(&end_notes(form, column)); text.push(')'); text
                } else {
                    let mut column = indent + 1 + visible_width(&head_text) + 1;
                    if column > indent + 18 { column = indent + 4; }
                    let render_unit = |unit: &[SyntaxId]| {
                        if unit.len() == 2 {
                            format!("{} {}", flat_(&form.child(unit[0])), pp(&form.child(unit[1]), column + visible_width(&flat_(&form.child(unit[0]))) + 1))
                        } else { pp(&form.child(unit[0]), column) }
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
        let name = &form.child(values[i]);
        let name_text = if deep(name) { pp(name, column + 1) } else { flat_(name) };
        let mut row = note_at(name, column + 1);
        row.push_str(&name_text);
        if i + 1 < values.len() {
            let value = &form.child(values[i + 1]);
            if !value.notes().is_empty() {
                row.push('\n'); row.push_str(&spaces(column + 3)); row.push_str(&note_at(value, column + 3)); row.push_str(&pp(value, column + 3));
            } else {
                row.push(' '); row.push_str(&pp(value, column + 2 + visible_width(&name_text)));
            }
            i += 2;
        } else { i += 1; }
        rows.push(row);
    }
    let mut text = String::from("[");
    if values.first().is_some_and(|value| !form.arena().notes(*value).is_empty()) {
        text.push('\n'); text.push_str(&spaces(column + 1));
    }
    text.push_str(&rows.join(&format!("\n{}", spaces(column + 1))));
    if !form.tail().is_empty() {
        text.push('\n'); text.push_str(&spaces(column + 1)); text.push_str(&newline_comments(&form.tail(), column + 1));
    }
    text.push(']');
    wrap(form, &text)
}

fn finish(text: &str, mut spans: Option<&mut Spans>) -> String {
    let bytes = text.as_bytes();
    let mut output = Vec::with_capacity(bytes.len());
    let mut stack: Vec<(syntax::Id, usize)> = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            1 => {
                let close = bytes[i..].iter().position(|byte| *byte == 2).map(|offset| i + offset).expect("unterminated printer marker");
                if spans.is_some() {
                    let id = text[i + 1..close].parse::<syntax::Id>().expect("invalid printer form id");
                    stack.push((id, output.len()));
                }
                i = close;
            }
            3 => {
                if let Some(spans) = spans.as_mut() && let Some((id, start)) = stack.pop() {
                    // The checked source printer validates the complete length before exposing these spans.
                    spans.push((id, Span { start: start as u32, finish: output.len() as u32 }));
                }
            }
            byte => output.push(byte),
        }
        i += 1;
    }
    String::from_utf8(output).expect("printer produced invalid UTF-8")
}

fn render(forms: &[Syntax], spans: Option<&mut Spans>) -> String {
    let _timer = crate::phase_timer::measure("print");
    let mut text = String::new();
    for (index, form) in forms.iter().enumerate() {
        if index > 0 { text.push_str("\n\n"); }
        text.push_str(&note_at(form, 0));
        text.push_str(&pp(form, 0));
    }
    text.push('\n');
    finish(&text, spans)
}

/// Formatted source and compact byte spans; oversized generated source is a diagnostic, not a panic.
pub fn print(forms: &[Syntax]) -> Result<(String, Spans), diagnostic::Diagnostic> {
    let mut spans = Vec::new();
    let text = render(forms, Some(&mut spans));
    diagnostic::check_source_len(text.len())?;
    spans.sort_by_key(|(id, _)| *id);
    Ok((text, spans))
}

/// Formatted display/hash text without compact source metadata or span-vector work.
pub fn text(forms: &[Syntax]) -> String {
    render(forms, None)
}

pub fn flat(form: &Syntax) -> String {
    finish(&flat_(form), None)
}

pub fn flat_id(id: SyntaxId, builder: &SyntaxBuilder) -> String {
    fn append(id: SyntaxId, builder: &SyntaxBuilder, text: &mut String) {
        if let Some(trivia) = builder.trivia(id) {
            for name in &trivia.meta {
                text.push_str("^:"); text.push_str(name); text.push(' ');
            }
        }
        match builder.row(id).node {
            Node::Sym(name) => text.push_str(builder.name(name)),
            Node::Kw(name) => { text.push(':'); text.push_str(builder.name(name)); }
            Node::Num(name) => text.push_str(&number(builder.name(name))),
            Node::Str(bytes) => text.push_str(&quote_string(builder.raw_bytes(bytes))),
            Node::Quote(kind, child) => { text.push_str(prefix(kind)); append(child, builder, text); }
            node @ (Node::List(range) | Node::Vec(range) | Node::Map(range)) => {
                let (open, close) = match node { Node::List(_) => ('(', ')'), Node::Vec(_) => ('[', ']'), _ => ('{', '}') };
                text.push(open);
                for (index, child) in builder.range_ids(range).iter().enumerate() {
                    if index > 0 { text.push(' '); }
                    append(*child, builder, text);
                }
                text.push(close);
            }
        }
    }
    let mut text = String::new();
    append(id, builder, &mut text);
    text
}

pub fn flat_with_meta(form: &Syntax, meta: &[String]) -> String {
    let mut text = String::new();
    for name in meta {
        text.push_str("^:");
        text.push_str(name);
        text.push(' ');
    }
    text.push_str(&core_flat(form));
    finish(&text, None)
}

pub fn patch_atoms(current: &(String, Spans), values: &[Syntax]) -> Option<(String, Spans)> {
    let (text, spans) = current;
    let mut by_id = HashMap::with_capacity(spans.len());
    for (id, span) in spans { by_id.insert(*id, span); }
    let mut edits = Vec::with_capacity(values.len());
    for value in values {
        let atom = match &value.node {
            Node::Num(raw) => syntax::number(value.name(*raw)) && syntax::number_value(value.name(*raw)).is_some(),
            Node::Str(_) => true,
            Node::Sym(name) => matches!(value.name(*name), "true" | "false"),
            _ => false,
        };
        let span = if atom && value.meta().is_empty() && value.id != 0 { by_id.get(&value.id).copied() } else { None }?;
        edits.push(((*span).clone(), flat(value)));
    }
    edits.sort_by_key(|(span, _)| span.start);
    let mut valid = true;
    let mut at = 0_u32;
    let mut delta = 0_i64;
    let mut shifts = Vec::with_capacity(edits.len());
    for (span, replacement) in &edits {
        if span.start < at || span.finish <= span.start || span.finish as usize > text.len() {
            valid = false;
        }
        if span.start as usize > text.len() || span.finish as usize > text.len() {
            valid = false;
        } else if text.get(span.start as usize..span.finish as usize).is_none() {
            valid = false;
        }
        at = span.finish;
        delta += replacement.len() as i64 - (i64::from(span.finish) - i64::from(span.start));
        shifts.push((span.finish, delta));
    }
    if !valid { return None; }

    let output_len = usize::try_from(text.len() as i64 + delta).ok()?;
    diagnostic::check_source_len(output_len).ok()?;
    let mut output = Vec::with_capacity(output_len);
    at = 0;
    for (span, replacement) in &edits {
        let start = span.start as usize;
        let finish = span.finish as usize;
        output.extend_from_slice(&text.as_bytes()[at as usize..start]);
        output.extend_from_slice(replacement.as_bytes());
        at = u32::try_from(finish).ok()?;
    }
    output.extend_from_slice(&text.as_bytes()[at as usize..]);
    let output = String::from_utf8(output).ok()?;
    let shift = |position: u32| {
        let mut low = 0;
        let mut high = shifts.len();
        while low < high {
            let middle = (low + high) / 2;
            if shifts[middle].0 <= position { low = middle + 1; } else { high = middle; }
        }
        u32::try_from(i64::from(position) + if low == 0 { 0 } else { shifts[low - 1].1 }).ok()
    };
    let repaired = spans.iter().map(|(id, span)| {
        Some((*id, Span { start: shift(span.start)?, finish: shift(span.finish)? }))
    }).collect::<Option<_>>()?;
    Some((output, repaired))
}

#[cfg(test)]
mod tests {
    use super::{flat, float, patch_atoms, print};
    use crate::syntax::{self, parse, Node, Syntax};
    use crate::arena::{SyntaxBuilder, SyntaxId};

    fn string(bytes: &[u8]) -> Syntax {
        let mut builder = SyntaxBuilder::new();
        let bytes = builder.bytes(bytes);
        let id = builder.push(0, Node::Str(bytes), rays_diagnostic::Span { start: 0, finish: 0 }, None);
        builder.freeze().handle(id)
    }

    fn rewrite(form: &Syntax, update: &mut dyn FnMut(&mut SyntaxBuilder, SyntaxId) -> SyntaxId) -> Syntax {
        let mut builder = SyntaxBuilder::new();
        let id = builder.import(form);
        let id = update(&mut builder, id);
        builder.freeze().handle(id)
    }

    fn number(form: &Syntax, text: &str) -> Syntax {
        rewrite(form, &mut |builder, id| {
            let name = builder.intern(text);
            let row = builder.row(id).clone();
            builder.push(row.id, Node::Num(name), row.span, builder.trivia(id).cloned())
        })
    }

    fn printed(source: &str) -> String {
        print(&parse(source).unwrap()).unwrap().0
    }

    #[test]
    fn text_only_rendering_matches_checked_source_and_preserves_authored_ids() {
        let mut forms = parse("; note\n^:bypass (sop/transform (sop/box) :translate [1 2 3])\n'(a \"\\001日本\")").unwrap();
        forms[0] = rewrite(&forms[0], &mut |builder, id| { let span = builder.row(id).span.clone(); builder.set_location(id, syntax::LIBRARY_BASE + 7, span); id });
        forms[1] = rewrite(&forms[1], &mut |builder, id| { let span = builder.row(id).span.clone(); builder.set_location(id, -5, span); id });
        let (text, spans) = print(&forms).unwrap();
        assert_eq!(super::text(&forms), text);
        assert!(spans.iter().any(|(id, _)| *id == syntax::LIBRARY_BASE + 7));
        assert!(spans.iter().any(|(id, _)| *id == -5));
        assert!(!text.bytes().any(|byte| (1..=3).contains(&byte)));
        let form = parse("^:old [^:child x]").unwrap().remove(0);
        assert_eq!(super::flat_with_meta(&form, &["parent".into()]), "^:parent [^:child x]");
        assert_eq!(super::flat_with_meta(&form, &[]), "[^:child x]");
        let mut builder = SyntaxBuilder::new();
        for source in ["^:old [^:child x]", "'(a :b 001 -0.0 \"\\255日本\")"] {
            let form = parse(source).unwrap().remove(0);
            let id = builder.import(&form);
            assert_eq!(super::flat_id(id, &builder), flat(&form));
        }
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
            ("(nums 007 0.0000001 1e3 -.5)", "(nums 007 1e-7 1e3 -0.5)\n"),
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
        let control = string(&[1, 2, 3]);
        let text = flat(&control);
        assert!(!text.bytes().any(|byte| (1..=3).contains(&byte)));
        let parsed = parse(&text).unwrap();
        assert!(matches!(parsed[0].node, Node::Str(bytes) if parsed[0].bytes(bytes) == &[1, 2, 3]));
        let every_byte: Vec<u8> = (0u8..=u8::MAX).collect();
        let every_byte_form = string(&every_byte);
        let flat_text = flat(&every_byte_form);
        let printed_text = print(&[every_byte_form]).unwrap().0;
        for text in [flat_text, printed_text] {
            assert!(std::str::from_utf8(text.as_bytes()).is_ok());
            assert!(!text.bytes().any(|byte| (1..=3).contains(&byte)));
            let parsed = parse(&text).unwrap();
            assert!(matches!(parsed[0].node, Node::Str(bytes) if parsed[0].bytes(bytes) == every_byte.as_slice()));
        }
        let escaped = string(&[255, 0]);
        assert_eq!(flat(&escaped), "\"\\255\0\"");
        let parsed = parse(&flat(&escaped)).unwrap();
        assert!(matches!(parsed[0].node, Node::Str(bytes) if parsed[0].bytes(bytes) == &[255_u8, 0]));
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
        assert_eq!(flat(&parse("(-> x (sop/a))").unwrap()[0]), "(sop/a x)");
        assert_eq!(flat(&parse("(-> x)").unwrap()[0]), "x");
        assert_eq!(printed("(-> x (sop/a) ^:bypass (sop/b) (sop/c))"), "(-> x\n    (sop/a)\n    ^:bypass (sop/b)\n    (sop/c))\n");
        assert_eq!(printed("(-> ; start\n x (sop/a) (sop/b) (sop/c))"), "(->\n    ; start\n    x\n    (sop/a)\n    (sop/b)\n    (sop/c))\n");
        assert_eq!(printed("(* (+ (- a 1) 2) 3)"), "(* (+ (- a 1) 2) 3)\n");
        assert_eq!(printed("(sop/d (sop/c (sop/b (sop/a :k 1))))"), "(-> (sop/a :k 1)\n    (sop/b)\n    (sop/c)\n    (sop/d))\n");
        let (text, spans) = print(&parse("(let* [g nope] (sop/c (sop/b (sop/a g :k 1))))").unwrap()).unwrap();
        assert!(!text.is_empty() && !spans.is_empty());
        assert!(spans.iter().all(|(_, span)| span.finish as usize <= text.len()));
        for invalid in ["(->)", "(-> x 5)", "(-> x g)"] {
            assert!(parse(invalid).is_err());
        }
        let long = "(for [i (range 4) j (range 3) k (range 5)] :skip [[0 1 2] [1 1 1] [3 2 4]] (scene/light :name \"lamp\" :translate [i j k] :intensity 30))";
        assert_eq!(flat(&parse(&printed(long)).unwrap()[0]), long);

        let (text, spans) = print(&parse("; hi\n(a ^:bypass (b [1 2]) \"s\")").unwrap()).unwrap();
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
        let ids: Vec<_> = spans.iter().map(|(id, _)| *id).collect();
        assert_eq!(ids.len(), 8);
        assert!(ids.windows(2).all(|pair| pair[0] < pair[1]));
        let nested = format!("(all {})", (0..30).map(|i| format!("(f{i} :k {i})")).collect::<Vec<_>>().join(" "));
        let forms = parse(&nested).unwrap();
        let (text, spans) = print(&forms).unwrap();
        let span = &spans.iter().find(|(id, _)| *id == 0).unwrap().1;
        assert!(span.start == 0 && span.finish as usize == text.len() - 1 && text.contains('\n'));
        assert_eq!(flat(&forms[0]), nested);
    }

    #[test]
    fn floats_and_patch_atoms() {
        for (value, expected) in [
            (1.0, "1.0"), (-0.5, "-0.5"), (1e-14, "1e-14"), (2e6, "2000000.0"),
            (6.1e-17, "6.1e-17"), (1234567.89, "1234567.89"), (0.1, "0.1"),
            (-3.3e-7, "-3.3e-7"), (1e21, "1e21"),
            (f64::NAN, "NaN"), (f64::INFINITY, "Infinity"), (f64::NEG_INFINITY, "-Infinity"),
        ] { assert_eq!(float(value), expected); }
        for value in [1e-14, 2e6, 6.1e-17, 0.30000000000000004, f64::MAX, f64::from_bits(1), -123.456e-9] {
            let text = float(value);
            assert!(syntax::number(&text));
            assert_eq!(syntax::number_value(&text), Some(value));
            assert_eq!(printed(&text), format!("{text}\n"));
        }
        assert_eq!(printed("[1e-14 2E+06]"), "[1e-14 2E+06]\n");

        let source = parse("[1 ; a note\n \"é\" true -3]").unwrap();
        let (text, spans) = print(&source).unwrap();
        let root = &source[0];
        let Node::Vec(range) = root.node else { panic!("expected vector"); };
        let kids = root.range(range);
        let a = number(&root.child(kids[0]), "1234");
        let b = rewrite(&root.child(kids[1]), &mut |builder, id| {
            let bytes = builder.bytes("λ\n\"".as_bytes());
            let row = builder.row(id).clone();
            builder.push(row.id, Node::Str(bytes), row.span, builder.trivia(id).cloned())
        });
        let patched = patch_atoms(&(text.clone(), spans.clone()), &[b.clone(), a.clone()]).unwrap();
        let expected_root = rewrite(root, &mut |builder, id| {
            let children = [builder.import(&a), builder.import(&b), builder.import(&root.child(kids[2])), builder.import(&root.child(kids[3]))];
            let range = builder.range(&children);
            let row = builder.row(id).clone();
            builder.push(row.id, Node::Vec(range), row.span, builder.trivia(id).cloned())
        });
        let (canonical, expected_spans) = print(&[expected_root]).unwrap();
        assert_eq!(patched, (canonical, expected_spans));
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[a.clone(), a.clone()]).is_none());
        let zero = rewrite(&number(&a, "2"), &mut |builder, id| { let span = builder.row(id).span.clone(); builder.set_location(id, 0, span); id });
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[zero]).is_none());
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[number(&a, "1e999")]).is_none());
        let meta = rewrite(&a, &mut |builder, id| { builder.trivia_mut(id).meta.push("bypass".into()); id });
        assert!(patch_atoms(&(text.clone(), spans.clone()), &[meta]).is_none());
        assert!(patch_atoms(&(text, spans), &[root.clone()]).is_none());
    }

    fn root() -> std::path::PathBuf { std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..") }

    #[test]
    fn checked_in_workspaces_print_as_fixed_points() {
        let cases = root().join("specification/workspace/cases");
        let comments = |source: &str| {
            let mut comments: Vec<_> = source.lines().filter_map(|line| {
                line.find(';').map(|at| line[at..].trim().trim_start_matches(';').trim().to_owned())
            }).collect();
            comments.sort();
            comments
        };
        let mut count = 0;
        let fixtures = [cases.join("bloom.lisp"), cases.join("variations.lisp")].into_iter().chain(
            ["facade", "garland", "kit", "orrery", "rosette", "sunflower", "tiles", "tree", "tunnel", "wave"]
                .into_iter().map(|name| root().join("sketches").join(format!("ws_{name}.rays")))
        );
        for path in fixtures {
                let source = std::fs::read_to_string(&path).unwrap();
                assert_eq!(printed(&source), source, "{path:?}");
                assert_eq!(printed(&printed(&source)), source, "{path:?}");
                assert!(source.lines().all(|line| !line.ends_with(' ')), "{path:?}");
                count += 1;
        }
        assert_eq!(count, 12);
        for entry in std::fs::read_dir(root().join("sketches")).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "rays") {
                let source = std::fs::read_to_string(&path).unwrap();
                let once = printed(&source);
                assert_eq!(printed(&once), once, "{path:?}");
                assert_eq!(comments(&source), comments(&once), "{path:?}: print lost a comment");
            }
        }
        for source in [
            "(graph g [a ; first\n b] -> geometry (sop/box))",
            "(graph g {:kind :sop} [a ; first\n b] -> geometry (sop/box))",
            "(defn f [x ; the x\n y] -> number (+ x y))",
            "(defmacro m [a ; why\n b] `(+ ~a ~b))",
            "(fn [x ; arg\n ] (+ x 1))",
            "(f ^:bypass ; between\n (sop/box))",
            "(f ' ; quoted\n a)",
            "(-> x ; one\n (sop/a) ; two\n (sop/b) (sop/c) ; end\n )",
            "(let* [[a ; in a pattern\n b] xs] a)",
            "(workspace w ; a\n (graph g [] -> geometry (sop/box)) ; tail\n )\n; after",
        ] {
            let once = printed(source);
            assert_eq!(printed(&once), once, "{source}");
            assert_eq!(comments(source), comments(&once), "{source}: print lost a comment");
        }
    }
}
