use flow_syntax::text_lexer::{self, Kind as LexKind, Word};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Comment,
    Str,
    Num,
    Kw,
    Meta,
    Head,
    Sym,
    Open(usize),
    Close(usize),
    Unmatched,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: Kind,
    pub start: usize,
    pub stop: usize,
}

pub const BINDERS: &[&str] = &["let", "let*", "for", "fold", "scan", "sum", "state", "fn"];

fn closer_of(open: char) -> char {
    match open {
        '(' => ')',
        '[' => ']',
        _ => '}',
    }
}

pub fn lex(text: &str) -> (Vec<Token>, Vec<(usize, usize)>, Vec<(char, usize)>) {
    let raw = text_lexer::lex(text);
    let mut tokens = Vec::new();
    let mut pairs = Vec::new();
    let mut stack: Vec<(char, usize, usize)> = Vec::new();
    let mut head_next = false;
    let mut index = 0;
    while index < raw.len() {
        let token = &raw[index];
        let start = token.start as usize;
        let stop = token.stop as usize;
        match &token.kind {
            LexKind::Open(open) => {
                let at = tokens.len();
                tokens.push(Token { kind: Kind::Open(stack.len()), start, stop });
                stack.push((*open, start, at));
                head_next = *open == '(';
            }
            LexKind::Close(close) => {
                let kind = if stack.last().is_some_and(|(open, _, _)| closer_of(*open) == *close) {
                    match stack.pop() {
                        Some((_, at, _)) => {
                            pairs.push((at, start));
                            Kind::Close(stack.len())
                        }
                        None => Kind::Unmatched,
                    }
                } else {
                    Kind::Unmatched
                };
                tokens.push(Token { kind, start, stop });
                head_next = false;
            }
            LexKind::Comment(_) => tokens.push(Token { kind: Kind::Comment, start, stop }),
            LexKind::String(_) => {
                tokens.push(Token { kind: Kind::Str, start, stop });
                head_next = false;
            }
            LexKind::Meta(_) => {
                tokens.push(Token { kind: if head_next { Kind::Head } else { Kind::Meta }, start, stop });
                head_next = false;
            }
            LexKind::Word(word) => {
                let kind = if head_next {
                    Kind::Head
                } else {
                    match word {
                        Word::Symbol(_) => Kind::Sym,
                        Word::Keyword(_) => Kind::Kw,
                        Word::Number(_) => Kind::Num,
                    }
                };
                tokens.push(Token { kind, start, stop });
                head_next = false;
            }
            LexKind::Prefix(_) => {
                let merged_stop = if let Some(next) = raw.get(index + 1) {
                    let mergeable = matches!(
                        &next.kind,
                        LexKind::Word(_) | LexKind::Meta(_) | LexKind::Prefix(_)
                    );
                    if mergeable && next.start == token.stop {
                        index += 1;
                        next.stop as usize
                    } else {
                        stop
                    }
                } else {
                    stop
                };
                tokens.push(Token {
                    kind: if head_next { Kind::Head } else { Kind::Sym },
                    start,
                    stop: merged_stop,
                });
                head_next = false;
            }
            LexKind::End => {}
        }
        index += 1;
    }
    for &(_, _, index) in &stack {
        tokens[index].kind = Kind::Unmatched;
    }
    let open_stack = stack.iter().rev().map(|&(open, at, _)| (open, at)).collect();
    (tokens, pairs, open_stack)
}

const BODY_FORM_CANDIDATES: &[&str] = &[
    "workspace", "graph", "defn", "defnode", "defmacro", "let*", "for", "fold", "scan",
    "sum", "state", "if", "fn", "cond", "case",
];

fn is_body_form(name: &[u8]) -> bool {
    BODY_FORM_CANDIDATES
        .iter()
        .any(|form| form.as_bytes() == name && crate::workspace::special_forms().contains(form))
}

fn column(text: &str, at: usize) -> usize {
    let bytes = text.as_bytes();
    let at = at.min(bytes.len());
    let start = bytes[..at].iter().rposition(|&byte| byte == b'\n').map_or(0, |i| i + 1);
    bytes[start..at].iter().filter(|&&byte| byte & 0xc0 != 0x80).count()
}

fn tokens_before(text: &str, caret: usize) -> Vec<Token> {
    let end = caret.min(text.len());
    let mut prefix_end = end;
    while !text.is_char_boundary(prefix_end) {
        prefix_end -= 1;
    }
    let (mut tokens, _, _) = lex(&text[..prefix_end]);
    if prefix_end != end {
        let source_tokens = lex(text).0;
        if let Some(partial) = source_tokens.iter().find(|token| token.start < end && token.stop > end) {
            if let Some(existing) = tokens.iter_mut().find(|token| token.start == partial.start) {
                existing.stop = end;
            } else {
                let mut partial = partial.clone();
                partial.stop = end;
                tokens.push(partial);
            }
        }
    }
    tokens
}

pub fn indent(text: &str, caret: usize) -> String {
    let tokens = tokens_before(text, caret);
    let caret = caret.min(text.len());
    let bytes = text.as_bytes();
    let after = |from: usize| tokens.iter().find(|token| token.start > from);
    let open = tokens.iter().rev().find_map(|token| {
        if token.kind != Kind::Unmatched {
            return None;
        }
        match bytes.get(token.start) {
            Some(b'(') => Some(('(', token.start)),
            Some(b'[') => Some(('[', token.start)),
            Some(b'{') => Some(('{', token.start)),
            _ => None,
        }
    });
    let width = match open {
        None => 0,
        Some((open, at)) if open != '(' => column(text, at) + 1,
        Some((_, at)) => {
            let line_end = bytes
                .get(at..caret)
                .unwrap_or_default()
                .iter()
                .position(|&byte| byte == b'\n')
                .map_or(caret, |offset| at + offset);
            let head = after(at).filter(|token| token.kind == Kind::Head);
            match head {
                Some(head) => {
                    let head_end = head.stop.min(caret);
                    let name = text.as_bytes().get(head.start..head_end).unwrap_or_default();
                    if is_body_form(name) {
                        column(text, at) + 2
                    } else {
                        after(head.stop)
                            .filter(|arg| arg.start < line_end)
                            .map_or(column(text, at) + 1, |arg| column(text, arg.start))
                    }
                }
                None => column(text, at) + 1,
            }
        }
    };
    " ".repeat(width)
}

pub fn word(text: &str, token: &Token) -> String {
    text.get(token.start..token.stop).unwrap_or_default().to_owned()
}

pub fn index_at(tokens: &[Token], at: usize) -> Option<usize> {
    tokens.iter().position(|token| token.start == at)
}

pub fn items(tokens: &[Token], i: usize) -> Vec<usize> {
    let mut items = Vec::new();
    let mut depth = 0;
    for (j, token) in tokens.iter().enumerate().skip(i.saturating_add(1)) {
        match token.kind {
            Kind::Open(_) => {
                if depth == 0 {
                    items.push(j);
                }
                depth += 1;
            }
            Kind::Close(_) => {
                if depth == 0 {
                    break;
                }
                depth -= 1;
            }
            _ if depth == 0 => items.push(j),
            _ => {}
        }
    }
    items
}

pub fn item_stop(tokens: &[Token], j: usize) -> usize {
    let Some(token) = tokens.get(j) else { return 0 };
    let Kind::Open(depth) = token.kind else { return token.stop };
    tokens
        .iter()
        .skip(j + 1)
        .find(|token| token.kind == Kind::Close(depth))
        .map_or_else(|| tokens.last().map_or(token.stop, |last| last.stop), |close| close.stop)
}

pub fn head_before(text: &str, tokens: &[Token], i: usize) -> Option<String> {
    let mut depth = 0;
    for j in (0..i.min(tokens.len())).rev() {
        match tokens[j].kind {
            Kind::Close(_) => depth += 1,
            Kind::Open(_) if depth > 0 => depth -= 1,
            Kind::Open(_) => return None,
            Kind::Head if depth == 0 => return Some(word(text, &tokens[j])),
            _ => {}
        }
    }
    None
}

fn snippet(text: &str, tokens: &[Token], j: usize) -> String {
    let Some(token) = tokens.get(j) else { return String::new() };
    let start = token.start;
    let stop = item_stop(tokens, j).min(start.saturating_add(48));
    let bytes = text.as_bytes();
    let Some(mut value) = bytes.get(start..stop) else { return String::new() };
    while !value.is_empty() && std::str::from_utf8(value).is_err() {
        value = &value[..value.len() - 1];
    }
    String::from_utf8_lossy(value).replace('\n', " ")
}

pub fn bound(
    text: &str,
    tokens: &[Token],
    limit: usize,
) -> (Vec<(String, String)>, Vec<String>, Vec<String>) {
    let mut names = Vec::new();
    let mut defns = Vec::new();
    let mut graphs = Vec::new();
    for i in 0..tokens.len() {
        if tokens[i].start >= limit {
            continue;
        }
        match tokens[i].kind {
            Kind::Head if i + 1 < tokens.len() && tokens[i + 1].kind == Kind::Sym => {
                let head = word(text, &tokens[i]);
                if head == "defn" || head == "defmacro" {
                    defns.push(word(text, &tokens[i + 1]));
                } else if head == "graph" {
                    graphs.push(word(text, &tokens[i + 1]));
                }
            }
            Kind::Open(_) | Kind::Unmatched if text.as_bytes().get(tokens[i].start) == Some(&b'[') => {
                match head_before(text, tokens, i).as_deref() {
                    Some(head) if BINDERS.contains(&head) => {
                        let items = items(tokens, i);
                        let all = head == "fn";
                        for (position, &j) in items.iter().enumerate() {
                            if tokens[j].kind == Kind::Sym
                                && tokens[j].start < limit
                                && (all || position % 2 == 0)
                            {
                                let description = if all || position + 1 >= items.len() {
                                    format!("parameter of {head}")
                                } else {
                                    format!("{head} binding · {}", snippet(text, tokens, items[position + 1]))
                                };
                                names.push((word(text, &tokens[j]), description));
                            }
                        }
                    }
                    Some("graph" | "defn") => {
                        for j in items(tokens, i) {
                            if matches!(tokens[j].kind, Kind::Open(_) | Kind::Unmatched)
                                && tokens.get(j + 1).is_some_and(|next| next.kind == Kind::Head)
                            {
                                names.push((
                                    word(text, &tokens[j + 1]),
                                    format!("input · {}", snippet(text, tokens, j)),
                                ));
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
    (names, defns, graphs)
}

pub fn kw_value(text: &str, tokens: &[Token], i: usize, kw: &str) -> Option<String> {
    let items = items(tokens, i);
    for pair in items.windows(2) {
        let (key, value) = (pair[0], pair[1]);
        if tokens[key].kind == Kind::Kw && word(text, &tokens[key]) == kw {
            return (tokens[value].kind == Kind::Sym).then(|| word(text, &tokens[value]));
        }
    }
    None
}

pub fn context_of(text: &str, tokens: &[Token], open_stack: &[(char, usize)]) -> String {
    for &(_, at) in open_stack.iter().rev() {
        let Some(i) = index_at(tokens, at) else { continue };
        if i + 1 < tokens.len() && tokens[i + 1].kind == Kind::Head {
            let head = word(text, &tokens[i + 1]);
            if head == "graph" || head == "defn" {
                if let Some(context) = kw_value(text, tokens, i, ":context") { return context; }
                let depth = match tokens[i].kind { Kind::Open(depth) => depth, _ => 0 };
                for token in tokens.iter().skip(i + 2).take_while(|token| token.kind != Kind::Close(depth)) {
                    if token.kind != Kind::Head { continue; }
                    let name = word(text, token);
                    let context = crate::op::find_ref(&[], &name, &flow_syntax::context::value()).map(|operator| operator.ctx)
                        .or_else(|| flow_syntax::context::of_qualified(&name));
                    if let Some(context) = context.filter(|context| *context != flow_syntax::context::value()) {
                        return flow_syntax::context::name(context).to_owned();
                    }
                }
                return "value".to_owned();
            }
        }
    }
    "sop".to_owned()
}

#[cfg(test)]
mod tests {
    use super::{bound, context_of, head_before, indent, index_at, item_stop, items, kw_value, lex, word, Kind};

    #[test]
    fn brackets_nesting_and_byte_offsets() {
        let text = "(a [b] c) ] {x";
        let (tokens, pairs, stack) = lex(text);
        assert_eq!(pairs, vec![(3, 5), (0, 8)]);
        assert_eq!(stack, vec![('{', 12)]);
        assert!(matches!(tokens[0].kind, Kind::Open(0)));
        assert!(matches!(tokens[2].kind, Kind::Open(1)));
        assert_eq!(tokens[7].kind, Kind::Unmatched);
        assert_eq!(tokens[8].kind, Kind::Unmatched);

        let (tokens, _, _) = lex("é (f π)");
        let pi = tokens.iter().find(|token| word("é (f π)", token) == "π").unwrap();
        assert_eq!((pi.start, pi.stop), (6, 8));
        assert_eq!(index_at(&tokens, 6), Some(3));
    }

    #[test]
    fn nested_items_and_heads() {
        let text = "(outer a [b (inner c)] d)";
        let (tokens, _, _) = lex(text);
        let top = items(&tokens, 0);
        assert_eq!(top.iter().map(|&i| tokens[i].start).collect::<Vec<_>>(), vec![1, 7, 9, 23]);
        assert_eq!(item_stop(&tokens, top[2]), 22);
        let inner = index_at(&tokens, 13).unwrap();
        assert_eq!(head_before(text, &tokens, inner + 1).as_deref(), Some("inner"));
        assert_eq!(head_before(text, &tokens, top[3]).as_deref(), Some("outer"));
    }

    #[test]
    fn editor_readers_keep_bindings_keywords_and_context() {
        let text = "(workspace w (graph g :context scene [(depth : int 5)] (let* [ring (sop/box) n 2] (fn [x y] x))))";
        let (tokens, _, stack) = lex(text);
        let (names, _defns, graphs) = bound(text, &tokens, text.len());
        assert!(names.iter().any(|(name, _)| name == "depth"));
        assert!(names.iter().any(|(name, _)| name == "ring"));
        assert!(names.iter().any(|(name, _)| name == "n"));
        assert!(names.iter().any(|(name, _)| name == "x"));
        let definitions = "(defn sample :context value [] 1) (defmacro other [x] x)";
        let (definition_tokens, _, _) = lex(definitions);
        let (_, defn_names, _) = bound(definitions, &definition_tokens, definitions.len());
        assert_eq!(defn_names, vec!["sample".to_owned(), "other".to_owned()]);
        assert_eq!(graphs, vec!["g"]);

        let graph = index_at(&tokens, 13).unwrap();
        assert_eq!(kw_value(text, &tokens, graph, ":context").as_deref(), Some("scene"));
        assert_eq!(context_of(text, &tokens, &stack), "sop");
        let graph_text = "(graph g :context scene [x";
        let (graph_tokens, _, graph_stack) = lex(graph_text);
        assert_eq!(context_of(graph_text, &graph_tokens, &graph_stack), "scene");
    }

    #[test]
    fn indentation_handles_unicode_and_partial_byte_carets() {
        assert_eq!(indent("(f é", 5), "   ");
        assert_eq!(indent("(let* [a 1]", 4), " ");
        assert_eq!(indent("(let* [a 1]", 5), "  ");
        assert_eq!(indent("(f é", 4), "   ");
        assert_eq!(indent("(sop/merge\nx", 12), " ");
    }
}

#[cfg(test)]
mod inferred_context_tests {
    #[test]
    fn completion_infers_context_through_let_without_an_annotation() {
        let source = "(workspace example (graph picture (let [width (frame/width)] (draw/circle [0 0] 3";
        let (tokens, _, stack) = super::lex(source);
        assert_eq!(super::context_of(source, &tokens, &stack), "draw");
    }
}
