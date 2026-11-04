use rays_diagnostic::{self as diagnostic, Diagnostic, Span};

pub type Id = i64;
pub const LIBRARY_BASE: Id = 1 << 40;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QuoteKind {
    Plain,
    Quasi,
    Unquote,
    Splice,
}

#[derive(Clone)]
pub struct Syntax {
    pub id: Id,
    pub node: Node,
    pub span: Span,
    /// Comments, metadata and trailing comments: absent for the common form that has none.
    pub trivia: Option<Box<Trivia>>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Trivia {
    pub notes: Vec<String>,
    pub meta: Vec<String>,
    pub tail: Vec<String>,
}

impl Syntax {
    pub fn notes(&self) -> &[String] {
        self.trivia.as_ref().map_or(&[], |t| &t.notes)
    }
    pub fn meta(&self) -> &[String] {
        self.trivia.as_ref().map_or(&[], |t| &t.meta)
    }
    pub fn tail(&self) -> &[String] {
        self.trivia.as_ref().map_or(&[], |t| &t.tail)
    }
    pub fn notes_mut(&mut self) -> &mut Vec<String> {
        &mut self.trivia.get_or_insert_with(Default::default).notes
    }
    pub fn meta_mut(&mut self) -> &mut Vec<String> {
        &mut self.trivia.get_or_insert_with(Default::default).meta
    }
    pub fn tail_mut(&mut self) -> &mut Vec<String> {
        &mut self.trivia.get_or_insert_with(Default::default).tail
    }
    /// The trivia of the three lists, or none when all are empty.
    pub fn trivia_of(notes: Vec<String>, meta: Vec<String>, tail: Vec<String>) -> Option<Box<Trivia>> {
        if notes.is_empty() && meta.is_empty() && tail.is_empty() { None } else { Some(Box::new(Trivia { notes, meta, tail })) }
    }
}

impl std::fmt::Debug for Syntax {
    // the field layout the printed form always had, so structure keys and goldens do not move
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Syntax").field("id", &self.id).field("node", &self.node).field("span", &self.span).field("notes", &self.notes()).field("meta", &self.meta()).field("tail", &self.tail()).finish()
    }
}

impl PartialEq for Syntax {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.node == other.node && self.span == other.span && self.notes() == other.notes() && self.meta() == other.meta() && self.tail() == other.tail()
    }
}
impl Eq for Syntax {}

/// The children of a list, vector or map form: one shared run, so cloning a `Syntax` is O(1).
/// Reads go through `Deref` as a `Vec`; a write (`DerefMut`) copies the run first when it is shared.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct Children(std::sync::Arc<Vec<Syntax>>);

impl Children {
    pub fn new(items: Vec<Syntax>) -> Self {
        Children(std::sync::Arc::new(items))
    }
    /// Whether two children share one run (cheap identity).
    pub fn ptr_eq(a: &Children, b: &Children) -> bool {
        std::sync::Arc::ptr_eq(&a.0, &b.0)
    }
}
impl std::ops::Deref for Children {
    type Target = Vec<Syntax>;
    fn deref(&self) -> &Vec<Syntax> {
        &self.0
    }
}
impl std::ops::DerefMut for Children {
    fn deref_mut(&mut self) -> &mut Vec<Syntax> {
        std::sync::Arc::make_mut(&mut self.0)
    }
}
impl std::fmt::Debug for Children {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.0, f)
    }
}
impl From<Vec<Syntax>> for Children {
    fn from(items: Vec<Syntax>) -> Self {
        Children::new(items)
    }
}
impl From<&[Syntax]> for Children {
    fn from(items: &[Syntax]) -> Self {
        Children::new(items.to_vec())
    }
}
impl FromIterator<Syntax> for Children {
    fn from_iter<I: IntoIterator<Item = Syntax>>(iter: I) -> Self {
        Children::new(iter.into_iter().collect())
    }
}
impl IntoIterator for Children {
    type Item = Syntax;
    type IntoIter = std::vec::IntoIter<Syntax>;
    fn into_iter(self) -> Self::IntoIter {
        std::sync::Arc::try_unwrap(self.0).unwrap_or_else(|shared| (*shared).clone()).into_iter()
    }
}
impl<'a> IntoIterator for &'a Children {
    type Item = &'a Syntax;
    type IntoIter = std::slice::Iter<'a, Syntax>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Node {
    Sym(String),
    Kw(String),
    Num(String),
    Str(Vec<u8>),
    List(Children),
    Vec(Children),
    Map(Children),
    Quote(QuoteKind, std::sync::Arc<Syntax>),
}

use crate::text_lexer;

const DEPTH_LIMIT: usize = 256;

pub fn number(text: &str) -> bool {
    text_lexer::number(text)
}

pub fn number_value(text: &str) -> Option<f64> {
    match text {
        "NaN" => Some(f64::NAN),
        "Infinity" => Some(f64::INFINITY),
        "-Infinity" => Some(f64::NEG_INFINITY),
        _ => {
            let (mantissa, exponent) = text
                .find(|c| c == 'e' || c == 'E')
                .map_or((text, ""), |at| text.split_at(at));
            let normalized = if let Some(value) = mantissa.strip_prefix("-.") {
                format!("-0.{value}{exponent}")
            } else if let Some(value) = mantissa.strip_prefix('.') {
                format!("0.{value}{exponent}")
            } else if mantissa.ends_with('.') {
                format!("{mantissa}0{exponent}")
            } else {
                text.to_owned()
            };
            normalized.parse::<f64>().ok().filter(|value| value.is_finite())
        }
    }
}

pub fn separator(c: char) -> bool {
    text_lexer::separator(c)
}

fn fail(source: &str, start: i64, finish: i64, code: &str, message: &str) -> Diagnostic {
    diagnostic::error(
        code.to_owned(),
        message.to_owned(),
        Some(Span { start, finish }),
        Some(diagnostic::position_of_offset(source, start)),
        None,
        Vec::new(),
        Vec::new(),
    )
}

#[derive(Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Clone)]
enum TokenKind {
    Open(char),
    Close(char),
    Prefix(QuoteKind),
    Meta(String),
    Comment(String),
    Word(Node),
    End,
}

fn tokenize(source: &str) -> Result<Vec<Token>, Diagnostic> {
    let raw = text_lexer::lex(source);
    let first_issue = raw
        .iter()
        .filter_map(|token| token.issue.as_ref())
        .find(|problem| problem.message.starts_with("Control byte "))
        .or_else(|| raw.iter().find_map(|token| token.issue.as_ref()));
    if let Some(problem) = first_issue {
        return Err(fail(source, problem.start, problem.finish, &problem.code, &problem.message));
    }
    Ok(raw
        .into_iter()
        .map(|token| {
            let kind = match token.kind {
                text_lexer::Kind::Open(c) => TokenKind::Open(c),
                text_lexer::Kind::Close(c) => TokenKind::Close(c),
                text_lexer::Kind::Prefix('\'') => TokenKind::Prefix(QuoteKind::Plain),
                text_lexer::Kind::Prefix('`') => TokenKind::Prefix(QuoteKind::Quasi),
                text_lexer::Kind::Prefix('~') => TokenKind::Prefix(QuoteKind::Unquote),
                text_lexer::Kind::Prefix('@') => TokenKind::Prefix(QuoteKind::Splice),
                text_lexer::Kind::Prefix(_) => unreachable!(),
                text_lexer::Kind::Meta(name) => TokenKind::Meta(name),
                text_lexer::Kind::Comment(text) => TokenKind::Comment(text),
                text_lexer::Kind::String(text) => TokenKind::Word(Node::Str(text)),
                text_lexer::Kind::Word(text_lexer::Word::Symbol(text)) => TokenKind::Word(Node::Sym(text)),
                text_lexer::Kind::Word(text_lexer::Word::Keyword(text)) => TokenKind::Word(Node::Kw(text)),
                text_lexer::Kind::Word(text_lexer::Word::Number(text)) => TokenKind::Word(Node::Num(text)),
                text_lexer::Kind::End => TokenKind::End,
            };
            Token { kind, span: Span { start: token.start, finish: token.stop } }
        })
        .collect())
}

pub fn parse(source: &str) -> Result<Vec<Syntax>, Diagnostic> {
    let _timer = crate::phase_timer::measure("parse");
    parse_inner(source)
}

fn parse_inner(source: &str) -> Result<Vec<Syntax>, Diagnostic> {
    let tokens = tokenize(source)?;
    let mut at = 0;
    let mut next_id = 0;

    fn fresh(next_id: &mut Id) -> Id {
        let id = *next_id;
        *next_id += 1;
        id
    }
    fn peek(tokens: &[Token], at: usize) -> &Token {
        &tokens[at]
    }
    fn take(tokens: &[Token], at: &mut usize) -> Token {
        let token = tokens[*at].clone();
        if !matches!(&token.kind, TokenKind::End) {
            *at += 1;
        }
        token
    }
    fn comments(tokens: &[Token], at: &mut usize) -> Vec<String> {
        let mut result = Vec::new();
        while let TokenKind::Comment(text) = &peek(tokens, *at).kind {
            result.push(text.clone());
            *at += 1;
        }
        result
    }
    fn parse_form(
        source: &str,
        tokens: &[Token],
        at: &mut usize,
        next_id: &mut Id,
        notes: Vec<String>,
        depth: usize,
    ) -> Result<Syntax, Diagnostic> {
        let token = take(tokens, at);
        if depth > DEPTH_LIMIT {
            return Err(fail(
                source,
                token.span.start,
                token.span.finish,
                "E_DEPTH",
                &format!("S-expression nesting exceeds {DEPTH_LIMIT} forms"),
            ));
        }
        match token.kind {
            TokenKind::Word(node) => Ok(Syntax { id: fresh(next_id), node, span: token.span, trivia: Syntax::trivia_of(notes, Vec::new(), Vec::new()) }),
            TokenKind::Open(c) => { let id = fresh(next_id); parse_group(source, tokens, at, next_id, token.span.clone(), c, id, notes, depth) }
            TokenKind::Prefix(kind) => {
                let id = fresh(next_id);
                let mut notes = notes;
                notes.extend(comments(tokens, at));
                let inner = parse_form(source, tokens, at, next_id, Vec::new(), depth + 1)?;
                Ok(Syntax {
                    id,
                    span: Span { start: token.span.start, finish: inner.span.finish },
                    node: Node::Quote(kind, std::sync::Arc::new(inner)),
                    trivia: Syntax::trivia_of(notes, Vec::new(), Vec::new()),
                })
            }
            TokenKind::Meta(name) => {
                let mut notes = notes;
                notes.extend(comments(tokens, at));
                if matches!(&peek(tokens, *at).kind, TokenKind::End | TokenKind::Close(_)) {
                    return Err(fail(source, token.span.start, token.span.finish, "E_UNEXPECTED", "Metadata needs a form after it"));
                }
                let mut inner = parse_form(source, tokens, at, next_id, notes, depth + 1)?;
                inner.meta_mut().insert(0, name);
                inner.span.start = token.span.start;
                Ok(inner)
            }
            TokenKind::Close(c) => Err(fail(source, token.span.start, token.span.finish, "E_UNEXPECTED", &format!("Unexpected {c:?}"))),
            TokenKind::Comment(_) | TokenKind::End => Err(fail(source, token.span.start, token.span.finish, "E_UNEXPECTED", "Expected a form")),
        }
    }
    fn parse_group(
        source: &str,
        tokens: &[Token],
        at: &mut usize,
        next_id: &mut Id,
        opening: Span,
        delimiter: char,
        id: Id,
        notes: Vec<String>,
        depth: usize,
    ) -> Result<Syntax, Diagnostic> {
        let expected = match delimiter { '(' => ')', '[' => ']', _ => '}' };
        let mut items = Vec::new();
        loop {
            let leading = comments(tokens, at);
            let token = peek(tokens, *at);
            match &token.kind {
                TokenKind::Close(actual) if *actual == expected => {
                    let closing = take(tokens, at);
                    let span = Span { start: opening.start, finish: closing.span.finish };
                    let tail = leading;
                    let node = match delimiter {
                        '(' => Node::List(items.into()),
                        '[' => Node::Vec(items.into()),
                        _ => Node::Map(items.into()),
                    };
                    let whole = Syntax { id, node, span: span.clone(), trivia: Syntax::trivia_of(notes.clone(), Vec::new(), tail.clone()) };
                    return match &whole.node {
                        Node::List(forms) if matches!(forms.first().map(|form| &form.node), Some(Node::Sym(head)) if head == "->") => {
                            if forms.len() == 1 {
                                Err(fail(source, opening.start, opening.finish, "E_THREAD", "-> needs a value to thread"))
                            } else if forms.len() == 2 {
                                let mut value = forms[1].clone();
                                *value.notes_mut() = [notes, value.notes().to_vec()].concat();
                                Ok(value)
                            } else {
                                let mut result = forms[1].clone();
                                let steps = &forms[2..];
                                if depth + steps.len() > DEPTH_LIMIT {
                                    return Err(fail(source, opening.start, opening.finish, "E_DEPTH", &format!("S-expression nesting exceeds {DEPTH_LIMIT} forms")));
                                }
                                for (index, step) in steps.iter().enumerate() {
                                    let Node::List(parts) = &step.node else {
                                        return Err(fail(source, step.span.start, step.span.finish, "E_THREAD", "A -> step is a call, as in (sop/normals :cusp_angle 0.6)"));
                                    };
                                    if parts.is_empty() || !matches!(&parts[0].node, Node::Sym(_)) {
                                        return Err(fail(source, step.span.start, step.span.finish, "E_THREAD", "A -> step is a call, as in (sop/normals :cusp_angle 0.6)"));
                                    }
                                    let mut next_parts = Vec::with_capacity(parts.len() + 1);
                                    next_parts.push(parts[0].clone());
                                    next_parts.push(result);
                                    next_parts.extend_from_slice(&parts[1..]);
                                    result = step.clone();
                                    result.node = Node::List(next_parts.into());
                                    if index + 1 == steps.len() {
                                        result.id = id;
                                        *result.notes_mut() = [notes.clone(), result.notes().to_vec()].concat();
                                        *result.tail_mut() = [tail.clone(), result.tail().to_vec()].concat();
                                        result.span = span.clone();
                                    }
                                }
                                Ok(result)
                            }
                        }
                        _ => Ok(whole),
                    };
                }
                TokenKind::Close(actual) => {
                    let line = diagnostic::position_of_offset(source, opening.start).line;
                    return Err(fail(
                        source,
                        token.span.start,
                        token.span.finish,
                        "E_UNEXPECTED",
                        &format!("Expected {expected:?} to close the {delimiter:?} on line {line}, found {actual:?}"),
                    ));
                }
                TokenKind::End => return Err(fail(source, opening.start, opening.finish, "E_UNCLOSED", &format!("This {delimiter:?} is never closed"))),
                _ => items.push(parse_form(source, tokens, at, next_id, leading, depth + 1)?),
            }
        }
    }

    let mut forms: Vec<Syntax> = Vec::new();
    loop {
        let leading = comments(&tokens, &mut at);
        if matches!(&peek(&tokens, at).kind, TokenKind::End) {
            if !leading.is_empty() {
                if let Some(last) = forms.last_mut() {
                    if matches!(&last.node, Node::List(_) | Node::Vec(_) | Node::Map(_)) {
                        last.tail_mut().extend(leading);
                    }
                }
            }
            return Ok(forms);
        }
        forms.push(parse_form(source, &tokens, &mut at, &mut next_id, leading, 0)?);
    }
}

pub fn make(node: Node) -> Syntax {
    make_with(node, Vec::new(), Vec::new())
}

pub fn make_with(node: Node, notes: Vec<String>, meta: Vec<String>) -> Syntax {
    Syntax { id: 0, node, span: Span { start: 0, finish: 0 }, trivia: Syntax::trivia_of(notes, meta, Vec::new()) }
}

pub fn equal(a: &Syntax, b: &Syntax) -> bool {
    a.notes() == b.notes()
        && a.meta() == b.meta()
        && a.tail() == b.tail()
        && match (&a.node, &b.node) {
            (Node::Sym(x), Node::Sym(y)) | (Node::Kw(x), Node::Kw(y)) | (Node::Num(x), Node::Num(y)) => x == y,
            (Node::Str(x), Node::Str(y)) => x == y,
            (Node::List(x), Node::List(y)) | (Node::Vec(x), Node::Vec(y)) | (Node::Map(x), Node::Map(y)) => x.len() == y.len() && x.iter().zip(y).all(|(x, y)| equal(x, y)),
            (Node::Quote(k, x), Node::Quote(other, y)) => k == other && equal(x, y),
            _ => false,
        }
}

pub fn head(form: &Syntax) -> Option<&str> {
    match &form.node {
        Node::List(forms) => match forms.first().map(|form| &form.node) { Some(Node::Sym(name)) => Some(name), _ => None },
        _ => None,
    }
}

pub fn children(form: &Syntax) -> Vec<&Syntax> {
    match &form.node {
        Node::List(forms) | Node::Vec(forms) | Node::Map(forms) => forms.iter().collect(),
        Node::Quote(_, child) => vec![child.as_ref()],
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) => Vec::new(),
    }
}

pub fn renumber(first: Id, form: &Syntax) -> (Syntax, Id) {
    fn go(next: &mut Id, form: &Syntax) -> Syntax {
        let id = *next;
        *next += 1;
        let node = match &form.node {
            Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) => form.node.clone(),
            Node::List(forms) => Node::List(forms.iter().map(|form| go(next, form)).collect()),
            Node::Vec(forms) => Node::Vec(forms.iter().map(|form| go(next, form)).collect()),
            Node::Map(forms) => Node::Map(forms.iter().map(|form| go(next, form)).collect()),
            Node::Quote(kind, child) => Node::Quote(*kind, std::sync::Arc::new(go(next, child))),
        };
        let mut result = form.clone();
        result.id = id;
        result.node = node;
        result
    }
    let mut next = first;
    let result = go(&mut next, form);
    (result, next)
}

fn is_one_of(value: &str, choices: &[&str]) -> bool {
    choices.contains(&value)
}

pub fn attribute_args(head: &str, args: &[Syntax]) -> Vec<Syntax> {
    if !(head.starts_with("sop/") || head.starts_with("array/") || head.ends_with("attr") || head.ends_with("attr_checked")) {
        return args.to_vec();
    }
    let attribute = is_one_of(head, &["sop/attr", "sop/attr_checked", "sop/with_attr", "sop/has_attr", "sop/attr_type", "sop/attr_names", "sop/attr_entries", "attr", "attr_checked", "with_attr", "has_attr"]);
    let group = is_one_of(head, &["sop/group", "sop/group_names", "sop/group_order", "sop/with_group"]);
    let attribute_metadata = is_one_of(head, &["sop/rename_attr", "sop/swap_attr"]);
    let group_metadata = is_one_of(head, &["sop/rename_group", "sop/delete_group"]);
    let positional = attribute || head == "sop/count";
    if !positional && !group && !attribute_metadata && !group_metadata && head != "array/range" && head != "array/max_length" {
        return args.to_vec();
    }
    let mut normalized = Vec::with_capacity(args.len());
    let mut i = 0;
    let mut position = 0;
    if !positional { normalized = args.to_vec(); }
    while positional && i < args.len() {
        if i + 1 < args.len() && matches!(&args[i].node, Node::Kw(key) if is_one_of(key, &["owner", "type", "selection", "default"])) {
            normalized.push(args[i].clone());
            normalized.push(args[i + 1].clone());
            i += 2;
        } else {
            let mut value = args[i].clone();
            if position == 1 {
                if let Node::Kw(name) = &value.node {
                    value.node = Node::Str(name.as_bytes().to_vec());
                }
            }
            normalized.push(value);
            position += 1;
            i += 1;
        }
    }
    let mut i = 0;
    while i < normalized.len() {
        if i + 1 < normalized.len() {
            if let (Node::Kw(kind), Node::Sym(name)) = (&normalized[i].node, &normalized[i + 1].node) {
                let enum_value =
                    (attribute && kind == "owner" && is_one_of(name, &["point", "vertex", "primitive", "detail"]))
                    || (group && kind == "owner" && is_one_of(name, &["point", "vertex", "primitive", "edge"]))
                    || (head == "sop/group" && kind == "as_owner" && name == "point")
                    || (attribute_metadata && kind == "owner" && is_one_of(name, &["point", "vertex", "primitive", "detail", "any"]))
                    || (group_metadata && kind == "owner" && is_one_of(name, &["point", "vertex", "primitive", "edge", "any"]))
                    || ((attribute_metadata || group_metadata) && kind == "conflict" && is_one_of(name, &["skip", "error", "overwrite", "union"]))
                    || (head == "sop/swap_attr" && kind == "method" && is_one_of(name, &["swap", "move", "copy"]))
                    || (kind == "type" && (attribute || head == "array/range" || head == "array/max_length") && is_one_of(name, if attribute { &["float", "int", "vec2", "vec3", "vec4", "text", "float_array", "int_array"] } else if head == "array/max_length" { &["float", "int", "text", "vec2", "vec3", "vec4"] } else { &["float", "int"] }));
                if enum_value {
                    normalized[i + 1].node = Node::Str(name.as_bytes().to_vec());
                    i += 2;
                    continue;
                }
            }
        }
        i += 1;
    }
    normalized
}

pub fn pairs(forms: &[Syntax]) -> Vec<(&Syntax, &Syntax)> {
    forms.chunks_exact(2).map(|pair| (&pair[0], &pair[1])).collect()
}

pub fn memory_bytes(forms: &[Syntax]) -> usize {
    fn strings(values: &[String]) -> usize {
        values.iter().map(|value| 3 + value.len() / 8).sum()
    }
    fn form_bytes(form: &Syntax) -> usize {
        13 + strings(&form.notes()) + strings(&form.meta()) + strings(&form.tail())
            + match &form.node {
                Node::Sym(text) | Node::Kw(text) | Node::Num(text) => 3 + text.len() / 8,
                Node::Str(text) => 3 + text.len() / 8,
                Node::List(forms) | Node::Vec(forms) | Node::Map(forms) => forms.iter().map(|child| 3 + form_bytes(child)).sum(),
                Node::Quote(_, child) => form_bytes(child),
            }
    }
    8 * forms.iter().map(form_bytes).sum::<usize>()
}

#[cfg(test)]
mod tests {
    use super::{attribute_args, children, equal, make, make_with, number, number_value, parse, renumber, separator, Node, QuoteKind, text_lexer, Syntax};
    use rays_diagnostic::{position_of_offset, Position, Span};

    fn one(source: &str) -> Syntax {
        let mut forms = parse(source).unwrap();
        assert_eq!(forms.len(), 1, "expected one form: {source}");
        forms.remove(0)
    }

    fn ids(form: &Syntax, result: &mut Vec<i64>) {
        result.push(form.id);
        for child in children(form) { ids(child, result); }
    }

    fn fails(source: &str, code: &str, offset: i64) {
        let problem = parse(source).unwrap_err();
        assert_eq!(problem.code, code);
        assert_eq!(problem.position, Some(position_of_offset(source, offset)));
        assert_eq!(problem.span.as_ref().map(|span| span.start), Some(offset));
    }

    #[test]
    fn parses_atoms_containers_quotes_and_comments() {
        let forms = parse(r#"[-.5 1. 2.0 3 - 1e3 :kw "s\n\"q\"" a/b :]"#).unwrap();
        let Node::Vec(items) = &forms[0].node else { panic!("expected vector"); };
        assert!(matches!(&items[0].node, Node::Num(text) if text == "-.5"));
        assert!(matches!(&items[1].node, Node::Num(text) if text == "1."));
        assert!(matches!(&items[2].node, Node::Num(text) if text == "2.0"));
        assert!(matches!(&items[3].node, Node::Num(text) if text == "3"));
        assert!(matches!(&items[4].node, Node::Sym(text) if text == "-"));
        assert!(matches!(&items[5].node, Node::Num(text) if text == "1e3"));
        assert!(matches!(&items[6].node, Node::Kw(text) if text == "kw"));
        assert!(matches!(&items[7].node, Node::Str(text) if text.as_slice() == &b"s\n\"q\""[..]));
        assert!(matches!(&items[8].node, Node::Sym(text) if text == "a/b"));
        assert!(matches!(&items[9].node, Node::Sym(text) if text == ":"));
        let escaped = parse("\"\\255\\000\"").unwrap();
        assert!(matches!(&escaped[0].node, Node::Str(bytes) if bytes.as_slice() == &[255_u8, 0][..]));
        assert!(matches!(one("{:a 1 :b [1 2 3 4]}").node, Node::Map(_)));
        let Node::List(quoted) = one("(f 'a `(b ~c ~@d))").node else { panic!("expected list"); };
        assert!(matches!(&quoted[1].node, Node::Quote(QuoteKind::Plain, inner) if matches!(&inner.node, Node::Sym(name) if name == "a")));
        let Node::Quote(QuoteKind::Quasi, quasi) = &quoted[2].node else { panic!("expected quasiquote"); };
        let Node::List(quoted_items) = &quasi.node else { panic!("expected quoted list"); };
        assert!(matches!(&quoted_items[1].node, Node::Quote(QuoteKind::Unquote, _)));
        assert!(matches!(&quoted_items[2].node, Node::Quote(QuoteKind::Splice, inner) if matches!(&inner.node, Node::Sym(name) if name == "d")));
        let Node::Vec(symbols) = one("[a' step# ~i]").node else { panic!("expected vector"); };
        assert!(matches!(&symbols[0].node, Node::Sym(name) if name == "a'"));
        assert!(matches!(&symbols[1].node, Node::Sym(name) if name == "step#"));
        assert!(matches!(&symbols[2].node, Node::Quote(QuoteKind::Unquote, _)));

        let forms = parse(";; head\n; second\n(a ; on b\n b\n  ;\n c ; trailing\n )\n; after\n(d)\n; end\n").unwrap();
        assert_eq!(forms[0].notes(), vec!["head".to_owned(), "second".to_owned()]);
        assert_eq!(forms[0].tail(), vec!["trailing".to_owned()]);
        let Node::List(children) = &forms[0].node else { panic!("expected list"); };
        assert_eq!(children[1].notes(), vec!["on b".to_owned()]);
        assert_eq!(children[2].notes(), vec!["".to_owned()]);
        assert_eq!(forms[1].notes(), vec!["after".to_owned()]);
        assert_eq!(forms[1].tail(), vec!["end".to_owned()]);
        assert_eq!(one("(a ; end\n)").tail(), vec!["end".to_owned()]);
        let Node::List(children) = one("(f ; c\n ' ; d\n x)").node else { panic!("expected list"); };
        assert_eq!(children[1].notes(), vec!["c".to_owned(), "d".to_owned()]);
        assert!(matches!(&children[1].node, Node::Quote(QuoteKind::Plain, inner) if inner.notes().is_empty()));
    }

    #[test]
    fn metadata_ids_and_identity() {
        let source = "(a ^:bypass ^:x (b c))";
        let form = one(source);
        let Node::List(children) = &form.node else { panic!("expected list"); };
        let metadata_form = &children[1];
        assert_eq!(metadata_form.meta(), ["bypass", "x"]);
        assert_eq!(&source[metadata_form.span.start as usize..metadata_form.span.finish as usize], "^:bypass ^:x (b c)");

        let form = one("(a [b c] 'd)");
        let mut original_ids = Vec::new();
        ids(&form, &mut original_ids);
        assert_eq!(original_ids, [0, 1, 2, 3, 4, 5, 6]);
        let (renumbered, next) = renumber(100, &form);
        let mut new_ids = Vec::new();
        ids(&renumbered, &mut new_ids);
        assert_eq!(new_ids, [100, 101, 102, 103, 104, 105, 106]);
        assert_eq!(next, 107);
        assert_eq!(renumbered.span, form.span);
        assert_eq!(parse("x y").unwrap().iter().map(|form| form.id).collect::<Vec<_>>(), vec![0_i64, 1]);
        assert_eq!(make(Node::Sym("z".into())).id, 0);
        let authored = make_with(Node::Sym("z".into()), vec!["note".into()], vec!["bypass".into()]);
        assert!(!equal(&authored, &make(Node::Sym("z".into()))));
        let mut same = make(Node::Sym("z".into()));
        same.id = 8;
        same.span = Span { start: 20, finish: 21 };
        assert!(equal(&same, &make(Node::Sym("z".into()))));
        let call = one("(call x)");
        assert_eq!(super::head(&call), Some("call"));
        let forms = parse("a b c").unwrap();
        let pairs = super::pairs(&forms);
        assert_eq!(pairs.len(), 1);
        assert!(super::memory_bytes(&forms) > 0);
        assert_eq!(super::LIBRARY_BASE, 1_i64 << 40);
    }

    #[test]
    fn diagnostics_depth_and_threading() {
        std::thread::Builder::new().stack_size(64 << 20).spawn(diagnostics_depth_body).unwrap().join().unwrap();
    }

    fn diagnostics_depth_body() {
        fails("(", "E_UNCLOSED", 0);
        fails("(a\n  [b", "E_UNCLOSED", 5);
        fails("{:a 1", "E_UNCLOSED", 0);
        fails("(a]", "E_UNEXPECTED", 2);
        fails("[a}", "E_UNEXPECTED", 2);
        fails(")", "E_UNEXPECTED", 0);
        fails("\"open", "E_UNCLOSED", 0);
        fails("(a \"\\q\")", "E_UNEXPECTED", 4);
        fails("^ x", "E_UNEXPECTED", 0);
        fails("(a ^:bypass)", "E_UNEXPECTED", 3);
        fails("'", "E_UNEXPECTED", 1);
        fails(&("(".repeat(300) + &")".repeat(300)), "E_DEPTH", 257);
        assert!(parse(&("(".repeat(250) + &")".repeat(250))).is_ok());
        let chain = |count: usize| format!("(-> x{})", " (f)".repeat(count));
        assert!(parse(&chain(200)).is_ok());
        fails(&chain(100_000), "E_DEPTH", 0);
        assert!(matches!(one("(-> x)").node, Node::Sym(_)));
        fails("(->)", "E_THREAD", 0);
        fails("(-> x 5)", "E_THREAD", 6);
        fails("(-> x g)", "E_THREAD", 6);
        fails("(a) ; \u{1}", "E_UNEXPECTED", 6);
        fails("\"\u{3}\"", "E_UNEXPECTED", 1);
        assert!(parse("").unwrap().is_empty());
        assert!(parse("; only a comment").unwrap().is_empty());
        for value in ["1e-14", "2e+06", "6.1E-17", "-3.3e7", "1.e2"] { assert!(number(value)); }
        for value in ["e5", "1e", "1e+", "1e5x", "-", "."] { assert!(!number(value)); }
        assert_eq!(number_value("-.5"), Some(-0.5));
        assert!(number_value("1e999").is_none());
        assert!(separator('(') && separator('\n') && !separator(':'));
        assert_eq!(parse("(a\n(b]").unwrap_err().position, Some(Position { line: 2, col: 3 }));
    }

    #[test]
    fn lexer_tolerates_later_tokens_and_attribute_normalizes_selectors() {
        let source = "(a \"bad\\q\" b)";
        let tokens = text_lexer::lex(source);
        let issue = tokens.iter().find_map(|token| token.issue.as_ref()).unwrap();
        assert_eq!((issue.code.as_str(), issue.start, issue.finish), ("E_UNEXPECTED", 7, 9));
        assert!(tokens.iter().any(|token| matches!(&token.kind, text_lexer::Kind::Word(text_lexer::Word::Symbol(value)) if value == "b") && token.start == 11));
        assert_eq!(parse(source).unwrap_err().code, issue.code);
        assert!(text_lexer::lex("x").iter().any(|token| matches!(&token.kind, text_lexer::Kind::End)));

        let parsed = parse("[size :normal :owner point :type vec3]").unwrap();
        let Node::Vec(args) = &parsed[0].node else { panic!("expected vector"); };
        let normalized = attribute_args("sop/attr", args);
        assert!(matches!(&normalized[1].node, Node::Str(name) if name.as_slice() == &b"normal"[..]));
        assert!(matches!(&normalized[2].node, Node::Kw(owner) if owner == "owner"));
        assert!(matches!(&normalized[3].node, Node::Str(value) if value.as_slice() == &b"point"[..]));
        assert!(matches!(&normalized[4].node, Node::Kw(kind) if kind == "type"));
        assert!(matches!(&normalized[5].node, Node::Str(value) if value.as_slice() == &b"vec3"[..]));
        assert!(matches!(&attribute_args("unrelated", &normalized)[0].node, Node::Sym(name) if name == "size"));
        let parsed = parse("[:owner point :type text]").unwrap();
        let Node::Vec(args) = &parsed[0].node else { panic!("expected vector"); };
        let normalized = attribute_args("array/range", args);
        assert!(matches!(&normalized[1].node, Node::Sym(value) if value == "point"));
        assert!(matches!(&normalized[3].node, Node::Sym(value) if value == "text"));
    }
    #[test]
    fn decimal_byte_escapes_preserve_all_bytes() {
        for byte in 0_u16..=255 {
            let source = format!("\"\\{byte:03}\"");
            assert_eq!(parse(&source).unwrap()[0].node, Node::Str(vec![byte as u8]));
        }
        for source in ["\"\\256\"", "\"\\999\"", "\"\\01\"", "\"\\1ab\"", "\"\\12\"", "\"\\00\""] {
            assert!(parse(source).is_err(), "accepted {source}");
        }
    }
}

// A form is one node of the tree every Lisp text becomes: keep it small (id, node, span, trivia pointer).
const _: () = assert!(std::mem::size_of::<Syntax>() <= 64);
const _: () = assert!(std::mem::size_of::<Node>() <= 32);
