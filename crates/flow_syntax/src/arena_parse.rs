use crate::{arena::{Node, Syntax, SyntaxBuilder, SyntaxId}, syntax::{Id, QuoteKind, Trivia}, text_lexer::{self, Kind, Token, Word}};
use rays_diagnostic::{self as diagnostic, Diagnostic, Span};

const DEPTH_LIMIT: usize = 256;

fn fail(source: &str, start: i64, finish: i64, code: &str, message: &str) -> Diagnostic {
    diagnostic::error(code.to_owned(), message.to_owned(),
        Some(Span { start: u32::try_from(start).expect("validated source offset"), finish: u32::try_from(finish).expect("validated source offset") }),
        Some(diagnostic::position_of_offset(source, start)), None, Vec::new(), Vec::new())
}
fn span(token: &Token) -> Span {
    Span { start: u32::try_from(token.start).expect("validated source offset"), finish: u32::try_from(token.stop).expect("validated source offset") }
}
fn fresh(next: &mut Id) -> Id { let id = *next; *next += 1; id }
fn comments(tokens: &[Token], at: &mut usize) -> Vec<String> {
    let mut notes = Vec::new();
    while let Kind::Comment(text) = &tokens[*at].kind { notes.push(text.clone()); *at += 1; }
    notes
}
fn trivia(notes: Vec<String>, tail: Vec<String>) -> Option<Trivia> {
    if notes.is_empty() && tail.is_empty() { None } else { Some(Trivia { notes, meta: Vec::new(), tail }) }
}
fn form(source: &str, tokens: &[Token], at: &mut usize, next: &mut Id, notes: Vec<String>, depth: usize, builder: &mut SyntaxBuilder) -> Result<SyntaxId, Diagnostic> {
    let token = &tokens[*at];
    if !matches!(token.kind, Kind::End) { *at += 1; }
    if depth > DEPTH_LIMIT {
        return Err(fail(source, token.start, token.stop, "E_DEPTH", &format!("S-expression nesting exceeds {DEPTH_LIMIT} forms")));
    }
    let node = match &token.kind {
        Kind::Word(Word::Symbol(text)) => Node::Sym(builder.intern(text)),
        Kind::Word(Word::Keyword(text)) => Node::Kw(builder.intern(text)),
        Kind::Word(Word::Number(text)) => Node::Num(builder.intern(text)),
        Kind::String(bytes) => Node::Str(builder.bytes(bytes)),
        Kind::Open(delimiter) => {
            let id = fresh(next);
            return group(source, tokens, at, next, token, *delimiter, id, notes, depth, builder);
        }
        Kind::Prefix(prefix) => {
            let id = fresh(next);
            let mut notes = notes;
            notes.extend(comments(tokens, at));
            let child = form(source, tokens, at, next, Vec::new(), depth + 1, builder)?;
            let kind = match prefix { '\'' => QuoteKind::Plain, '`' => QuoteKind::Quasi, '~' => QuoteKind::Unquote, '@' => QuoteKind::Splice, _ => unreachable!() };
            let span = Span { start: span(token).start, finish: builder.row(child).span.finish };
            return Ok(builder.push(id, Node::Quote(kind, child), span, trivia(notes, Vec::new())));
        }
        Kind::Meta(name) => {
            let mut notes = notes;
            notes.extend(comments(tokens, at));
            if matches!(tokens[*at].kind, Kind::End | Kind::Close(_)) {
                return Err(fail(source, token.start, token.stop, "E_UNEXPECTED", "Metadata needs a form after it"));
            }
            let child = form(source, tokens, at, next, notes, depth + 1, builder)?;
            builder.trivia_mut(child).meta.insert(0, name.clone());
            let row = builder.row(child);
            let span = Span { start: span(token).start, finish: row.span.finish };
            builder.set_location(child, row.id, span);
            return Ok(child);
        }
        Kind::Close(delimiter) => return Err(fail(source, token.start, token.stop, "E_UNEXPECTED", &format!("Unexpected {delimiter:?}"))),
        Kind::Comment(_) | Kind::End => return Err(fail(source, token.start, token.stop, "E_UNEXPECTED", "Expected a form")),
    };
    Ok(builder.push(fresh(next), node, span(token), trivia(notes, Vec::new())))
}
// ponytail: indentation only attributes an already-invalid form; use a line map if very large malformed files make this error path slow.
fn unclosed_span(source: &str, tokens: &[Token], opening: &Token) -> (i64, i64) {
    let mut stack: Vec<&Token> = Vec::new();
    let mut suspect = opening;
    for token in tokens {
        match token.kind {
            Kind::Open('(') => {
                let position = diagnostic::position_of_offset(source, token.start);
                if let Some(parent) = stack.last() {
                    let parent_position = diagnostic::position_of_offset(source, parent.start);
                    if position.line > parent_position.line && position.col <= parent_position.col {
                        suspect = parent;
                    }
                }
                stack.push(token);
            }
            Kind::Open(_) => stack.push(token),
            Kind::Close(_) => { stack.pop(); }
            _ => (),
        }
    }
    if suspect.start < opening.start { (opening.start, opening.stop) } else { (suspect.start, suspect.stop) }
}

fn group(source: &str, tokens: &[Token], at: &mut usize, next: &mut Id, opening: &Token, delimiter: char, id: Id, notes: Vec<String>, depth: usize, builder: &mut SyntaxBuilder) -> Result<SyntaxId, Diagnostic> {
    let expected = match delimiter { '(' => ')', '[' => ']', _ => '}' };
    let mut items = Vec::new();
    loop {
        let leading = comments(tokens, at);
        let token = &tokens[*at];
        match &token.kind {
            Kind::Close(actual) if *actual == expected => {
                *at += 1;
                let span = Span { start: span(opening).start, finish: span(token).finish };
                if delimiter == '(' && items.first().is_some_and(|&row| matches!(builder.row(row).node, Node::Sym(name) if builder.name(name) == "->")) {
                    return thread(source, opening, id, span, &items, notes, leading, depth, builder);
                }
                let range = builder.range(&items);
                let node = match delimiter { '(' => Node::List(range), '[' => Node::Vec(range), _ => Node::Map(range) };
                return Ok(builder.push(id, node, span, trivia(notes, leading)));
            }
            Kind::Close(actual) => {
                let line = diagnostic::position_of_offset(source, opening.start).line;
                return Err(fail(source, token.start, token.stop, "E_UNEXPECTED", &format!("Expected {expected:?} to close the {delimiter:?} on line {line}, found {actual:?}")));
            }
            Kind::End => {
                let (start, stop) = unclosed_span(source, tokens, opening);
                return Err(fail(source, start, stop, "E_UNCLOSED", &format!("This {delimiter:?} is never closed")));
            },
            _ => items.push(form(source, tokens, at, next, leading, depth + 1, builder)?),
        }
    }
}
fn thread(source: &str, opening: &Token, id: Id, span: Span, forms: &[SyntaxId], notes: Vec<String>, tail: Vec<String>, depth: usize, builder: &mut SyntaxBuilder) -> Result<SyntaxId, Diagnostic> {
    if forms.len() == 1 { return Err(fail(source, opening.start, opening.stop, "E_THREAD", "-> needs a value to thread")); }
    let mut result = forms[1];
    if forms.len() == 2 {
        if !notes.is_empty() {
            let existing = std::mem::take(&mut builder.trivia_mut(result).notes);
            builder.trivia_mut(result).notes = [notes, existing].concat();
        }
        return Ok(result);
    }
    let steps = &forms[2..];
    if depth + steps.len() > DEPTH_LIMIT {
        return Err(fail(source, opening.start, opening.stop, "E_DEPTH", &format!("S-expression nesting exceeds {DEPTH_LIMIT} forms")));
    }
    for (index, &step) in steps.iter().enumerate() {
        let row = builder.row(step).clone();
        let Node::List(range) = row.node else {
            return Err(fail(source, row.span.start.into(), row.span.finish.into(), "E_THREAD", "A -> step is a call, as in (sop/normals :cusp_angle 0.6)"));
        };
        let parts = builder.range_ids(range);
        if parts.is_empty() || !matches!(builder.row(parts[0]).node, Node::Sym(_)) {
            return Err(fail(source, row.span.start.into(), row.span.finish.into(), "E_THREAD", "A -> step is a call, as in (sop/normals :cusp_angle 0.6)"));
        }
        let mut threaded = Vec::with_capacity(parts.len() + 1);
        threaded.push(parts[0]); threaded.push(result); threaded.extend_from_slice(&parts[1..]);
        let range = builder.range(&threaded);
        let last = index + 1 == steps.len();
        let mut trivia = builder.trivia(step).cloned().unwrap_or_default();
        if last {
            trivia.notes = [notes.clone(), trivia.notes].concat();
            trivia.tail = [tail.clone(), trivia.tail].concat();
        }
        result = builder.push(if last { id } else { row.id }, Node::List(range), if last { span.clone() } else { row.span }, Some(trivia));
    }
    Ok(result)
}

/// Parse directly into one packed owner; no recursive Syntax or per-node arena imports are built.
pub fn parse(source: &str) -> Result<Vec<Syntax>, Diagnostic> {
    let _timer = crate::phase_timer::measure("parse");
    diagnostic::check_source_len(source.len())?;
    let tokens = text_lexer::lex(source);
    let first_issue = tokens.iter().filter_map(|token| token.issue.as_ref()).find(|problem| problem.message.starts_with("Control byte "))
        .or_else(|| tokens.iter().find_map(|token| token.issue.as_ref()));
    if let Some(problem) = first_issue { return Err(fail(source, problem.start, problem.finish, &problem.code, &problem.message)); }
    let mut builder = SyntaxBuilder::new();
    let (mut at, mut next) = (0, 0);
    let mut roots = Vec::new();
    loop {
        let leading = comments(&tokens, &mut at);
        if matches!(tokens[at].kind, Kind::End) {
            if !leading.is_empty() {
                if let Some(&last) = roots.last() {
                    if matches!(builder.row(last).node, Node::List(_) | Node::Vec(_) | Node::Map(_)) { builder.trivia_mut(last).tail.extend(leading); }
                }
            }
            let arena = builder.freeze();
            return Ok(roots.into_iter().map(|row| arena.handle(row)).collect());
        }
        roots.push(form(source, &tokens, &mut at, &mut next, leading, 0, &mut builder)?);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn authored_parser_and_debug_contract_is_unchanged() {
        for source in ["", "; comment", "a b", "[-.5 :kw \"\\255\\000\"]", "(a ^:x ^:bypass (b c))", "(-> x)", "(-> x (f) (g 3))", "; head\n(f ; inside\n ' ; quote\n x)\n; tail", "(f 'a `(b ~c ~@d))"] {
            let packed = parse(source).unwrap();
            if let Some(first) = packed.first() { assert!(packed.iter().all(|form| Arc::ptr_eq(first.arena(), form.arena()))); }
        }
        let expected = "Syntax { id: 0, node: List([Syntax { id: 1, node: Sym(\"a\"), span: Span { start: 1, finish: 2 }, notes: [], meta: [], tail: [] }]), span: Span { start: 0, finish: 3 }, notes: [], meta: [], tail: [] }";
        assert_eq!(format!("{:?}", parse("(a)").unwrap()[0]), expected);
        for (source, code) in [("(", "E_UNCLOSED"), ("[a}", "E_UNEXPECTED"), ("'", "E_UNEXPECTED"), ("^:x", "E_UNEXPECTED"), ("(->)", "E_THREAD"), ("(-> x 5)", "E_THREAD"), ("(a \"bad\\q\" b)", "E_UNEXPECTED")] {
            assert_eq!(parse(source).unwrap_err().code, code, "{source}");
        }
    }
    #[test]
    fn normalization_changes_atoms_without_copying_unchanged_trees() {
        for (head, text) in [
            ("sop/attr", "[(large [unchanged tree]) :normal :owner point :type vec3]"),
            ("sop/group", "[source :owner edge :as_owner point]"),
            ("sop/rename_attr", "[source :owner any :conflict overwrite]"),
            ("sop/swap_attr", "[source :method move]"),
            ("array/range", "[:type float :owner point]"),
            ("array/max_length", "[:type vec3]"), ("unrelated", "[x :type float]"),
        ] {
            let root = parse(text).unwrap().remove(0);
            let args: Vec<Syntax> = root.children().iter().map(|&id| root.child(id)).collect();
            let normalized = crate::arena::attribute_args(head, &args);
            for (index, (id, logical_text)) in crate::arena::attribute_args_view(head, root.arena(), root.children()).enumerate() {
                assert_eq!(id, root.children()[index]);
                if let Some(text) = logical_text {
                    let Node::Str(bytes) = normalized[index].node else { panic!("normalized atom must be text"); };
                    assert_eq!(normalized[index].bytes(bytes), text);
                } else { assert_eq!(normalized[index].identity(), args[index].identity()); }
            }
            let mut builder = SyntaxBuilder::new();
            let imported = builder.import(&root);
            let Node::Vec(range) = builder.row(imported).node else { unreachable!() };
            let local_args = builder.range_ids(range).to_vec();
            let local_normalized = builder.attribute_args(head, &local_args);
            let arena = builder.freeze();
            let local_normalized: Vec<Syntax> = local_normalized.into_iter().map(|id| arena.handle(id)).collect();
            assert_eq!(format!("{local_normalized:?}"), format!("{normalized:?}"));
            if head == "sop/attr" { assert_eq!(normalized[0].identity(), args[0].identity()); }
        }
    }
    use std::sync::Arc;
}

#[cfg(test)]
mod unclosed_tests {
    #[test]
    fn unclosed_form_points_at_the_inner_indentation_break() {
        let source = "(workspace example\n  (graph first :context draw\n    (draw/circle [0 0] 3\n  (graph next :context draw (draw/merge)))";
        let diagnostic = crate::syntax::parse(source).unwrap_err();
        assert_eq!(diagnostic.code, "E_UNCLOSED");
        assert_eq!(diagnostic.position.unwrap().line, 3);
        assert_eq!(diagnostic.span.unwrap().start as usize, source.find("(draw/circle").unwrap());
        assert!(crate::syntax::parse("(workspace example\n(graph first :context draw\n(draw/circle [0 0] 3)))").is_ok(), "indentation must never change valid Lisp semantics");
    }
}
