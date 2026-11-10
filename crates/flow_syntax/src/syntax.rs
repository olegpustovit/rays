
pub use crate::arena::{attribute_args, attribute_args_view, equal, equal_node, head, make, make_with, renumber,
    BytesId, Draft, NameId, Node, RangeId, Syntax, SyntaxArena, SyntaxBuilder, SyntaxId, SyntaxRow};
pub use crate::arena_parse::parse;

pub type Id = i64;
pub const LIBRARY_BASE: Id = 1 << 40;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QuoteKind { Plain, Quasi, Unquote, Splice }

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Trivia { pub notes: Vec<String>, pub meta: Vec<String>, pub tail: Vec<String> }

pub fn number(text: &str) -> bool { crate::text_lexer::number(text) }
pub fn number_value(text: &str) -> Option<f64> {
    match text {
        "NaN" => Some(f64::NAN), "Infinity" => Some(f64::INFINITY), "-Infinity" => Some(f64::NEG_INFINITY),
        _ => {
            let (mantissa, exponent) = text.find(|c| c == 'e' || c == 'E').map_or((text, ""), |at| text.split_at(at));
            let normalized = if let Some(value) = mantissa.strip_prefix("-.") { format!("-0.{value}{exponent}") }
                else if let Some(value) = mantissa.strip_prefix('.') { format!("0.{value}{exponent}") }
                else if mantissa.ends_with('.') { format!("{mantissa}0{exponent}") } else { text.to_owned() };
            normalized.parse::<f64>().ok().filter(|value| value.is_finite())
        }
    }
}
pub fn separator(c: char) -> bool { crate::text_lexer::separator(c) }
pub fn children(form: &Syntax) -> &[SyntaxId] { form.children() }
pub fn pairs(forms: &[Syntax]) -> Vec<(&Syntax, &Syntax)> { forms.chunks_exact(2).map(|pair| (&pair[0], &pair[1])).collect() }
pub fn memory_bytes(forms: &[Syntax]) -> usize {
    let mut memory = Memory::default();
    for form in forms { memory.add(form); }
    memory.bytes()
}

/// Retained packed storage, deduplicated across all roots and historical workspaces.
/// Counts capacities and pointee storage, not allocator metadata or Arc control blocks.
#[derive(Default)]
pub struct Memory { seen: std::collections::HashSet<usize>, bytes: usize, allocations: Vec<(usize, usize)> }
impl Memory {
    pub fn bytes(&self) -> usize { self.bytes }
    pub fn into_allocations(self) -> Vec<(usize, usize)> { self.allocations }
    pub fn allocation<T>(&mut self, pointer: *const T, bytes: usize) -> bool {
        if bytes == 0 || !self.seen.insert(pointer as usize) { return false; }
        self.bytes += bytes;
        self.allocations.push((pointer as usize, bytes));
        true
    }
    pub fn forms(&mut self, forms: &Vec<Syntax>) {
        if self.allocation(forms.as_ptr(), forms.capacity() * std::mem::size_of::<Syntax>()) {
            for form in forms { self.add(form); }
        }
    }
    pub fn shared_forms(&mut self, forms: &std::sync::Arc<Vec<Syntax>>) {
        if self.allocation(std::sync::Arc::as_ptr(forms), std::mem::size_of::<Vec<Syntax>>()) { self.forms(forms); }
    }
    pub fn shared_form(&mut self, form: &std::sync::Arc<Syntax>) {
        if self.allocation(std::sync::Arc::as_ptr(form), std::mem::size_of::<Syntax>()) { self.add(form); }
    }
    pub fn add(&mut self, form: &Syntax) { form.arena().attribute(self); }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text_lexer;
    use rays_diagnostic::{position_of_offset, Position};

    fn one(source: &str) -> Syntax {
        let mut forms = parse(source).unwrap();
        assert_eq!(forms.len(), 1, "expected one form: {source}");
        forms.remove(0)
    }
    fn child(form: &Syntax, index: usize) -> Syntax { form.child(form.children()[index]) }
    fn word(form: &Syntax) -> &str {
        match form.node { Node::Sym(name) | Node::Kw(name) | Node::Num(name) => form.name(name), _ => panic!("expected atom: {form:?}") }
    }
    fn bytes(form: &Syntax) -> &[u8] {
        match form.node { Node::Str(bytes) => form.bytes(bytes), _ => panic!("expected string: {form:?}") }
    }
    fn ids(form: &Syntax, result: &mut Vec<i64>) {
        let mut pending = vec![form.row_id()];
        while let Some(id) = pending.pop() {
            result.push(form.arena().row(id).id);
            pending.extend(form.arena().children(id).iter().rev().copied());
        }
    }
    fn fails(source: &str, code: &str, offset: i64) {
        let problem = parse(source).unwrap_err();
        assert_eq!(problem.code, code);
        assert_eq!(problem.position, Some(position_of_offset(source, offset)));
        assert_eq!(problem.span.as_ref().map(|span| i64::from(span.start)), Some(offset));
    }

    #[test]
    fn retained_memory_counts_capacity_and_deduplicates_shared_owners() {
        let mut text = Vec::with_capacity(128);
        text.push(b'x');
        let atom = make(Draft::Str(text));
        let Node::Str(id) = atom.node else { unreachable!() };
        let allocation = (atom.bytes(id).as_ptr() as usize, atom.bytes(id).len() + 2 * std::mem::size_of::<usize>());
        let mut memory = Memory::default();
        memory.add(&atom);
        let first = memory.bytes();
        memory.add(&atom.clone());
        assert_eq!(memory.bytes(), first);
        assert!(memory.into_allocations().contains(&allocation), "immutable bytes retain their Arc header and payload");
        let form = one("; note\n(a ['b] \"text\")");
        let mut memory = Memory::default();
        memory.add(&form);
        let first = memory.bytes();
        memory.add(&form);
        memory.add(&form.clone());
        assert_eq!(memory.bytes(), first, "cloned roots share rows, names, bytes and trivia");
    }

    #[test]
    fn parses_atoms_containers_quotes_and_comments() {
        let form = one(r#"[-.5 1. 2.0 3 - 1e3 :kw "s\n\"q\"" a/b :]"#);
        assert!(matches!(form.node, Node::Vec(_)));
        assert_eq!(form.children().len(), 10);
        for (index, text) in ["-.5", "1.", "2.0", "3", "-", "1e3", "kw"].iter().enumerate() { assert_eq!(word(&child(&form, index)), *text); }
        for index in [0, 1, 2, 3, 5] { assert!(matches!(child(&form, index).node, Node::Num(_))); }
        assert!(matches!(child(&form, 4).node, Node::Sym(_)));
        assert!(matches!(child(&form, 6).node, Node::Kw(_)));
        assert_eq!(bytes(&child(&form, 7)), b"s\n\"q\"");
        assert_eq!(word(&child(&form, 8)), "a/b");
        assert_eq!(word(&child(&form, 9)), ":");
        assert_eq!(bytes(&one("\"\\255\\000\"")), &[255, 0]);
        let map = one("{:a 1 :b [1 2 3 4]}");
        assert!(matches!(map.node, Node::Map(_)));
        assert_eq!(map.children().len(), 4);
        assert_eq!(word(&child(&map, 0)), "a");
        assert_eq!(word(&child(&map, 2)), "b");
        assert_eq!(child(&map, 3).children().len(), 4);
        let quoted = one("(f 'a `(b ~c ~@d))");
        assert_eq!(quoted.children().len(), 3);
        let plain = child(&quoted, 1);
        assert!(matches!(plain.node, Node::Quote(QuoteKind::Plain, _)));
        assert_eq!(word(&child(&plain, 0)), "a");
        let quasi = child(&quoted, 2);
        assert!(matches!(quasi.node, Node::Quote(QuoteKind::Quasi, _)));
        let quoted_items = child(&quasi, 0);
        assert_eq!(quoted_items.children().len(), 3);
        assert!(matches!(child(&quoted_items, 1).node, Node::Quote(QuoteKind::Unquote, _)));
        let splice = child(&quoted_items, 2);
        assert!(matches!(splice.node, Node::Quote(QuoteKind::Splice, _)));
        assert_eq!(word(&child(&splice, 0)), "d");
        let symbols = one("[a' step# ~i]");
        assert_eq!(symbols.children().len(), 3);
        assert_eq!(word(&child(&symbols, 0)), "a'");
        assert_eq!(word(&child(&symbols, 1)), "step#");
        assert!(matches!(child(&symbols, 2).node, Node::Quote(QuoteKind::Unquote, _)));
        assert_eq!(word(&child(&child(&symbols, 2), 0)), "i");
        let forms = parse(";; head\n; second\n(a ; on b\n b\n  ;\n c ; trailing\n )\n; after\n(d)\n; end\n").unwrap();
        assert_eq!(forms[0].notes(), ["head", "second"]);
        assert_eq!(forms[0].tail(), ["trailing"]);
        assert_eq!(child(&forms[0], 1).notes(), ["on b"]);
        assert_eq!(child(&forms[0], 2).notes(), [""]);
        assert_eq!(forms[1].notes(), ["after"]);
        assert_eq!(forms[1].tail(), ["end"]);
        assert_eq!(one("(a ; end\n)").tail(), ["end"]);
        let quoted = child(&one("(f ; c\n ' ; d\n x)"), 1);
        assert_eq!(quoted.notes(), ["c", "d"]);
        assert!(child(&quoted, 0).notes().is_empty());
    }

    #[test]
    fn metadata_ids_and_identity() {
        let source = "(a ^:bypass ^:x (b c))";
        let form = one(source);
        let metadata_form = child(&form, 1);
        assert_eq!(metadata_form.meta(), ["bypass", "x"]);
        assert_eq!(&source[metadata_form.span.start as usize..metadata_form.span.finish as usize], "^:bypass ^:x (b c)");
        let form = one("(a [b c] 'd)");
        let mut original_ids = Vec::new(); ids(&form, &mut original_ids);
        assert_eq!(original_ids, [0, 1, 2, 3, 4, 5, 6]);
        let (renumbered, next) = renumber(100, &form);
        let mut new_ids = Vec::new(); ids(&renumbered, &mut new_ids);
        assert_eq!(new_ids, [100, 101, 102, 103, 104, 105, 106]);
        assert_eq!(next, 107);
        assert_eq!(renumbered.span, form.span);
        assert_eq!(parse("x y").unwrap().iter().map(|form| form.id).collect::<Vec<_>>(), [0, 1]);
        assert_eq!(make(Draft::Sym("z".into())).id, 0);
        let authored = make_with(Draft::Sym("z".into()), vec!["note".into()], vec!["bypass".into()]);
        assert!(!equal(&authored, &make(Draft::Sym("z".into()))));
        let mut builder = SyntaxBuilder::new();
        let original = make(Draft::Sym("z".into()));
        let original_row = builder.import(&original);
        let changed = builder.with_location(original_row, 8, rays_diagnostic::Span { start: 20, finish: 21 });
        assert!(equal(&builder.freeze().handle(changed), &original));
        assert_eq!(head(&one("(call x)")), Some("call"));
        let forms = parse("a b c").unwrap();
        assert_eq!(pairs(&forms).len(), 1);
        assert!(memory_bytes(&forms) > 0);
        assert_eq!(LIBRARY_BASE, 1_i64 << 40);
    }

    #[test]
    fn diagnostics_depth_and_threading() {
        std::thread::Builder::new().stack_size(64 << 20).spawn(diagnostics_depth_body).unwrap().join().unwrap();
    }
    fn diagnostics_depth_body() {
        for (source, code, offset) in [("(", "E_UNCLOSED", 0), ("(a\n  [b", "E_UNCLOSED", 5), ("{:a 1", "E_UNCLOSED", 0), ("(a]", "E_UNEXPECTED", 2), ("[a}", "E_UNEXPECTED", 2), (")", "E_UNEXPECTED", 0), ("\"open", "E_UNCLOSED", 0), ("(a \"\\q\")", "E_UNEXPECTED", 4), ("^ x", "E_UNEXPECTED", 0), ("(a ^:bypass)", "E_UNEXPECTED", 3), ("'", "E_UNEXPECTED", 1)] { fails(source, code, offset); }
        fails(&("(".repeat(300) + &")".repeat(300)), "E_DEPTH", 257);
        assert!(parse(&("(".repeat(250) + &")".repeat(250))).is_ok());
        let chain = |count: usize| format!("(-> x{})", " (f)".repeat(count));
        assert!(parse(&chain(200)).is_ok());
        fails(&chain(100_000), "E_DEPTH", 0);
        assert!(matches!(one("(-> x)").node, Node::Sym(_)));
        fails("(->)", "E_THREAD", 0); fails("(-> x 5)", "E_THREAD", 6); fails("(-> x g)", "E_THREAD", 6);
        fails("(a) ; \u{1}", "E_UNEXPECTED", 6); fails("\"\u{3}\"", "E_UNEXPECTED", 1);
        assert!(parse("").unwrap().is_empty()); assert!(parse("; only a comment").unwrap().is_empty());
        for value in ["1e-14", "2e+06", "6.1E-17", "-3.3e7", "1.e2"] { assert!(number(value)); }
        for value in ["e5", "1e", "1e+", "1e5x", "-", "."] { assert!(!number(value)); }
        assert_eq!(number_value("-.5"), Some(-0.5)); assert!(number_value("1e999").is_none());
        assert!(separator('(') && separator('\n') && !separator(':'));
        let problem = parse("(a\n(b]").unwrap_err();
        assert_eq!(problem.position, Some(Position { line: 2, col: 3 })); assert!(!problem.message.is_empty());
    }

    #[test]
    fn lexer_tolerates_later_tokens_and_attribute_normalizes_selectors() {
        let source = "(a \"bad\\q\" b)";
        let tokens = text_lexer::lex(source);
        let issue = tokens.iter().find_map(|token| token.issue.as_ref()).unwrap();
        assert_eq!((issue.code.as_str(), issue.start, issue.finish), ("E_UNEXPECTED", 7, 9));
        assert!(tokens.iter().any(|token| matches!(&token.kind, text_lexer::Kind::Word(text_lexer::Word::Symbol(value)) if value == "b") && token.start == 11));
        let problem = parse(source).unwrap_err();
        assert_eq!(problem.code, issue.code); assert_eq!(problem.span.as_ref().map(|span| i64::from(span.start)), Some(issue.start));
        assert!(text_lexer::lex("x").iter().any(|token| matches!(token.kind, text_lexer::Kind::End)));
        let root = one("[size :normal :owner point :type vec3]");
        let args: Vec<Syntax> = root.children().iter().map(|&id| root.child(id)).collect();
        let normalized = attribute_args("sop/attr", &args);
        assert_eq!(bytes(&normalized[1]), b"normal"); assert_eq!(word(&normalized[2]), "owner");
        assert_eq!(bytes(&normalized[3]), b"point"); assert_eq!(word(&normalized[4]), "type"); assert_eq!(bytes(&normalized[5]), b"vec3");
        assert_eq!(word(&attribute_args("unrelated", &normalized)[0]), "size");
        let root = one("[:owner point :type text]");
        let args: Vec<Syntax> = root.children().iter().map(|&id| root.child(id)).collect();
        let normalized = attribute_args("array/range", &args);
        assert!(matches!(normalized[1].node, Node::Sym(_))); assert_eq!(word(&normalized[1]), "point");
        assert!(matches!(normalized[3].node, Node::Sym(_))); assert_eq!(word(&normalized[3]), "text");
    }
    #[test]
    fn decimal_byte_escapes_preserve_all_bytes() {
        for byte in 0_u16..=255 { assert_eq!(bytes(&one(&format!("\"\\{byte:03}\""))), &[byte as u8]); }
        for source in ["\"\\256\"", "\"\\999\"", "\"\\01\"", "\"\\1ab\"", "\"\\12\"", "\"\\00\""] { assert!(parse(source).is_err(), "accepted {source}"); }
    }
}
