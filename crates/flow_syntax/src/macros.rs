use crate::lisp;
use crate::syntax::{Node, QuoteKind, Syntax};
use crate::arena::{NameId, RangeId, SyntaxBuilder, SyntaxId};
use rays_diagnostic::{self as diagnostic, Diagnostic, Span};
use std::collections::HashMap;

pub const MAX_DEPTH: usize = 32;
pub const MAX_FORMS: usize = 5000;

pub fn valid_name(name: &str) -> bool {
    let bytes = name.as_bytes();
    matches!(bytes.first().copied(), Some(b'a'..=b'z'))
        && bytes[1..].iter().all(|byte| matches!(*byte, b'a'..=b'z' | b'0'..=b'9' | b'_' | b'-'))
}

fn fail(form: &Syntax, code: &str, message: &str) -> Diagnostic {
    diagnostic::error(
        code.to_owned(),
        message.to_owned(),
        Some(form.span.clone()),
        None,
        None,
        Vec::new(),
        Vec::new(),
    )
}

fn base(name: &str) -> &str {
    name.split_once('.').map_or(name, |(base, _)| base)
}

fn fresh_name(name: &str) -> bool {
    base(name).ends_with('#')
}

fn parts(form: &Syntax) -> Option<(&str, &[SyntaxId], SyntaxId)> {
    let Node::List(range) = form.node else { return None; };
    let forms = form.range(range);
    if forms.len() != 4 || crate::syntax::head(form) != Some("defmacro") { return None; }
    let Node::Sym(name) = form.arena().row(forms[1]).node else { return None; };
    let Node::Vec(parameters) = form.arena().row(forms[2]).node else { return None; };
    Some((form.name(name), form.range(parameters), forms[3]))
}

fn shape(form: &Syntax) -> Result<(&str, &[SyntaxId], SyntaxId), Diagnostic> {
    parts(form).ok_or_else(|| fail(form, "E_MACRO_SHAPE", "A macro is (defmacro name [params] template)."))
}

fn params_inner(form: &Syntax) -> Result<(Vec<String>, Option<String>), Diagnostic> {
    let (name, parameters, _) = shape(form)?;
    let bad_rest = format!("Macro {name}: & is followed by exactly one rest parameter, at the end.");
    let mut required = Vec::new();
    let mut i = 0;
    while i < parameters.len() {
        let parameter = form.child(parameters[i]);
        match &parameter.node {
            Node::Sym(value) if parameter.name(*value) == "&" => {
                if i + 2 == parameters.len() {
                    if let Node::Sym(rest) = form.arena().row(parameters[i + 1]).node {
                        let rest = form.name(rest);
                        if valid_name(rest) && !required.iter().any(|name| name == rest) {
                            return Ok((required, Some(rest.to_owned())));
                        }
                    }
                }
                return Err(fail(form, "E_MACRO_PARAM", &bad_rest));
            }
            Node::Sym(name) if valid_name(parameter.name(*name)) && !required.iter().any(|value| value == parameter.name(*name)) => {
                required.push(parameter.name(*name).to_owned());
                i += 1;
            }
            _ => {
                return Err(fail(
                    form,
                    "E_MACRO_PARAM",
                    &format!("Macro {name}: invalid or repeated parameter {}.", lisp::flat(&parameter)),
                ));
            }
        }
    }
    Ok((required, None))
}

pub fn params(form: &Syntax) -> Result<(Vec<String>, Option<String>), Diagnostic> {
    params_inner(form)
}

fn quasi(form: &Syntax) -> Option<Syntax> {
    let (_, _, body) = parts(form)?;
    match form.arena().row(body).node {
        Node::Quote(QuoteKind::Quasi, child) => Some(form.child(child)),
        _ => None,
    }
}

pub fn check(form: &Syntax, known: &dyn Fn(bool, &str) -> bool) -> Vec<Diagnostic> {
    match check_inner(form, known) {
        Ok(()) => Vec::new(),
        Err(problem) => vec![problem],
    }
}

fn check_inner(form: &Syntax, known: &dyn Fn(bool, &str) -> bool) -> Result<(), Diagnostic> {
    let (name, _, _) = shape(form)?;
    let (required, rest) = params_inner(form)?;
    let mut parameters = required;
    if let Some(rest) = &rest { parameters.push(rest.clone()); }
    let Some(body) = quasi(form) else {
        return Err(fail(form, "E_MACRO_TEMPLATE", &format!("Macro {name}: the template is quoted: write `(…) with ~parameters.")));
    };

    fn bad_unquote(form: &Syntax, name: &str) -> Diagnostic {
        fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: macros unquote only their parameters; {} is not one.", lisp::flat(form)))
    }
    fn walk_items(
        owner: &Syntax, forms: &[SyntaxId],
        name: &str,
        parameters: &[String],
        rest: Option<&str>,
        known: &dyn Fn(bool, &str) -> bool,
    ) -> Result<(), Diagnostic> {
        let mut i = 0;
        while i < forms.len() {
            let child = owner.child(forms[i]);
            if matches!(&child.node, Node::Sym(symbol) if child.name(*symbol) == ":") && i + 1 < forms.len() {
                walk(&child, false, name, parameters, rest, known)?;
                i += 2;
            } else {
                walk(&child, false, name, parameters, rest, known)?;
                i += 1;
            }
        }
        Ok(())
    }
    fn walk(
        form: &Syntax,
        head: bool,
        name: &str,
        parameters: &[String],
        rest: Option<&str>,
        known: &dyn Fn(bool, &str) -> bool,
    ) -> Result<(), Diagnostic> {
        match &form.node {
            Node::Sym(symbol) => {
                let symbol = form.name(*symbol);
                let base = base(symbol);
                if fresh_name(base) {
                    let stem = &base[..base.len() - 1];
                    if !valid_name(stem) {
                        return Err(fail(form, "E_MACRO_TEMPLATE", &format!("Macro {name}: {symbol} is not a valid fresh name.")));
                    }
                } else if base != ":" && !known(head, base) {
                    return Err(fail(
                        form,
                        "E_MACRO_CAPTURE",
                        &format!("Macro {name}: {symbol} would capture a name from the call site. Pass it as a parameter (~{base}) or write {base}# for a fresh name."),
                    ));
                }
                Ok(())
            }
            Node::Kw(_) | Node::Num(_) | Node::Str(_) => Ok(()),
            Node::Quote(QuoteKind::Unquote, child) => {
                let child = form.child(*child);
                if let Node::Sym(parameter) = child.node {
                    let parameter = child.name(parameter);
                    if !parameters.iter().any(|name| name == parameter) { return Err(bad_unquote(form, name)); }
                    if rest == Some(parameter) {
                        return Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: splice the rest parameter with ~@{parameter}.")));
                    }
                    Ok(())
                } else { Err(bad_unquote(form, name)) }
            }
            Node::Quote(QuoteKind::Splice, child) => {
                let child = form.child(*child);
                if matches!(&child.node, Node::Sym(parameter) if rest == Some(child.name(*parameter))) {
                    Ok(())
                } else {
                    let suffix = rest.map_or(String::new(), |parameter| format!(" (~@{parameter})"));
                    Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: ~@ splices the rest parameter only{suffix}.")))
                }
            }
            Node::Quote(_, _) => Err(fail(form, "E_MACRO_TEMPLATE", &format!("Macro {name}: nested quoting is not supported."))),
            Node::List(range) => {
                let forms = form.range(*range);
                if let Some((first, rest_forms)) = forms.split_first() {
                    walk(&form.child(*first), true, name, parameters, rest, known)?;
                    walk_items(form, rest_forms, name, parameters, rest, known)
                } else { Ok(()) }
            }
            Node::Vec(range) | Node::Map(range) => walk_items(form, form.range(*range), name, parameters, rest, known),
        }
    }
    walk(&body, false, name, &parameters, rest.as_deref(), known)
}

#[derive(Clone, Debug, Default)]
pub struct State {
    next_name: u64,
    size: usize,
}

pub fn state() -> State {
    State::default()
}

fn count(builder: &SyntaxBuilder, form: SyntaxId) -> usize {
    1 + match builder.row(form).node {
        Node::List(range) | Node::Vec(range) | Node::Map(range) =>
            builder.range_ids(range).iter().map(|child| count(builder, *child)).sum(),
        Node::Quote(_, child) => count(builder, child),
        _ => 0,
    }
}

fn fresh_symbol(state: &mut State, fresh: &mut HashMap<String, String>, symbol: &str) -> String {
    let stem = base(symbol);
    if !fresh_name(stem) { return symbol.to_owned(); }
    let replacement = if let Some(existing) = fresh.get(stem) {
        existing.clone()
    } else {
        state.next_name += 1;
        let generated = format!("{}__{}", &stem[..stem.len() - 1], state.next_name);
        fresh.insert(stem.to_owned(), generated.clone());
        generated
    };
    format!("{replacement}{}", &symbol[stem.len()..])
}
fn fail_at(builder: &SyntaxBuilder, form: SyntaxId, code: &str, message: &str) -> Diagnostic {
    diagnostic::error(code.to_owned(), message.to_owned(), Some(builder.row(form).span.clone()), None, None, Vec::new(), Vec::new())
}

fn with_node(builder: &mut SyntaxBuilder, form: SyntaxId, node: Node, span: Span) -> SyntaxId {
    let id = builder.row(form).id;
    let trivia = builder.trivia(form).cloned();
    builder.push(id, node, span, trivia)
}

fn parts_id(form: SyntaxId, builder: &SyntaxBuilder) -> Option<(NameId, RangeId, SyntaxId)> {
    let Node::List(range) = builder.row(form).node else { return None; };
    let forms = builder.range_ids(range);
    if forms.len() != 4 || head(builder, form) != Some("defmacro") { return None; }
    let Node::Sym(name) = builder.row(forms[1]).node else { return None; };
    let Node::Vec(params) = builder.row(forms[2]).node else { return None; };
    Some((name, params, forms[3]))
}

fn params_id(form: SyntaxId, name: &str, range: RangeId, builder: &SyntaxBuilder) -> Result<(Vec<String>, Option<String>), Diagnostic> {
    let parameters = builder.range_ids(range);
    let mut required = Vec::new();
    let mut i = 0;
    while i < parameters.len() {
        match builder.row(parameters[i]).node {
            Node::Sym(value) if builder.name(value) == "&" => {
                if i + 2 == parameters.len() {
                    if let Node::Sym(rest) = builder.row(parameters[i + 1]).node {
                        let rest = builder.name(rest);
                        if valid_name(rest) && !required.iter().any(|name| name == rest) { return Ok((required, Some(rest.to_owned()))); }
                    }
                }
                return Err(fail_at(builder, form, "E_MACRO_PARAM", &format!("Macro {name}: & is followed by exactly one rest parameter, at the end.")));
            }
            Node::Sym(parameter) if valid_name(builder.name(parameter)) && !required.iter().any(|name| name == builder.name(parameter)) => {
                required.push(builder.name(parameter).to_owned());
                i += 1;
            }
            _ => return Err(fail_at(builder, form, "E_MACRO_PARAM", &format!("Macro {name}: invalid or repeated parameter {}.", lisp::flat_id(parameters[i], builder)))),
        }
    }
    Ok((required, None))
}

fn expand_sequence(
    builder: &mut SyntaxBuilder, state: &mut State, fresh: &mut HashMap<String, String>,
    forms: RangeId, call: SyntaxId, name: &str,
    environment: &HashMap<String, SyntaxId>, rest: Option<&str>, more: &[SyntaxId],
) -> Result<Vec<SyntaxId>, Diagnostic> {
    let len = builder.range_ids(forms).len();
    let mut result = Vec::with_capacity(len);
    for index in 0..len {
        let form = builder.range_ids(forms)[index];
        if let Node::Quote(QuoteKind::Splice, child) = builder.row(form).node {
            if let Node::Sym(parameter) = builder.row(child).node {
                if rest == Some(builder.name(parameter)) {
                    result.extend_from_slice(more);
                    continue;
                }
            }
            return Err(fail_at(builder, form, "E_MACRO_UNQUOTE", &format!("Macro {name}: ~@ splices the rest parameter only.")));
        }
        result.push(expand_template(builder, state, fresh, form, call, name, environment, rest, more)?);
    }
    Ok(result)
}

fn expand_template(
    builder: &mut SyntaxBuilder, state: &mut State, fresh: &mut HashMap<String, String>,
    form: SyntaxId, call: SyntaxId, name: &str, environment: &HashMap<String, SyntaxId>,
    rest: Option<&str>, more: &[SyntaxId],
) -> Result<SyntaxId, Diagnostic> {
    let original = builder.row(form).node;
    let node = match original {
        Node::Sym(symbol) => {
            let name = fresh_symbol(state, fresh, builder.name(symbol));
            Node::Sym(builder.intern(&name))
        }
        Node::Kw(_) | Node::Num(_) | Node::Str(_) => original,
        Node::Quote(QuoteKind::Unquote, child) => {
            if let Node::Sym(parameter) = builder.row(child).node {
                if let Some(argument) = environment.get(builder.name(parameter)) { return Ok(*argument); }
            }
            return Err(fail_at(builder, form, "E_MACRO_UNQUOTE", &format!("Macro {name}: macros unquote only their parameters; {} is not one.", lisp::flat_id(form, builder))));
        }
        Node::Quote(QuoteKind::Splice, _) => return Err(fail_at(builder, form, "E_MACRO_UNQUOTE", &format!("Macro {name}: ~@ splices into a list."))),
        Node::Quote(_, _) => return Err(fail_at(builder, form, "E_MACRO_TEMPLATE", &format!("Macro {name}: nested quoting is not supported."))),
        Node::List(range) | Node::Vec(range) | Node::Map(range) => {
            let children = expand_sequence(builder, state, fresh, range, call, name, environment, rest, more)?;
            let range = builder.range(&children);
            match original { Node::List(_) => Node::List(range), Node::Vec(_) => Node::Vec(range), _ => Node::Map(range) }
        }
    };
    let span = builder.row(call).span.clone();
    Ok(with_node(builder, form, node, span))
}

fn step(builder: &mut SyntaxBuilder, state: &mut State, definition: SyntaxId, call: SyntaxId) -> Result<SyntaxId, Diagnostic> {
    let (name, parameters, template) = parts_id(definition, builder)
        .ok_or_else(|| fail_at(builder, definition, "E_MACRO_SHAPE", "A macro is (defmacro name [params] template)."))?;
    let name = builder.name(name).to_owned();
    let (required, rest) = params_id(definition, &name, parameters, builder)?;
    let arguments = match builder.row(call).node { Node::List(range) => &builder.range_ids(range)[1..], _ => &[] };
    let required_count = required.len();
    let argument_count = arguments.len();
    if if rest.is_some() { argument_count < required_count } else { argument_count != required_count } {
        return Err(fail_at(
            builder, call, "E_MACRO_ARITY",
            &format!("{name} expects {}{} argument{}; got {argument_count}.", if rest.is_some() { "at least " } else { "" }, required_count, if required_count == 1 { "" } else { "s" }),
        ));
    }
    let environment = required.into_iter().zip(arguments.iter().copied()).collect();
    let more = arguments[required_count..].to_vec();
    let Node::Quote(QuoteKind::Quasi, body) = builder.row(template).node else {
        return Err(fail_at(builder, definition, "E_MACRO_TEMPLATE", &format!("Macro {name}: the template is quoted: write `(…) with ~parameters.")));
    };
    expand_template(builder, state, &mut HashMap::new(), body, call, &name, &environment, rest.as_deref(), &more)
}

fn head(builder: &SyntaxBuilder, form: SyntaxId) -> Option<&str> {
    let Node::List(range) = builder.row(form).node else { return None; };
    let first = *builder.range_ids(range).first()?;
    let Node::Sym(name) = builder.row(first).node else { return None; };
    Some(builder.name(name))
}

fn inert(builder: &SyntaxBuilder, form: SyntaxId) -> bool {
    head(builder, form) == Some("defmacro") || matches!(builder.row(form).node, Node::Quote(_, _))
}

fn call_of(builder: &SyntaxBuilder, macros: &[SyntaxId], form: SyntaxId) -> Option<usize> {
    let head = head(builder, form)?;
    macros.iter().position(|definition| parts_id(*definition, builder).is_some_and(|(name, _, _)| builder.name(name) == head))
}

fn expand_at(
    builder: &mut SyntaxBuilder, state: &mut State, macros: &[SyntaxId], depth: usize,
    once: bool, finished: &mut bool, form: SyntaxId,
) -> Result<SyntaxId, Diagnostic> {
    if (once && *finished) || inert(builder, form) { return Ok(form); }
    if let Some(index) = call_of(builder, macros, form) {
        if !once && depth >= MAX_DEPTH {
            return Err(fail_at(builder, form, "E_MACRO_DEPTH", &format!("Macro expansion exceeds {MAX_DEPTH} nested expansions (at {}).", head(builder, form).unwrap_or(""))));
        }
        let expanded = step(builder, state, macros[index], form)?;
        *finished = true;
        if once { return Ok(expanded); }
        state.size += count(builder, expanded);
        if state.size > MAX_FORMS {
            return Err(fail_at(builder, form, "E_MACRO_SIZE", &format!("Macro expansion of {} exceeds 5,000 forms.", head(builder, form).unwrap_or(""))));
        }
        return expand_at(builder, state, macros, depth + 1, once, finished, expanded);
    }
    let node = builder.row(form).node;
    let range = match node {
        Node::List(range) | Node::Vec(range) | Node::Map(range) => range,
        _ => return Ok(form),
    };
    let len = builder.range_ids(range).len();
    let mut children = None;
    for index in 0..len {
        let child = builder.range_ids(range)[index];
        let expanded = expand_at(builder, state, macros, depth, once, finished, child)?;
        if expanded != child && children.is_none() {
            let mut rewritten = Vec::with_capacity(len);
            rewritten.extend_from_slice(&builder.range_ids(range)[..index]);
            children = Some(rewritten);
        }
        if let Some(children) = &mut children { children.push(expanded); }
    }
    let Some(children) = children else { return Ok(form); };
    let range = builder.range(&children);
    let node = match node { Node::List(_) => Node::List(range), Node::Vec(_) => Node::Vec(range), _ => Node::Map(range) };
    let span = builder.row(form).span.clone();
    Ok(with_node(builder, form, node, span))
}

fn run(macros: &[Syntax], form: &Syntax, state: &mut State, once: bool) -> Result<Syntax, Diagnostic> {
    if macros.is_empty() { return Ok(form.clone()); }
    let mut builder = SyntaxBuilder::new();
    let root = builder.import(form);
    let macros: Vec<_> = macros.iter().map(|definition| builder.import(definition)).collect();
    let result = expand_at(&mut builder, state, &macros, 0, once, &mut false, root)?;
    if result == root { Ok(form.clone()) } else { Ok(builder.freeze().handle(result)) }
}

pub fn expand(macros: &[Syntax], form: &Syntax) -> Result<Syntax, Diagnostic> {
    run(macros, form, &mut state(), false)
}

pub fn expand_once(macros: &[Syntax], form: &Syntax) -> Result<Syntax, Diagnostic> {
    expand_once_with_state(macros, form, &mut state())
}

pub fn expand_once_with_state(macros: &[Syntax], form: &Syntax, state: &mut State) -> Result<Syntax, Diagnostic> {
    run(macros, form, state, true)
}

pub fn expand_into(macros: &[SyntaxId], form: SyntaxId, builder: &mut SyntaxBuilder) -> Result<SyntaxId, Diagnostic> {
    if macros.is_empty() { return Ok(form); }
    expand_at(builder, &mut state(), macros, 0, false, &mut false, form)
}

#[cfg(test)]
mod tests {
    use super::{check, expand, expand_once_with_state, params, state, valid_name, MAX_DEPTH, MAX_FORMS};
    use crate::lisp;
    use crate::syntax::{self, Syntax};

    fn parse(source: &str) -> Vec<Syntax> {
        syntax::parse(source).unwrap()
    }

    fn one(source: &str) -> Syntax {
        parse(source).remove(0)
    }

    fn known(head: bool, name: &str) -> bool {
        ["+", "*", "/", "-", "sop/merge", "sop/transform", "sop/box", "for", "let*"].contains(&name)
            || (head && ["range", "count"].contains(&name))
    }

    fn rejected(source: &str, code: &str, excerpt: &str) {
        let errors = check(&one(source), &known);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, code);
        assert!(errors[0].message.contains(excerpt), "{}", errors[0].message);
    }

    fn expanded(definitions: &str, call: &str) -> String {
        lisp::flat(&expand(&parse(definitions), &one(call)).unwrap())
    }

    fn refuses(definitions: &str, call: &str, code: &str, excerpt: &str) {
        let problem = expand(&parse(definitions), &one(call)).unwrap_err();
        assert_eq!(problem.code, code);
        assert!(problem.message.contains(excerpt), "{}", problem.message);
    }

    #[test]
    fn hygiene_and_checker_errors() {
        let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))";
        assert!(check(&one(radial), &known).is_empty());
        assert_eq!(
            expanded(radial, "(radial k 3 (sop/box))"),
            "(sop/merge (for [k (range 3)] (sop/transform (sop/box) :rotate [0 (* (/ k 3) 6.2832) 0])))"
        );
        assert_eq!(
            expanded(radial, "(radial j 2 (sop/box :size (+ j 1)))"),
            "(sop/merge (for [j (range 2)] (sop/transform (sop/box :size (+ j 1)) :rotate [0 (* (/ j 2) 6.2832) 0])))"
        );
        rejected("(defmacro m [x] `(+ ~x y))", "E_MACRO_CAPTURE", "would capture a name from the call site");
        rejected("(defmacro m [x] `(let* [tmp ~x] tmp))", "E_MACRO_CAPTURE", "write tmp# for a fresh name");
        assert!(check(&one("(defmacro m [x] `(let* [tmp# ~x] tmp#))"), &known).is_empty());
        assert!(check(&one("(defmacro m [x] `(for [(p# : (array vec3)) ~x] p#))"), &known).is_empty());
        rejected("(defmacro m [x] `(+ ~x k.y))", "E_MACRO_CAPTURE", "k.y");
        rejected("(defmacro m [x] `(+ ~x 1#x#))", "E_MACRO_TEMPLATE", "not a valid fresh name");
        rejected("(defmacro n [x] `(+ count ~x))", "E_MACRO_CAPTURE", "count would capture");
        assert!(check(&one("(defmacro n [xs] `(count ~xs))"), &known).is_empty());
    }

    #[test]
    fn unquote_splicing_params_and_inert_forms() {
        rejected("(defmacro m [x] `(+ ~x ~(+ 1 2)))", "E_MACRO_UNQUOTE", "macros unquote only their parameters");
        rejected("(defmacro m [x] `(+ ~x ~z))", "E_MACRO_UNQUOTE", "not one");
        rejected("(defmacro m [x & xs] `(+ ~x ~xs))", "E_MACRO_UNQUOTE", "splice the rest parameter with ~@xs");
        rejected("(defmacro m [x & xs] `(+ ~@x ~@xs))", "E_MACRO_UNQUOTE", "splices the rest parameter only");
        rejected("(defmacro m [x] `(+ ~@x))", "E_MACRO_UNQUOTE", "~@");
        rejected("(defmacro m [x] `(+ ~x `1))", "E_MACRO_TEMPLATE", "nested quoting");
        rejected("(defmacro twice [x] (+ x x))", "E_MACRO_TEMPLATE", "the template is quoted");
        rejected("(defmacro m [x x] `(+ ~x 1))", "E_MACRO_PARAM", "repeated parameter");
        rejected("(defmacro m [& a b] `(+ 1 1))", "E_MACRO_PARAM", "rest parameter");
        assert_eq!(check(&one("(defmacro m [] )"), &known)[0].code, "E_MACRO_SHAPE");
        assert_eq!(expanded("(defmacro all [& xs] `(+ 0 (+ ~@xs)))", "(all 1 2)"), "(+ 0 (+ 1 2))");
        assert_eq!(expanded("(defmacro all [& xs] `(+ 0 (+ ~@xs)))", "(all)"), "(+ 0 (+))");
        assert_eq!(expanded("(defmacro other [a] (+ a a))", "(defmacro other [a] (+ a a))"), "(defmacro other [a] (+ a a))");
    }

    #[test]
    fn fresh_names_stepping_arity_and_limits() {
        let definition = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))";
        let call = "(+ (add1 1) (add1 (add1 2)))";
        let expected = "(+ (let* [t__1 1] (+ t__1 1)) (let* [t__2 (let* [t__3 2] (+ t__3 1))] (+ t__2 1)))";
        assert_eq!(expanded(definition, call), expected);
        assert_eq!(expanded(definition, call), expected);
        assert_eq!(expanded(definition, "(add1 5)"), "(let* [t__1 5] (+ t__1 1))");

        let definitions = parse("(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))");
        let call = one("(radial k 2 (add1 1))");
        let mut expansion_state = state();
        let once = expand_once_with_state(&definitions, &call, &mut expansion_state).unwrap();
        assert_eq!(lisp::flat(&once), "(sop/merge (for [k (range 2)] (sop/transform (add1 1) :rotate [0 (* (/ k 2) 6.2832) 0])))");
        let twice = expand_once_with_state(&definitions, &once, &mut expansion_state).unwrap();
        let finished = expand_once_with_state(&definitions, &twice, &mut expansion_state).unwrap();
        assert_eq!(lisp::flat(&finished), lisp::flat(&twice));
        assert_eq!(lisp::flat(&finished), expanded("(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))", "(radial k 2 (add1 1))"));
        let call = one("\n\n(add1 5)");
        let result = expand(&parse(definition), &call).unwrap();
        assert_eq!(result.span, call.span);

        assert_eq!((MAX_DEPTH, MAX_FORMS), (32, 5000));
        refuses("(defmacro m [x] `(m ~x))", "(m 2)", "E_MACRO_DEPTH", "32 nested expansions");
        let nested = format!("{}1{}", "(d ".repeat(12), ")".repeat(12));
        refuses("(defmacro d [x] `(+ ~x ~x))", &nested, "E_MACRO_SIZE", "5,000 forms");
        let below_limit = format!("{}1{}", "(d ".repeat(5), ")".repeat(5));
        expanded("(defmacro d [x] `(+ ~x ~x))", &below_limit);
        refuses("(defmacro m [a b c] `(+ ~a ~b ~c))", "(m 1 2)", "E_MACRO_ARITY", "expects 3 arguments; got 2");
        refuses("(defmacro radial [i n body] `(sop/merge ~body))", "(radial k 2)", "E_MACRO_ARITY", "expects 3 arguments; got 2");
        refuses("(defmacro m [x & rest] `(+ ~x ~@rest))", "(m)", "E_MACRO_ARITY", "at least 1 argument; got 0");
    }

    #[test]
    fn packed_expansion_preserves_authored_ids_and_shared_arguments() {
        let definition = one("(defmacro twice [x] `(+ ~x ~x))");
        let call = one("(twice [1 2])");
        let argument = call.child(call.children()[1]);
        let template = super::quasi(&definition).unwrap();
        let result = expand_once_with_state(std::slice::from_ref(&definition), &call, &mut state()).unwrap();
        assert_eq!(result.id, template.id);
        assert_eq!(result.span, call.span);
        let children = result.children();
        assert_eq!(children[1], children[2]);
        assert_eq!(result.child(children[1]).id, argument.id);
        assert_eq!(result.child(children[1]).span, argument.span);
        let unchanged = super::expand(&[], &result).unwrap();
        assert_eq!(unchanged.identity(), result.identity());
        let mut builder = super::SyntaxBuilder::new();
        let definition = builder.import(&definition);
        let call = builder.import(&call);
        let expanded = super::expand_into(&[definition], call, &mut builder).unwrap();
        assert_eq!(lisp::flat_id(expanded, &builder), lisp::flat(&result));
        assert_eq!(builder.row(expanded).id, template.id);
    }

    #[test]
    fn macro_parameter_shapes_and_names() {
        assert_eq!(params(&one("(defmacro m [a b & rest] `(+ ~a ~b))")).unwrap(), (vec!["a".to_owned(), "b".to_owned()], Some("rest".to_owned())));
        assert_eq!(params(&one("(defmacro m [a b] `(+ ~a ~b))")).unwrap(), (vec!["a".to_owned(), "b".to_owned()], None));
        assert!(valid_name("sort_by") && valid_name("a1_b"));
        assert!(!valid_name("A") && !valid_name("1a"));
    }
}
