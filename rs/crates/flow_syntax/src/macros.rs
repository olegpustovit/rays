use crate::lisp;
use crate::syntax::{Node, QuoteKind, Syntax};
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

fn parts(form: &Syntax) -> Option<(&str, &[Syntax], &Syntax)> {
    let Node::List(forms) = &form.node else { return None; };
    if forms.len() != 4 || !matches!(&forms[0].node, Node::Sym(head) if head == "defmacro") {
        return None;
    }
    let Node::Sym(name) = &forms[1].node else { return None; };
    let Node::Vec(parameters) = &forms[2].node else { return None; };
    Some((name, parameters, &forms[3]))
}

fn shape(form: &Syntax) -> Result<(&str, &[Syntax], &Syntax), Diagnostic> {
    parts(form).ok_or_else(|| fail(form, "E_MACRO_SHAPE", "A macro is (defmacro name [params] template)."))
}

fn params_inner(form: &Syntax) -> Result<(Vec<String>, Option<String>), Diagnostic> {
    let (name, parameters, _) = shape(form)?;
    let bad_rest = format!("Macro {name}: & is followed by exactly one rest parameter, at the end.");
    let mut required = Vec::new();
    let mut i = 0;
    while i < parameters.len() {
        match &parameters[i].node {
            Node::Sym(value) if value == "&" => {
                if i + 2 == parameters.len() {
                    if let Node::Sym(rest) = &parameters[i + 1].node {
                        if valid_name(rest) && !required.contains(rest) {
                            return Ok((required, Some(rest.clone())));
                        }
                    }
                }
                return Err(fail(form, "E_MACRO_PARAM", &bad_rest));
            }
            Node::Sym(parameter) if valid_name(parameter) && !required.contains(parameter) => {
                required.push(parameter.clone());
                i += 1;
            }
            _ => {
                return Err(fail(
                    form,
                    "E_MACRO_PARAM",
                    &format!("Macro {name}: invalid or repeated parameter {}.", lisp::flat(&parameters[i])),
                ));
            }
        }
    }
    Ok((required, None))
}

pub fn params(form: &Syntax) -> Result<(Vec<String>, Option<String>), Diagnostic> {
    params_inner(form)
}

fn quasi(form: &Syntax) -> Option<&Syntax> {
    match parts(form) {
        Some((_, _, Syntax { node: Node::Quote(QuoteKind::Quasi, body), .. })) => Some(body),
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
        forms: &[Syntax],
        name: &str,
        parameters: &[String],
        rest: Option<&str>,
        known: &dyn Fn(bool, &str) -> bool,
    ) -> Result<(), Diagnostic> {
        let mut i = 0;
        while i < forms.len() {
            if matches!(&forms[i].node, Node::Sym(symbol) if symbol == ":") && i + 1 < forms.len() {
                walk(&forms[i], false, name, parameters, rest, known)?;
                i += 2;
            } else {
                walk(&forms[i], false, name, parameters, rest, known)?;
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
                if let Node::Sym(parameter) = &child.node {
                    if !parameters.contains(parameter) { return Err(bad_unquote(form, name)); }
                    if rest == Some(parameter.as_str()) {
                        return Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: splice the rest parameter with ~@{parameter}.")));
                    }
                    Ok(())
                } else { Err(bad_unquote(form, name)) }
            }
            Node::Quote(QuoteKind::Splice, child) => {
                if matches!(&child.node, Node::Sym(parameter) if rest == Some(parameter.as_str())) {
                    Ok(())
                } else {
                    let suffix = rest.map_or(String::new(), |parameter| format!(" (~@{parameter})"));
                    Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: ~@ splices the rest parameter only{suffix}.")))
                }
            }
            Node::Quote(_, _) => Err(fail(form, "E_MACRO_TEMPLATE", &format!("Macro {name}: nested quoting is not supported."))),
            Node::List(forms) => {
                if let Some((first, rest_forms)) = forms.split_first() {
                    walk(first, true, name, parameters, rest, known)?;
                    walk_items(rest_forms, name, parameters, rest, known)
                } else { Ok(()) }
            }
            Node::Vec(forms) | Node::Map(forms) => walk_items(forms, name, parameters, rest, known),
        }
    }
    walk(body, false, name, &parameters, rest.as_deref(), known)
}

#[derive(Clone, Debug, Default)]
pub struct State {
    next_name: u64,
    size: usize,
}

pub fn state() -> State {
    State::default()
}

fn count(form: &Syntax) -> usize {
    let mut size = 1;
    match &form.node {
        Node::List(forms) | Node::Vec(forms) | Node::Map(forms) => {
            for child in forms { size += count(child); }
        }
        Node::Quote(_, child) => size += count(child),
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) => (),
    }
    size
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
fn with_node(form: &Syntax, node: Node, span: Span) -> Syntax {
    Syntax {
        id: form.id,
        node,
        span,
        notes: form.notes.clone(),
        meta: form.meta.clone(),
        tail: form.tail.clone(),
    }
}

fn expand_sequence(
    state: &mut State,
    fresh: &mut HashMap<String, String>,
    forms: &[Syntax],
    call: &Syntax,
    name: &str,
    environment: &HashMap<String, Syntax>,
    rest: Option<&str>,
    more: &[Syntax],
) -> Result<Vec<Syntax>, Diagnostic> {
    let mut result = Vec::with_capacity(forms.len());
    for form in forms {
        if let Node::Quote(QuoteKind::Splice, child) = &form.node {
            if let Node::Sym(parameter) = &child.node {
                if rest == Some(parameter.as_str()) {
                    result.extend_from_slice(more);
                    continue;
                }
            }
            return Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: ~@ splices the rest parameter only.")));
        }
        result.push(expand_template(state, fresh, form, call, name, environment, rest, more)?);
    }
    Ok(result)
}

fn expand_template(
    state: &mut State,
    fresh: &mut HashMap<String, String>,
    form: &Syntax,
    call: &Syntax,
    name: &str,
    environment: &HashMap<String, Syntax>,
    rest: Option<&str>,
    more: &[Syntax],
) -> Result<Syntax, Diagnostic> {
    let node = match &form.node {
        Node::Sym(symbol) => Node::Sym(fresh_symbol(state, fresh, symbol)),
        Node::Kw(keyword) => Node::Kw(keyword.clone()),
        Node::Num(number) => Node::Num(number.clone()),
        Node::Str(text) => Node::Str(text.clone()),
        Node::Quote(QuoteKind::Unquote, child) => {
            if let Node::Sym(parameter) = &child.node {
                if let Some(argument) = environment.get(parameter) { return Ok(argument.clone()); }
            }
            return Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: macros unquote only their parameters; {} is not one.", lisp::flat(form))));
        }
        Node::Quote(QuoteKind::Splice, _) => return Err(fail(form, "E_MACRO_UNQUOTE", &format!("Macro {name}: ~@ splices into a list."))),
        Node::Quote(_, _) => return Err(fail(form, "E_MACRO_TEMPLATE", &format!("Macro {name}: nested quoting is not supported."))),
        Node::List(forms) => Node::List(expand_sequence(state, fresh, forms, call, name, environment, rest, more)?),
        Node::Vec(forms) => Node::Vec(expand_sequence(state, fresh, forms, call, name, environment, rest, more)?),
        Node::Map(forms) => Node::Map(expand_sequence(state, fresh, forms, call, name, environment, rest, more)?),
    };
    Ok(with_node(form, node, call.span.clone()))
}

fn step(state: &mut State, definition: &Syntax, call: &Syntax) -> Result<Syntax, Diagnostic> {
    let (name, _, _) = shape(definition)?;
    let (required, rest) = params_inner(definition)?;
    let arguments = match &call.node { Node::List(forms) => &forms[1..], _ => &[] };
    let required_count = required.len();
    let argument_count = arguments.len();
    if if rest.is_some() { argument_count < required_count } else { argument_count != required_count } {
        return Err(fail(
            call,
            "E_MACRO_ARITY",
            &format!("{name} expects {}{} argument{}; got {argument_count}.", if rest.is_some() { "at least " } else { "" }, required_count, if required_count == 1 { "" } else { "s" }),
        ));
    }
    let mut environment = HashMap::with_capacity(required_count);
    for (parameter, argument) in required.iter().zip(arguments.iter()) {
        environment.insert(parameter.clone(), argument.clone());
    }
    let more = &arguments[required_count..];
    let Some(body) = quasi(definition) else {
        return Err(fail(definition, "E_MACRO_TEMPLATE", &format!("Macro {name}: the template is quoted: write `(…) with ~parameters.")));
    };
    expand_template(state, &mut HashMap::new(), body, call, name, &environment, rest.as_deref(), more)
}


fn inert(form: &Syntax) -> bool {
    matches!(&form.node, Node::List(forms) if forms.first().is_some_and(|head| matches!(&head.node, Node::Sym(name) if name == "defmacro")))
        || matches!(&form.node, Node::Quote(_, _))
}

fn call_of(macros: &[Syntax], form: &Syntax) -> Option<usize> {
    let head = crate::syntax::head(form)?;
    macros.iter().position(|definition| parts(definition).is_some_and(|(name, _, _)| name == head))
}

fn expand_children(state: &mut State, macros: &[Syntax], depth: usize, form: &Syntax) -> Result<Syntax, Diagnostic> {
    let node = match &form.node {
        Node::List(forms) => {
            let mut expanded = Vec::with_capacity(forms.len());
            for child in forms { expanded.push(expand_at(state, macros, depth, child)?); }
            Node::List(expanded)
        }
        Node::Vec(forms) => {
            let mut expanded = Vec::with_capacity(forms.len());
            for child in forms { expanded.push(expand_at(state, macros, depth, child)?); }
            Node::Vec(expanded)
        }
        Node::Map(forms) => {
            let mut expanded = Vec::with_capacity(forms.len());
            for child in forms { expanded.push(expand_at(state, macros, depth, child)?); }
            Node::Map(expanded)
        }
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) | Node::Quote(_, _) => return Ok(form.clone()),
    };
    Ok(with_node(form, node, form.span.clone()))
}

fn expand_at(state: &mut State, macros: &[Syntax], depth: usize, form: &Syntax) -> Result<Syntax, Diagnostic> {
    if inert(form) { return Ok(form.clone()); }
    if let Some(index) = call_of(macros, form) {
        let definition = &macros[index];
        let head = crate::syntax::head(form).unwrap_or("");
        if depth >= MAX_DEPTH {
            return Err(fail(form, "E_MACRO_DEPTH", &format!("Macro expansion exceeds {MAX_DEPTH} nested expansions (at {head}).")));
        }
        let expanded = step(state, definition, form)?;
        state.size += count(&expanded);
        if state.size > MAX_FORMS {
            return Err(fail(form, "E_MACRO_SIZE", &format!("Macro expansion of {head} exceeds 5,000 forms.")));
        }
        return expand_at(state, macros, depth + 1, &expanded);
    }
    expand_children(state, macros, depth, form)
}

pub fn expand(macros: &[Syntax], form: &Syntax) -> Result<Syntax, Diagnostic> {
    expand_at(&mut state(), macros, 0, form)
}

fn expand_once_at(
    state: &mut State,
    macros: &[Syntax],
    finished: &mut bool,
    form: &Syntax,
) -> Result<Syntax, Diagnostic> {
    if *finished || inert(form) { return Ok(form.clone()); }
    if let Some(index) = call_of(macros, form) {
        *finished = true;
        return step(state, &macros[index], form);
    }
    let node = match &form.node {
        Node::List(forms) => {
            let mut expanded = Vec::with_capacity(forms.len());
            for child in forms { expanded.push(expand_once_at(state, macros, finished, child)?); }
            Node::List(expanded)
        }
        Node::Vec(forms) => {
            let mut expanded = Vec::with_capacity(forms.len());
            for child in forms { expanded.push(expand_once_at(state, macros, finished, child)?); }
            Node::Vec(expanded)
        }
        Node::Map(forms) => {
            let mut expanded = Vec::with_capacity(forms.len());
            for child in forms { expanded.push(expand_once_at(state, macros, finished, child)?); }
            Node::Map(expanded)
        }
        Node::Sym(_) | Node::Kw(_) | Node::Num(_) | Node::Str(_) | Node::Quote(_, _) => return Ok(form.clone()),
    };
    Ok(with_node(form, node, form.span.clone()))
}

pub fn expand_once(macros: &[Syntax], form: &Syntax) -> Result<Syntax, Diagnostic> {
    expand_once_with_state(macros, form, &mut state())
}

pub fn expand_once_with_state(macros: &[Syntax], form: &Syntax, state: &mut State) -> Result<Syntax, Diagnostic> {
    let mut finished = false;
    let result = expand_once_at(state, macros, &mut finished, form)?;
    if finished { Ok(result) } else { Ok(form.clone()) }
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
    fn macro_parameter_shapes_and_names() {
        assert_eq!(params(&one("(defmacro m [a b & rest] `(+ ~a ~b))")).unwrap(), (vec!["a".to_owned(), "b".to_owned()], Some("rest".to_owned())));
        assert_eq!(params(&one("(defmacro m [a b] `(+ ~a ~b))")).unwrap(), (vec!["a".to_owned(), "b".to_owned()], None));
        assert!(valid_name("sort-by") && valid_name("a1_b"));
        assert!(!valid_name("A") && !valid_name("1a"));
    }
}
