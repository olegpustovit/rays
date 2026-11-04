// Port of lib/flow/test_macro.ml, assertion for assertion.
use crate::lisp;
use crate::macros::{self, MAX_DEPTH, MAX_FORMS};
use crate::syntax::{self, Syntax};
use rays_diagnostic::Diagnostic;

fn parse(source: &str) -> Vec<Syntax> {
    syntax::parse(source).unwrap()
}

fn one(source: &str) -> Syntax {
    parse(source).remove(0)
}

// [count] and [range] are operators a binding could be named after: free only as a head.
fn known(head: bool, name: &str) -> bool {
    ["+", "*", "/", "-", "sop/merge", "sop/transform", "sop/box", "for", "let*"].contains(&name)
        || (head && ["range", "count"].contains(&name))
}

fn check(source: &str) -> Vec<Diagnostic> {
    macros::check(&one(source), &known)
}

fn contains(text: &str, d: &Diagnostic) -> bool {
    d.message.contains(text)
}

const RADIAL: &str = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))";
const ADD1: &str = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))";

fn expand(ms: &str, call: &str) -> String {
    match macros::expand(&parse(ms), &one(call)) {
        Ok(x) => lisp::flat(&x),
        Err(d) => panic!("{}", d.message),
    }
}

// A definition the checker refuses.
fn rejects(source: &str, code: &str, text: &str) {
    match check(source).as_slice() {
        [d] if d.code == code && contains(text, d) => {}
        ds => panic!("{source}: {:?}", ds.iter().map(|d| d.message.clone()).collect::<Vec<_>>()),
    }
}

// A call the expander refuses.
fn refuses(ms: &str, call: &str, code: &str, text: &str) {
    match macros::expand(&parse(ms), &one(call)) {
        Err(d) if d.code == code && contains(text, &d) => {}
        Err(d) => panic!("{}", d.message),
        Ok(x) => panic!("accepted: {}", lisp::flat(&x)),
    }
}

#[test]
fn hygiene_a_caller_names_arrive_only_through_holes() {
    assert!(check(RADIAL).is_empty());
    assert_eq!(
        expand(RADIAL, "(radial k 3 (sop/box))"),
        "(sop/merge (for [k (range 3)] (sop/transform (sop/box) :rotate [0 (* (/ k 3) 6.2832) 0])))"
    );
    assert_eq!(
        expand(RADIAL, "(radial j 2 (sop/box :size (+ j 1)))"),
        "(sop/merge (for [j (range 2)] (sop/transform (sop/box :size (+ j 1)) :rotate [0 (* (/ j 2) 6.2832) 0])))"
    );
    rejects("(defmacro m [x] `(+ ~x y))", "E_MACRO_CAPTURE", "would capture a name from the call site");
    rejects("(defmacro m [x] `(let* [tmp ~x] tmp))", "E_MACRO_CAPTURE", "write tmp# for a fresh name");
    assert!(check("(defmacro m [x] `(let* [tmp# ~x] tmp#))").is_empty());
    // A binder's type is not a name.
    assert!(check("(defmacro m [x] `(for [(p# : (array vec3)) ~x] p#))").is_empty());
    rejects("(defmacro m [x] `(+ ~x k.y))", "E_MACRO_CAPTURE", "k.y");
    rejects("(defmacro m [x] `(+ ~x 1#x#))", "E_MACRO_TEMPLATE", "not a valid fresh name");
}

#[test]
fn unquoting() {
    rejects("(defmacro m [x] `(+ ~x ~(+ 1 2)))", "E_MACRO_UNQUOTE", "macros unquote only their parameters");
    rejects("(defmacro m [x] `(+ ~x ~z))", "E_MACRO_UNQUOTE", "not one");
    rejects("(defmacro m [x & xs] `(+ ~x ~xs))", "E_MACRO_UNQUOTE", "splice the rest parameter with ~@xs");
    rejects("(defmacro m [x & xs] `(+ ~@x ~@xs))", "E_MACRO_UNQUOTE", "splices the rest parameter only");
    rejects("(defmacro m [x] `(+ ~@x))", "E_MACRO_UNQUOTE", "~@");
    rejects("(defmacro m [x] `(+ ~x `1))", "E_MACRO_TEMPLATE", "nested quoting");
    rejects("(defmacro m [x x] `(+ ~x 1))", "E_MACRO_PARAM", "repeated parameter");
    rejects("(defmacro m [& a b] `(+ 1 1))", "E_MACRO_PARAM", "rest parameter");
    rejects("(defmacro m [] )", "E_MACRO_SHAPE", "defmacro name");
    assert_eq!(expand("(defmacro all [& xs] `(+ 0 (+ ~@xs)))", "(all 1 2)"), "(+ 0 (+ 1 2))");
    assert_eq!(expand("(defmacro all [& xs] `(+ 0 (+ ~@xs)))", "(all)"), "(+ 0 (+))");
}

#[test]
fn fresh_names_are_deterministic_distinct_and_stepped() {
    let call = "(+ (add1 1) (add1 (add1 2)))";
    let a = expand(ADD1, call);
    assert_eq!(a, "(+ (let* [t__1 1] (+ t__1 1)) (let* [t__2 (let* [t__3 2] (+ t__3 1))] (+ t__2 1)))");
    assert_eq!(a, expand(ADD1, call));
    // t# is t__1: never the reserved name t.
    assert!(!a.is_empty() && !a.split(' ').any(|word| word == "t"));
    // The counter starts at 1 for each call and is shared by the nested steps.
    assert_eq!(expand(ADD1, "(add1 5)"), "(let* [t__1 5] (+ t__1 1))");

    // A stepper with one state reaches the same text as expand.
    let ms = parse(&format!("{RADIAL}{ADD1}"));
    let mut state = macros::state();
    let x = one("(radial k 2 (add1 1))");
    let s1 = macros::expand_once_with_state(&ms, &x, &mut state).unwrap();
    assert_eq!(
        lisp::flat(&s1),
        "(sop/merge (for [k (range 2)] (sop/transform (add1 1) :rotate [0 (* (/ k 2) 6.2832) 0])))"
    );
    let s2 = macros::expand_once_with_state(&ms, &s1, &mut state).unwrap();
    // OCaml `==`: a finished form is returned as it is; Rust has no physical identity, so compare values.
    assert_eq!(macros::expand_once_with_state(&ms, &s2, &mut state).unwrap(), s2);
    assert_eq!(lisp::flat(&s2), expand(&format!("{RADIAL}{ADD1}"), "(radial k 2 (add1 1))"));

    // The new forms take the span of the call.
    let call = parse("\n\n(add1 5)").remove(0);
    match macros::expand(&parse(ADD1), &call) {
        Ok(form) => assert_eq!(form.span, call.span),
        Err(_) => panic!("add1 expansion refused"),
    }
}

#[test]
fn limits_arity_and_depth() {
    assert!(MAX_DEPTH == 32 && MAX_FORMS == 5000);
    refuses("(defmacro m [x] `(m ~x))", "(m 2)", "E_MACRO_DEPTH", "32 nested expansions");
    let nested = format!("{}1{}", "(d ".repeat(12), ")".repeat(12));
    refuses("(defmacro d [x] `(+ ~x ~x))", &nested, "E_MACRO_SIZE", "5,000 forms");
    // Below the limit is fine.
    expand("(defmacro d [x] `(+ ~x ~x))", &format!("{}1{}", "(d ".repeat(5), ")".repeat(5)));
    refuses(RADIAL, "(radial k 2)", "E_MACRO_ARITY", "expects 3 arguments; got 2");
    refuses("(defmacro m [x & r] `(+ ~x ~@r))", "(m)", "E_MACRO_ARITY", "at least 1 argument; got 0");
}

#[test]
fn template_quoting_operator_capture_and_inert_forms() {
    // A template is quoted: there is no unquoted form.
    rejects("(defmacro twice [x] (+ x x))", "E_MACRO_TEMPLATE", "the template is quoted");
    // An operator a caller can bind is free as a head and a capture as an argument.
    assert!(check("(defmacro n [xs] `(count ~xs))").is_empty());
    rejects("(defmacro n [x] `(+ count ~x))", "E_MACRO_CAPTURE", "count would capture");
    // A macro call is left alone inside quotes and defmacro forms.
    assert_eq!(expand(ADD1, "(defmacro other [a] (+ a a))"), "(defmacro other [a] (+ a a))");
}

#[test]
fn macro_params_and_valid_names() {
    match macros::params(&one("(defmacro m [a b & rest] `(+ ~a ~b))")) {
        Ok((req, Some(rest))) => {
            assert_eq!(req, vec!["a".to_owned(), "b".to_owned()]);
            assert_eq!(rest, "rest");
        }
        _ => panic!("rest parameter shape"),
    }
    match macros::params(&one("(defmacro m [a b] `(+ ~a ~b))")) {
        Ok((req, None)) => assert_eq!(req, vec!["a".to_owned(), "b".to_owned()]),
        _ => panic!("required parameter shape"),
    }
    assert!(macros::valid_name("sort-by") && macros::valid_name("a1_b"));
    assert!(!macros::valid_name("A") && !macros::valid_name("1a"));
}
