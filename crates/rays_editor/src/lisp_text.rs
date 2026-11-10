//! The workspace Lisp as the text area's `pxui::ui::Language`: an error-tolerant lexer colours
//! comments, strings, numbers, keywords, `^:meta` and the head of every form, and brackets by depth
//! (an unmatched one in the invalid colour); Enter indents as a Lisp editor does: a body form
//! (`let*`, `for`, `defn`, ...) two columns in from its paren, a call under its first argument, a
//! vector under its first element. Over the same tokens and a `Vocab` of the catalog it completes the
//! token at the caret (kinds, parameters, choices, forms, operators, bindings in scope, ranked),
//! describes the token under the pointer and finds the number a drag changes.
//!
//! ponytail: the text is lexed once per frame per need (colours, pairs, indent, completions); the
//! workspace texts are a few hundred lines.

use std::any::Any;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

use flow_check::lisp_scan::{self as scan, Kind, Token};
use flow_check::{op, workspace};
use flow_sop::catalog::Descriptor;
use pxui::theme::{self, Tokens as Theme};
use pxui::ui::{CodeContext, Completion, Language};
use sop::edit_graph::InputRequirement;

pub use flow_syntax::text_lexer::{number, separator};

pub type Lexed = (Vec<Token>, Vec<(usize, usize)>, Vec<(char, usize)>);

static LEXED: OnceLock<lru::IdentityList> = OnceLock::new();

fn lexed_cache() -> &'static lru::IdentityList {
    LEXED.get_or_init(|| {
        lru::IdentityList::new(
            lru::cache("editor.lisp_text"),
            4,
            Arc::new(|key: &lru::Key, other: &(dyn Any + Send + Sync)| {
                matches!((key.as_any().downcast_ref::<String>(), other.downcast_ref::<String>()), (Some(a), Some(b)) if a == b)
            }),
        )
    })
}

/// The lexing of the last four texts asked for, most recent first: a frame asks for the same text
/// several times and the next frame asks again, so a text is lexed once per version. A hit moves to
/// the front so the long text outlives the caret prefixes the completions lex. The result is shared,
/// never mutated by readers.
pub fn lex(text: &str) -> Arc<Lexed> {
    let key = text.to_string();
    if let Some(found) = lexed_cache().find_by(&key) {
        if let Ok(tokens) = found.downcast::<Lexed>() {
            return tokens;
        }
    }
    let tokens: Arc<Lexed> = Arc::new(scan::lex(text));
    let bytes = text.len() + tokens.0.len() * std::mem::size_of::<Token>() + tokens.1.len() * 16 + tokens.2.len() * 16;
    lexed_cache().add(lru::Key::from_value(Arc::new(key)), tokens.clone(), bytes as i64);
    tokens
}

fn sub(text: &str, from: usize, to: usize) -> &str {
    let (mut from, mut to) = (from.min(text.len()), to.min(text.len()));
    while !text.is_char_boundary(from) {
        from -= 1;
    }
    while !text.is_char_boundary(to) {
        to += 1;
    }
    if from > to { "" } else { &text[from..to] }
}

fn word(text: &str, t: &Token) -> String {
    scan::word(text, t)
}

pub fn colorize_now(theme: &Theme, text: &str) -> Vec<(i64, i64, pxui::geom::color::T)> {
    // kit rev 3: forms in the float blue, a kind's namespace in the vector purple, keywords in the
    // bool red, numbers green, strings in the record green; brackets and comments recede to ink-3
    let ports = rays_graph::scope_paint::ports(theme);
    let quiet = theme::ink_3(theme);
    let lexed = lex(text);
    let tokens = &lexed.0;
    let mut out = Vec::new();
    for (i, t) in tokens.iter().enumerate() {
        let span = |color| vec![(t.start as i64, t.stop as i64, color)];
        // the name of a typed input, `(floors : int 5)`, is a binding, not a form: ink
        let declares = i + 1 < tokens.len() && tokens[i + 1].kind == Kind::Sym && sub(text, tokens[i + 1].start, tokens[i + 1].stop) == ":";
        let spans = match t.kind {
            Kind::Comment | Kind::Meta => span(quiet),
            Kind::Str => span(ports.record),
            Kind::Num => span(ports.int),
            Kind::Kw => span(ports.bool),
            Kind::Head if declares => vec![],
            Kind::Head => match text.as_bytes()[t.start..].iter().position(|b| *b == b'/').map(|p| p + t.start) {
                Some(slash) if slash + 1 < t.stop => vec![(t.start as i64, slash as i64 + 1, ports.vec3)],
                _ => span(ports.float),
            },
            Kind::Sym => vec![],
            Kind::Open(_) | Kind::Close(_) => span(quiet),
            Kind::Unmatched => span(theme::invalid()),
        };
        out.extend(spans);
    }
    out
}

pub fn brackets(text: &str) -> Vec<(i64, i64)> {
    lex(text).1.iter().map(|(a, b)| (*a as i64, *b as i64)).collect()
}

/// One kind of the catalog as the text editor sees it: the qualified name a head writes, its context
/// (the namespace), its label and category, the slots and the parameters.
#[derive(Clone)]
pub struct Entry {
    pub qualified: String,
    pub context: String,
    pub title: String,
    pub category: Vec<String>,
    /// name, required
    pub slots: Vec<(String, bool)>,
    pub fields: Vec<param::FieldView>,
}

#[derive(Clone)]
pub struct Vocab {
    pub kinds: Vec<Entry>,
    pub by_name: HashMap<String, usize>,
    pub ops: Vec<op::Op>,
}

pub fn vocab(ops: Vec<op::Op>, descriptors: &[Descriptor]) -> Vocab {
    let mut by_name: HashMap<String, usize> = HashMap::new();
    let mut kinds: Vec<Entry> = Vec::new();
    for d in descriptors {
        if by_name.contains_key(&d.qualified) {
            continue;
        }
        let context = match flow_syntax::context::of_qualified(&d.qualified) {
            Some(context) => flow_syntax::context::name(context).to_string(),
            None => match d.qualified.find('/') {
                Some(i) => d.qualified[..i].to_string(),
                None => "sop".to_string(),
            },
        };
        by_name.insert(d.qualified.clone(), kinds.len());
        kinds.push(Entry {
            qualified: d.qualified.clone(),
            context,
            title: d.label.clone(),
            category: d.category.clone(),
            slots: d.slots.iter().map(|(n, r)| (n.clone(), *r == InputRequirement::Required)).collect(),
            fields: d.fields.clone(),
        });
    }
    Vocab { kinds, by_name, ops }
}

pub fn empty_vocab() -> Vocab {
    vocab(Vec::new(), &[])
}

/// Language forms use the same signatures and descriptions as CLI lookup.
pub fn specials() -> Vec<(String, String)> {
    flow_check::registry::FORMS.iter().chain(flow_check::registry::DIRECTIVES)
        .map(|(name, args, doc, _)| ((*name).to_owned(), format!("({name} {args}) · {doc}"))).collect()
}

pub fn constants() -> Vec<(&'static str, &'static str)> {
    vec![
        ("t", "time in seconds \u{b7} a binding reading t recooks every frame"),
        ("pi", "3.14159..."),
        ("true", "boolean true"),
        ("false", "boolean false"),
        ("nil", "empty geometry \u{b7} an unconnected slot"),
    ]
}

pub fn contexts() -> Vec<String> {
    flow_syntax::context::all().into_iter().map(|c| flow_syntax::context::name(c).to_string()).collect()
}

pub fn special_keywords() -> Vec<(String, String)> {
    vec![
        (":context".to_string(), format!("the graph's context: {}", contexts().join(", "))),
        (":skip".to_string(), ":skip [i ...] \u{b7} iteration tuples a for or scene/merge leaves out".to_string()),
        (":else".to_string(), "the branch when no test holds".to_string()),
        (":keys".to_string(), "{:keys [a b]} \u{b7} destructure a record's fields".to_string()),
        (":bypass".to_string(), "^:bypass (call ...) \u{b7} the call passes its first input through".to_string()),
    ]
}

fn special_keyword(name: &str) -> Option<String> {
    special_keywords().into_iter().find(|(k, _)| k == name).map(|(_, d)| d)
}

fn ws_context(name: &str) -> flow_syntax::context::Context {
    flow_syntax::context::of_string(name).unwrap_or_else(|_| flow_syntax::context::sop())
}

pub fn ops_of(vocab: &Vocab, context: &str) -> Vec<(String, workspace::OpSignature)> {
    op::of_context(&vocab.ops, &ws_context(context))
        .into_iter()
        .map(|o| {
            let s = workspace::OpSignature { pos: o.signature.pos.clone(), opt: o.signature.opt.clone(), rest: o.signature.rest.clone(), kw: o.signature.kw.clone() };
            (o.name.clone(), s)
        })
        .collect()
}

fn ty_name(ty: &flow_syntax::ty::Ty) -> String {
    flow_syntax::ty::to_string(ty)
}

pub fn op_doc(name: &str, s: &workspace::OpSignature) -> String {
    let mut parts: Vec<String> = vec![name.to_string()];
    parts.extend(s.pos.iter().map(|(n, t)| format!("{n}:{}", ty_name(t))));
    parts.extend(s.opt.iter().map(|(n, t)| format!("[{n}:{}]", ty_name(t))));
    if let Some((n, t)) = &s.rest {
        parts.push(format!("{n}:{}...", ty_name(t)));
    }
    parts.extend(s.kw.iter().map(|(n, t)| format!(":{n} {}", ty_name(t))));
    format!("({}) \u{b7} built-in operator", parts.join(" "))
}

pub fn kind_doc(k: &Entry) -> String {
    let slots = if k.slots.is_empty() {
        String::new()
    } else {
        format!(" \u{b7} inputs {}", k.slots.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>().join(", "))
    };
    let params = match k.fields.len() {
        0 => String::new(),
        n => format!(" \u{b7} {n} parameter{}", if n == 1 { "" } else { "s" }),
    };
    let category = if k.category.is_empty() { String::new() } else { format!(" \u{b7} {}", k.category.join(" / ")) };
    format!("{}{category}{slots}{params}", k.title)
}

fn value_text(v: &param::Value) -> String {
    match v {
        param::Value::Bool(b) => b.to_string(),
        param::Value::Int(i) => i.to_string(),
        param::Value::Float(f) => cfmt::g(*f, 6),
        param::Value::Text(s) => format!("\"{}\"", String::from_utf8_lossy(s)),
        param::Value::Choice(s) => format!("\"{s}\""),
    }
}

fn kind_text(f: &param::FieldView) -> String {
    match &f.kind {
        param::KindView::Toggle => "bool".to_string(),
        param::KindView::Integer(r) => format!("int {}..{}", r.soft_min, r.soft_max),
        param::KindView::Floating(r) => format!("float {}..{}", cfmt::g(r.soft_min, 6), cfmt::g(r.soft_max, 6)),
        param::KindView::Text => "text".to_string(),
        param::KindView::Choice(options) => format!("one of {}", options.join(", ")),
    }
}

fn field_doc(f: &param::FieldView) -> String {
    let folder = if f.folder.is_empty() { String::new() } else { format!(" \u{b7} {}", f.folder.join(" / ")) };
    let description = match &f.description {
        Some(d) => format!(" \u{b7} {d}"),
        None => String::new(),
    };
    format!("{}{folder} \u{b7} {} \u{b7} default {}{description}", f.label, kind_text(f), value_text(&f.default))
}

/// The keyword arguments a kind takes, as (name, type, doc, primary): a vec3 group once, before its
/// components.
pub fn keywords_of(k: &Entry) -> Vec<(String, String, String, bool)> {
    let mut out = Vec::new();
    for group in param::group_vec3(&k.fields, false).unwrap_or_default() {
        let Some(f) = group.first() else { continue };
        match &f.vec3 {
            Some((group_name, _)) => {
                // the group is named by its first component's label without the axis
                let title = if f.label.len() > 2 && f.label.as_bytes()[f.label.len() - 2] == b' ' {
                    sub(&f.label, 0, f.label.len() - 2).to_string()
                } else {
                    f.label.clone()
                };
                let folder = if f.folder.is_empty() { String::new() } else { format!(" \u{b7} {}", f.folder.join(" / ")) };
                let description = match &f.description {
                    Some(d) => format!(" \u{b7} {d}"),
                    None => String::new(),
                };
                out.push((group_name.clone(), "vec3".to_string(), format!("{title}{folder} \u{b7} [x y z]{description}"), f.primary));
            }
            None => out.push((f.name.clone(), kind_text(f), field_doc(f), f.primary)),
        }
    }
    for f in &k.fields {
        if f.vec3.is_some() {
            out.push((f.name.clone(), kind_text(f), field_doc(f), false));
        }
    }
    out
}

fn usage(text: &str, tokens: &[Token], name: &str) -> i64 {
    tokens.iter().filter(|t| t.kind == Kind::Head && word(text, t) == name).count() as i64
}

/// How well `name` matches the typed `prefix`: 3 a prefix, 2 a prefix of a word after _ or /, 1 a
/// subsequence, 0 none.
pub fn quality(prefix: &str, name: &str) -> i64 {
    let p = prefix.to_ascii_lowercase();
    let n = name.to_ascii_lowercase();
    if p.is_empty() || n.starts_with(&p) {
        3
    } else if n.split('_').any(|w| w.starts_with(&p)) || n.split('/').any(|w| w.starts_with(&p)) {
        2
    } else if pxui::ui::fuzzy_match(&p, &n) {
        1
    } else {
        0
    }
}

#[derive(Clone, Debug)]
pub struct Candidate {
    pub name: String,
    pub insert: String,
    pub detail: String,
    pub doc: String,
    pub group: i64,
    pub uses: i64,
}

/// Best match first, then the group (kinds before forms before operators; primary parameters first),
/// then the names the text already uses, then the alphabet; at most 48.
pub fn rank(prefix: &str, candidates: Vec<Candidate>) -> Vec<Candidate> {
    let mut scored: Vec<(i64, Candidate)> = candidates
        .into_iter()
        .filter_map(|c| match quality(prefix, &c.name) {
            0 => None,
            q => Some((q, c)),
        })
        .collect();
    scored.sort_by(|(qa, a), (qb, b)| {
        qb.cmp(qa).then(a.group.cmp(&b.group)).then(b.uses.cmp(&a.uses)).then_with(|| a.name.as_bytes().cmp(b.name.as_bytes()))
    });
    scored.into_iter().take(48).map(|(_, c)| c).collect()
}

/// What the document knows beyond the text shown: every graph, the material graphs, the cameras of
/// the scene and the names of the layouts (the values of `:material`, `:camera`, `:active`).
#[derive(Clone, Debug, Default)]
pub struct Names {
    pub graphs: Vec<String>,
    pub materials: Vec<String>,
    pub cameras: Vec<String>,
    pub layouts: Vec<String>,
}

pub fn no_names() -> Names {
    Names::default()
}

const VALUE_KEYWORDS: [&str; 3] = [":material", ":camera", ":active"];

fn mk(group: i64, uses: i64, insert: Option<String>, name: &str, detail: &str, doc: &str) -> Candidate {
    Candidate { name: name.to_string(), insert: insert.unwrap_or_else(|| name.to_string()), detail: detail.to_string(), doc: doc.to_string(), group, uses }
}

fn kind_of<'a>(vocab: &'a Vocab, context: &str, name: &str) -> Option<&'a Entry> {
    vocab
        .by_name
        .get(name)
        .or_else(|| vocab.by_name.get(&format!("{context}/{name}")))
        .map(|i| &vocab.kinds[*i])
}

pub fn complete(names: &Names, imports: &[(String, String)], vocab: &Vocab, text: &str, caret: usize) -> Vec<Completion> {
    let mut caret = caret.min(text.len());
    while !text.is_char_boundary(caret) {
        caret -= 1;
    }
    let bytes = text.as_bytes();
    // the token being typed ends at the caret: the text before it says what it is
    let before_lexed = lex(&text[..caret]);
    let (before, _, open_stack) = (&before_lexed.0, &before_lexed.1, &before_lexed.2);
    let last = before.last();
    let current: Option<&Token> = match last {
        Some(t) if t.stop == caret && matches!(t.kind, Kind::Sym | Kind::Kw | Kind::Head | Kind::Meta | Kind::Num | Kind::Str) => Some(t),
        _ => None,
    };
    let head_position = match (current, last) {
        (Some(t), _) if t.kind == Kind::Head => true,
        (None, Some(t)) if matches!(t.kind, Kind::Unmatched | Kind::Open(_)) && t.stop == caret && t.stop > 0 && bytes[t.stop - 1] == b'(' => true,
        _ => false,
    };
    let after_value_keyword = match last {
        Some(t) if t.kind == Kind::Kw => t.stop < caret && VALUE_KEYWORDS.contains(&word(text, t).as_str()),
        _ => false,
    };
    if current.is_none() && !head_position && !after_value_keyword {
        return Vec::new();
    }
    // the whole text, so the brackets around the caret are matched
    let whole = lex(text);
    let tokens = &whole.0;
    let prefix = match current {
        Some(t) if t.kind == Kind::Str => sub(text, t.start + 1, caret).to_string(), // past the quote
        Some(t) => word(text, t),
        None => String::new(),
    };
    let replace: (usize, usize) = match current {
        Some(t) if t.kind == Kind::Str && caret < text.len() && bytes[caret] == b'"' => (t.start, caret + 1),
        Some(t) => (t.start, caret),
        None => (caret, caret),
    };
    let context = scan::context_of(text, tokens, open_stack);
    // The first stack entry is the innermost open bracket.
    let innermost = open_stack.first();
    let enclosing: Option<usize> = innermost.and_then(|(_, at)| scan::index_at(tokens, *at));
    let enclosing_head: Option<String> = match enclosing {
        Some(i) if i + 1 < tokens.len() && tokens[i + 1].kind == Kind::Head => Some(word(text, &tokens[i + 1])),
        _ => None,
    };
    let prior: Vec<usize> = match enclosing {
        Some(i) => scan::items(tokens, i).into_iter().filter(|j| tokens[*j].stop <= replace.0).collect(),
        None => Vec::new(),
    };
    let present: Vec<String> = prior.iter().filter(|j| tokens[**j].kind == Kind::Kw).map(|j| word(text, &tokens[*j])).collect();
    let after_kw: Option<String> = match prior.last() {
        Some(j) if tokens[*j].kind == Kind::Kw => Some(word(text, &tokens[*j])),
        _ => None,
    };
    let kind: Option<&Entry> = enclosing_head.as_deref().and_then(|h| kind_of(vocab, &context, h));
    let strip = |kw: &str| kw[1..].to_string();
    // the choices of the keyword before the caret
    let mut choices: Vec<Candidate> = Vec::new();
    if let (Some(kw), Some(k)) = (&after_kw, kind) {
        let kw = strip(kw);
        if let Some(f) = k.fields.iter().find(|f| f.name == kw) {
            if let param::KindView::Choice(options) = &f.kind {
                choices = options.iter().map(|o| mk(0, 0, Some(format!("\"{o}\"")), o, "choice", &format!("{} = {o}", f.label))).collect();
            }
        }
    }
    // an operator's text argument: after its keyword, or at its place among the positional ones
    if choices.is_empty() {
        if let Some(h) = &enclosing_head {
            let argument: Option<flow_syntax::symbol::Symbol> = match &after_kw {
                Some(kw) => flow_syntax::symbol::find(&strip(kw)),
                None if present.is_empty() && !prior.is_empty() => {
                    workspace::op_signature(&vocab.ops, &ws_context(&context), h).and_then(|s| s.pos.get(prior.len() - 1).map(|(n, _)| *n))
                }
                None => None,
            };
            if let Some(argument) = argument {
                choices = workspace::op_choices(&vocab.ops, h, &argument)
                    .iter()
                    .map(|o| mk(0, 0, Some(format!("\"{o}\"")), o, "choice", &format!("{h} \u{b7} {o}")))
                    .collect();
            }
        }
    }
    // a new name in a binder's vector is the writer's to choose
    let naming = match (innermost, enclosing) {
        (Some(('[', _)), Some(i)) => match scan::head_before(text, tokens, i) {
            Some(h) if scan::BINDERS.contains(&h.as_str()) => h == "fn" || prior.len() % 2 == 0,
            _ => false,
        },
        _ => false,
    };
    // the value of :material, :camera or :active, from the document
    let valued: Vec<Candidate> = match (&after_kw, current) {
        (_, Some(t)) if matches!(t.kind, Kind::Kw | Kind::Head | Kind::Meta | Kind::Str) => Vec::new(),
        (Some(kw), _) if kw == ":material" => {
            names.materials.iter().map(|m| mk(0, 0, Some(format!("(ref {m})")), m, "material", &format!("(ref {m})"))).collect()
        }
        (Some(kw), _) if kw == ":camera" => names.cameras.iter().map(|c| mk(0, 0, None, c, "camera", "a camera of the scene")).collect(),
        (Some(kw), _) if kw == ":active" => names
            .layouts
            .iter()
            .enumerate()
            .map(|(i, l)| mk(0, 0, Some(i.to_string()), &format!("{i} {l}"), "layout", &format!("layout {l}")))
            .collect(),
        _ => Vec::new(),
    };
    let cur_kind = current.map(|t| t.kind);
    let candidates: Vec<Candidate> = if naming {
        Vec::new()
    } else if !valued.is_empty() {
        valued
    } else if cur_kind == Some(Kind::Meta) {
        vec![mk(5, 0, None, "^:bypass", "meta", &special_keyword(":bypass").unwrap_or_default())]
    } else if cur_kind == Some(Kind::Kw) {
        // a keyword: the kind's parameters and slots, an operator's keywords, a form's keywords
        let mut out: Vec<Candidate> = Vec::new();
        if let Some(k) = kind {
            for (name, ty, doc, primary) in keywords_of(k) {
                if present.contains(&format!(":{name}")) {
                    continue;
                }
                out.push(mk(if primary { 0 } else { 1 }, 0, None, &format!(":{name}"), &ty, &doc));
            }
            for (s, required) in &k.slots {
                out.push(mk(
                    2,
                    0,
                    None,
                    &format!(":{s}"),
                    "input",
                    &format!("{s} \u{b7} {} input of {}", if *required { "required" } else { "optional" }, k.qualified),
                ));
            }
        }
        if let Some(h) = &enclosing_head {
            if let Some(s) = workspace::op_signature(&vocab.ops, &ws_context(&context), h) {
                for (k, t) in &s.kw {
                    out.push(mk(0, 0, None, &format!(":{k}"), &ty_name(t), &op_doc(h, &s)));
                }
            }
        }
        let of_form: &[&str] = match enclosing_head.as_deref() {
            Some("graph" | "defn" | "defmacro") => &[":context"],
            Some("for" | "scene/merge") => &[":skip"],
            Some("cond" | "case") => &[":else"],
            _ => &[],
        };
        for k in of_form {
            out.push(mk(3, 0, None, k, "form", &special_keyword(k).unwrap_or_default()));
        }
        out
    } else if cur_kind == Some(Kind::Str) {
        choices.clone()
    } else if cur_kind == Some(Kind::Num) {
        Vec::new()
    } else if head_position {
        // the head of a call: this context's kinds, the defns, the special forms, the operators
        let (_, defns, _) = scan::bound(text, tokens, caret);
        let mut out: Vec<Candidate> = Vec::new();
        for d in &defns {
            out.push(mk(0, usage(text, tokens, d), None, d, "defn", &format!("{d} \u{b7} a function of this workspace")));
        }
        for (name, file) in imports {
            if !names.graphs.contains(name) {
                out.push(mk(0, 0, None, name, &format!("import \u{b7} {file}"), &format!("{name} \u{b7} imported from {file}")));
            }
        }
        for k in &vocab.kinds {
            if k.context == context {
                out.push(mk(0, usage(text, tokens, &k.qualified), None, &k.qualified, &k.category.join(" / "), &format!("{} \u{b7} {}", k.qualified, kind_doc(k))));
            }
        }
        for (s, doc) in specials() {
            out.push(mk(1, usage(text, tokens, &s), None, &s, "form", &doc));
        }
        for (o, s) in ops_of(vocab, &context) {
            out.push(mk(2, usage(text, tokens, &o), None, &o, "operator", &op_doc(&o, &s)));
        }
        out
    } else if cur_kind == Some(Kind::Sym) {
        // an argument: a context after :context, a graph after ref, a choice after its keyword,
        // else the bindings in scope, the constants and the defns
        if after_kw.as_deref() == Some(":context") {
            contexts().iter().map(|c| mk(0, 0, None, c, "context", &format!("{c} graph"))).collect()
        } else if enclosing_head.as_deref() == Some("ref") && prior.len() <= 1 {
            let (_, _, graphs) = scan::bound(text, tokens, caret);
            let mut all = graphs.clone();
            all.extend(names.graphs.iter().filter(|g| !graphs.contains(g)).cloned());
            all.iter()
                .map(|g| {
                    let detail = match imports.iter().find(|(n, _)| n == g) {
                        Some((_, file)) => format!("graph \u{b7} {file}"),
                        None => "graph".to_string(),
                    };
                    mk(0, 0, None, g, &detail, &format!("(ref {g})"))
                })
                .collect()
        } else if !choices.is_empty() {
            choices.clone()
        } else {
            let (bindings, defns, _) = scan::bound(text, tokens, caret);
            let mut seen: Vec<String> = Vec::new();
            let mut out: Vec<Candidate> = Vec::new();
            for (name, doc) in bindings.iter().rev() {
                if seen.contains(name) {
                    continue;
                }
                seen.push(name.clone());
                out.push(mk(0, 0, None, name, "binding", doc));
            }
            for (c, doc) in constants() {
                out.push(mk(1, 0, None, c, "constant", doc));
            }
            for d in &defns {
                out.push(mk(2, 0, None, d, "defn", &format!("{d} \u{b7} a function value")));
            }
            out
        }
    } else {
        Vec::new()
    };
    rank(&prefix, candidates)
        .into_iter()
        .map(|c| Completion {
            replace: (replace.0 as i64, replace.1 as i64),
            insert: c.insert,
            label: c.name,
            detail: c.detail,
            doc: c.doc,
        })
        .collect()
}

/// The token at `byte` and what it is.
pub fn describe(vocab: &Vocab, text: &str, byte: usize) -> Option<(i64, i64, String)> {
    let lexed = lex(text);
    let tokens = &lexed.0;
    let i = tokens.iter().position(|t| t.start <= byte && byte < t.stop)?;
    let t = &tokens[i];
    let w = word(text, t);
    let enclosing_head = scan::head_before(text, tokens, i);
    let open_stack = lex(sub(text, 0, t.start)).2.clone();
    let context = scan::context_of(text, tokens, &open_stack);
    let kind = |name: &str| kind_of(vocab, &context, name);
    let signature = |name: &str| workspace::op_signature(&vocab.ops, &ws_context(&context), name);
    let bare = if w.len() > 1 { w[1..].to_string() } else { String::new() };
    let sp = specials();
    let doc: Option<String> = match t.kind {
        Kind::Head => match (sp.iter().find(|(n, _)| *n == w), kind(&w), signature(&w)) {
            (Some((_, doc)), _, _) => Some(doc.clone()),
            (None, Some(k), _) => Some(format!("{} \u{b7} {}", k.qualified, kind_doc(k))),
            (None, None, Some(s)) => Some(op_doc(&w, &s)),
            (None, None, None) => {
                let (_, defns, _) = scan::bound(text, tokens, t.start);
                if defns.contains(&w) { Some(format!("{w} \u{b7} a function of this workspace")) } else { None }
            }
        },
        Kind::Kw => {
            let k = enclosing_head.as_deref().and_then(kind);
            let s = enclosing_head.as_deref().and_then(signature);
            match (k, s) {
                (Some(k), _) => match keywords_of(k).into_iter().find(|(name, ..)| *name == bare) {
                    Some((_, _, doc, _)) => Some(doc),
                    None => {
                        if k.slots.iter().any(|(n, _)| *n == bare) {
                            Some(format!("{bare} \u{b7} input of {}", k.qualified))
                        } else {
                            special_keyword(&w)
                        }
                    }
                },
                (None, Some(s)) if s.kw.iter().any(|(n, _)| *n == bare) => {
                    let ty = s.kw.iter().find(|(n, _)| *n == bare).map(|(_, t)| ty_name(t)).unwrap_or_default();
                    Some(format!("{w} \u{b7} {ty}"))
                }
                _ => special_keyword(&w),
            }
        }
        Kind::Sym => match constants().iter().find(|(n, _)| *n == w) {
            Some((_, doc)) => Some(doc.to_string()),
            None => {
                let (bindings, defns, graphs) = scan::bound(text, tokens, t.start);
                match bindings.iter().rev().find(|(n, _)| *n == w) {
                    Some((_, doc)) => Some(format!("{w} \u{b7} {doc}")),
                    None => {
                        if defns.contains(&w) {
                            Some(format!("{w} \u{b7} a function of this workspace"))
                        } else if graphs.contains(&w) {
                            Some(format!("{w} \u{b7} a graph of this workspace"))
                        } else {
                            None
                        }
                    }
                }
            }
        },
        Kind::Num => Some(format!("{w} \u{b7} drag sideways to change it, Shift for coarse steps")),
        Kind::Meta => special_keyword(&bare),
        Kind::Str => match enclosing_head.as_deref().and_then(kind) {
            Some(k) => {
                let v: String = w.replace('"', " ");
                let v = v.trim().to_string();
                k.fields
                    .iter()
                    .find(|f| matches!(&f.kind, param::KindView::Choice(options) if options.contains(&v)))
                    .map(|f| format!("{v} \u{b7} a choice of :{} ({})", f.name, f.label))
            }
            None => None,
        },
        Kind::Comment | Kind::Open(_) | Kind::Close(_) | Kind::Unmatched => None,
    };
    doc.map(|doc| (t.start as i64, t.stop as i64, doc))
}

pub fn number_at(text: &str, byte: usize) -> Option<(i64, i64)> {
    lex(text).0.iter().find(|t| t.kind == Kind::Num && t.start <= byte && byte < t.stop).map(|t| (t.start as i64, t.stop as i64))
}

/// The graph named by the `(ref name)` form under `byte`.
pub fn ref_at(text: &str, byte: usize) -> Option<String> {
    let lexed = lex(text);
    let tokens = &lexed.0;
    let mut found = None;
    for (i, t) in tokens.iter().enumerate() {
        if i > 0
            && i + 1 < tokens.len()
            && t.kind == Kind::Head
            && word(text, t) == "ref"
            && tokens[i + 1].kind == Kind::Sym
            && tokens[i - 1].start <= byte
            && byte <= tokens[i + 1].stop + 1
        {
            found = Some(word(text, &tokens[i + 1]));
        }
    }
    found
}

/// What a character typed at `caret` lands in: inside a string (the quotes' insides, an open one to
/// the end) or after the semicolon of a comment, else code.
pub fn context_at(text: &str, caret: usize) -> CodeContext {
    let lexed = lex(text);
    let bytes = text.as_bytes();
    match lexed.0.iter().find(|t| {
        t.start < caret
            && caret <= t.stop
            && (t.kind == Kind::Comment || (t.kind == Kind::Str && (caret < t.stop || t.stop - t.start < 2 || bytes[t.stop - 1] != b'"')))
    }) {
        Some(t) if t.kind == Kind::Comment => CodeContext::Comment,
        Some(_) => CodeContext::String,
        None => CodeContext::Code,
    }
}

/// The "#rrggbb" literals of the text with their colour.
pub fn color_chips(text: &str) -> Vec<(i64, i64, pxui::geom::color::T)> {
    let lexed = lex(text);
    let bytes = text.as_bytes();
    let mut out = Vec::new();
    for t in lexed.0.iter() {
        if t.kind != Kind::Str || t.stop - t.start < 9 || bytes[t.stop - 1] != b'"' {
            continue;
        }
        if let Ok(color) = pxui::geom::color::hex(sub(text, t.start + 1, t.stop - 1)) {
            out.push((t.start as i64, t.stop as i64, color));
        }
    }
    out
}

pub fn language(vocab: Option<Vocab>, names: Option<Names>, imports: Option<Vec<(String, String)>>, parinfer: bool, theme: Theme) -> Language {
    let vocab = Rc::new(vocab.unwrap_or_else(empty_vocab));
    let names = Rc::new(names.unwrap_or_default());
    let imports = Rc::new(imports.unwrap_or_default());
    let (complete_vocab, describe_vocab) = (vocab.clone(), vocab);
    Language {
        colorize: Rc::new(move |text| colorize_now(&theme, text)),
        brackets: Rc::new(brackets),
        indent: Rc::new(|text, caret| scan::indent(text, caret.max(0) as usize)),
        pairs: vec![('(', ')'), ('[', ']'), ('{', '}'), ('"', '"')],
        complete: Rc::new(move |text, caret| complete(&names, &imports, &complete_vocab, text, caret.max(0) as usize)),
        describe: Rc::new(move |text, byte| describe(&describe_vocab, text, byte.max(0) as usize)),
        number_at: Rc::new(|text, byte| number_at(text, byte.max(0) as usize)),
        context_at: Rc::new(|text, caret| context_at(text, caret.max(0) as usize)),
        comment: ";".to_string(),
        rewrite: if parinfer { Some(Rc::new(|text, caret| flow_syntax::parinfer::rewrite(text, caret))) } else { None },
    }
}
