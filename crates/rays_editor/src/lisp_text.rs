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

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex, OnceLock, atomic::{AtomicI64, Ordering}};

use flow_check::lisp_scan::{self as scan, Kind, Token};
use flow_check::{op, workspace};
use flow_sop::catalog::Descriptor;
use pxui::theme::{Tokens as Theme};
use pxui::ui::{CodeContext, Completion};
use sop::edit_graph::InputRequirement;

pub use flow_syntax::text_lexer::{number, separator};

pub use scan::Lexed;

#[derive(Default)]
struct LexEntry {
    text: String,
    tokens: Arc<Lexed>,
}

impl LexEntry {
    fn bytes(&self) -> i64 {
        let (tokens, pairs, open) = self.tokens.as_ref();
        (self.text.capacity() + std::mem::size_of::<Lexed>()
            + tokens.capacity() * std::mem::size_of::<Token>()
            + pairs.capacity() * std::mem::size_of::<(usize, usize)>()
            + open.capacity() * std::mem::size_of::<(char, usize)>()) as i64
    }
}

static LEXED: Mutex<Vec<LexEntry>> = Mutex::new(Vec::new());
static LEX_OWNER: OnceLock<mem::Owner> = OnceLock::new();
static LEX_LAST_USE: AtomicI64 = AtomicI64::new(0);

/// The four most recent texts share their lexing across callers. On a miss, reuse the
/// oldest entry's buffers once its readers have let go.
pub fn lex(text: &str) -> Arc<Lexed> {
    let owner = LEX_OWNER.get_or_init(|| {
        let owner = mem::cache("editor.lisp_text");
        let evict_owner = owner.clone();
        let options = mem::EvictorOptions {
            last_use: Some(Arc::new(|| LEX_LAST_USE.load(Ordering::Relaxed))),
            ..Default::default()
        };
        mem::register_evictor(&owner, options, Arc::new(move || {
            let mut entries = LEXED.lock().unwrap_or_else(|e| e.into_inner());
            let Some(entry) = entries.pop() else { return false };
            mem::release(&evict_owner, entry.bytes());
            true
        }));
        owner
    });
    let mut entries = LEXED.lock().unwrap_or_else(|e| e.into_inner());
    LEX_LAST_USE.store(mem::epoch(), Ordering::Relaxed);
    if let Some(at) = entries.iter().position(|entry| entry.text == text) {
        entries[..=at].rotate_right(1);
        return entries[0].tokens.clone();
    }
    let mut entry = if entries.len() == 4 {
        let entry = entries.pop().expect("full lex cache");
        mem::release(owner, entry.bytes());
        entry
    } else {
        LexEntry::default()
    };
    entry.text.clear();
    entry.text.push_str(text);
    if Arc::get_mut(&mut entry.tokens).is_none() {
        entry.tokens = Arc::default();
    }
    scan::lex_into(text, Arc::get_mut(&mut entry.tokens).expect("unique lex storage"));
    let tokens = entry.tokens.clone();
    mem::charge(owner, entry.bytes());
    entries.insert(0, entry);
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

fn word<'a>(text: &'a str, t: &Token) -> &'a str {
    scan::word(text, t)
}

pub fn colorize_now(theme: &Theme, text: &str, out: &mut Vec<(usize, usize, pxui::Rgba)>) {
    // kit rev 3: forms in the float blue, a kind's namespace in the vector purple, keywords in the
    // bool red, numbers green, strings in the record green; brackets and comments recede to ink-3
    let ports = rays_graph::scope_paint::ports(theme);
    let quiet = theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.521);
    let lexed = lex(text);
    let tokens = &lexed.0;
    out.clear();
    for (i, t) in tokens.iter().enumerate() {
        let push = |out: &mut Vec<_>, color| out.push((t.start, t.stop, color));
        let declares = i + 1 < tokens.len() && tokens[i + 1].kind == Kind::Sym && sub(text, tokens[i + 1].start, tokens[i + 1].stop) == ":";
        match t.kind {
            Kind::Comment | Kind::Meta => push(out, quiet),
            Kind::Str => push(out, ports.record),
            Kind::Num => push(out, ports.int),
            Kind::Kw => push(out, ports.bool),
            Kind::Head if declares => {}
            Kind::Head => match text.as_bytes()[t.start..].iter().position(|b| *b == b'/').map(|p| p + t.start) {
                Some(slash) if slash + 1 < t.stop => out.push((t.start, slash + 1, ports.vec3)),
                _ => push(out, ports.float),
            },
            Kind::Sym => {}
            Kind::Open(_) | Kind::Close(_) => push(out, quiet),
            Kind::Unmatched => push(out, pxui::Rgba::hex(0xc2255c)),
        }
    }
}

pub fn brackets(text: &str) -> Vec<(usize, usize)> {
    // ponytail: PXUI wants usize; hand off a capacity shell like colorize_now.
    thread_local!(static PAIRS: std::cell::RefCell<Vec<(usize, usize)>> = const { std::cell::RefCell::new(Vec::new()) });
    PAIRS.with(|cell| {
        let mut out = cell.borrow_mut();
        out.clear();
        out.extend_from_slice(&lex(text).1);
        let cap = out.capacity();
        std::mem::replace(&mut *out, Vec::with_capacity(cap))
    })
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
/// Built once: every describe/complete call borrows the same table.
pub fn specials() -> &'static [(&'static str, String)] {
    static SPECIALS: OnceLock<Vec<(&'static str, String)>> = OnceLock::new();
    SPECIALS.get_or_init(|| {
        flow_check::registry::FORMS
            .iter()
            .chain(flow_check::registry::DIRECTIVES)
            .map(|(name, args, doc, _)| (*name, format!("({name} {args}) · {doc}")))
            .collect()
    })
}

pub fn constants() -> &'static [(&'static str, &'static str)] {
    &[
        ("t", "time in seconds \u{b7} a binding reading t recooks every frame"),
        ("pi", "3.14159..."),
        ("true", "boolean true"),
        ("false", "boolean false"),
        ("nil", "empty geometry \u{b7} an unconnected slot"),
    ]
}

const CONTEXT_NAMES: &[&str] = &["scene", "sop", "material", "world", "editor", "settings", "value", "draw", "image", "host"];

pub fn contexts() -> &'static [&'static str] {
    CONTEXT_NAMES
}

pub fn special_keywords() -> &'static [(String, String)] {
    static KWS: OnceLock<Vec<(String, String)>> = OnceLock::new();
    KWS.get_or_init(|| {
        vec![
            (":context".to_string(), format!("the graph's context: {}", CONTEXT_NAMES.join(", "))),
            (":skip".to_string(), ":skip [i ...] \u{b7} iteration tuples a for or scene/merge leaves out".to_string()),
            (":else".to_string(), "the branch when no test holds".to_string()),
            (":keys".to_string(), "{:keys [a b]} \u{b7} destructure a record's fields".to_string()),
            (":bypass".to_string(), "^:bypass (call ...) \u{b7} the call passes its first input through".to_string()),
        ]
    })
}

fn special_keyword(name: &str) -> Option<&'static str> {
    special_keywords().iter().find(|(k, _)| k == name).map(|(_, d)| d.as_str())
}

fn ws_context(name: &str) -> flow_syntax::context::Context {
    flow_syntax::context::of_string(name).unwrap_or_else(|_| flow_syntax::context::sop())
}

pub fn ops_of(vocab: &Vocab, context: &str) -> Vec<(String, workspace::OpSignature)> {
    ops_borrowed(vocab, context)
        .map(|o| {
            let s = workspace::OpSignature {
                pos: o.signature.pos.clone(),
                opt: o.signature.opt.clone(),
                rest: o.signature.rest.clone(),
                kw: o.signature.kw.clone(),
            };
            (o.name.clone(), s)
        })
        .collect()
}

/// Builtins + extras for `context` and `value`, borrowed (no Op clone).
fn ops_borrowed<'a>(vocab: &'a Vocab, context: &str) -> impl Iterator<Item = &'a op::Op> {
    let ctx = ws_context(context);
    let value = flow_syntax::context::value();
    op::all()
        .iter()
        .chain(vocab.ops.iter())
        .filter(move |o| o.ctx == ctx || o.ctx == value)
}

fn ty_name(ty: &flow_syntax::ty::Ty) -> String {
    flow_syntax::ty::to_string(ty)
}

fn op_doc_fields(
    name: &str,
    pos: &[(flow_syntax::symbol::Symbol, flow_syntax::ty::Ty)],
    opt: &[(flow_syntax::symbol::Symbol, flow_syntax::ty::Ty)],
    rest: &Option<(flow_syntax::symbol::Symbol, flow_syntax::ty::Ty)>,
    kw: &[(flow_syntax::symbol::Symbol, flow_syntax::ty::Ty)],
) -> String {
    let mut parts: Vec<String> = vec![name.to_string()];
    parts.extend(pos.iter().map(|(n, t)| format!("{n}:{}", ty_name(t))));
    parts.extend(opt.iter().map(|(n, t)| format!("[{n}:{}]", ty_name(t))));
    if let Some((n, t)) = rest {
        parts.push(format!("{n}:{}...", ty_name(t)));
    }
    parts.extend(kw.iter().map(|(n, t)| format!(":{n} {}", ty_name(t))));
    format!("({}) \u{b7} built-in operator", parts.join(" "))
}

fn op_doc_sig(name: &str, s: &op::Signature) -> String {
    op_doc_fields(name, &s.pos, &s.opt, &s.rest, &s.kw)
}

pub fn op_doc(name: &str, s: &workspace::OpSignature) -> String {
    op_doc_fields(name, &s.pos, &s.opt, &s.rest, &s.kw)
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

fn ascii_prefix(name: &str, prefix: &str) -> bool {
    let (n, p) = (name.as_bytes(), prefix.as_bytes());
    p.len() <= n.len() && n[..p.len()].iter().zip(p).all(|(a, b)| a.eq_ignore_ascii_case(b))
}

/// How well `name` matches the typed `prefix`: 3 a prefix, 2 a prefix of a word after _ or /, 1 a
/// subsequence, 0 none. Allocates nothing.
pub fn quality(prefix: &str, name: &str) -> i64 {
    if prefix.is_empty() || ascii_prefix(name, prefix) {
        3
    } else if name.split('_').any(|w| ascii_prefix(w, prefix)) || name.split('/').any(|w| ascii_prefix(w, prefix)) {
        2
    } else if pxui::ui::fuzzy_match(prefix, name) {
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

/// Scored name before `mk`: build detail/doc strings only for the top 48.
enum SoftKind<'a> {
    Defn,
    Import { file: &'a str },
    Kind { k: &'a Entry },
    Form { doc: &'a str },
    Op { op: &'a op::Op },
}

struct Soft<'a> {
    q: i64,
    group: i64,
    uses: i64,
    name: &'a str,
    kind: SoftKind<'a>,
}

fn soft_push<'a>(out: &mut Vec<Soft<'a>>, prefix: &str, group: i64, uses: i64, name: &'a str, kind: SoftKind<'a>) {
    let q = quality(prefix, name);
    if q != 0 {
        out.push(Soft { q, group, uses, name, kind });
    }
}

fn soft_finish(mut soft: Vec<Soft<'_>>) -> Vec<Candidate> {
    soft.sort_by(|a, b| {
        b.q.cmp(&a.q)
            .then(a.group.cmp(&b.group))
            .then(b.uses.cmp(&a.uses))
            .then_with(|| a.name.as_bytes().cmp(b.name.as_bytes()))
    });
    soft.truncate(48);
    soft.into_iter()
        .map(|s| match s.kind {
            SoftKind::Defn => mk(s.group, s.uses, None, s.name, "defn", &format!("{0} \u{b7} a function of this workspace", s.name)),
            SoftKind::Import { file } => {
                mk(s.group, s.uses, None, s.name, &format!("import \u{b7} {file}"), &format!("{0} \u{b7} imported from {file}", s.name))
            }
            SoftKind::Kind { k } => {
                mk(s.group, s.uses, None, s.name, &k.category.join(" / "), &format!("{} \u{b7} {}", k.qualified, kind_doc(k)))
            }
            SoftKind::Form { doc } => mk(s.group, s.uses, None, s.name, "form", doc),
            SoftKind::Op { op } => mk(s.group, s.uses, None, s.name, "operator", &op_doc_sig(&op.name, &op.signature)),
        })
        .collect()
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
        Some(t) if t.kind == Kind::Kw => t.stop < caret && VALUE_KEYWORDS.contains(&word(text, t)),
        _ => false,
    };
    if current.is_none() && !head_position && !after_value_keyword {
        return Vec::new();
    }
    // the whole text, so the brackets around the caret are matched
    let whole = lex(text);
    let tokens = &whole.0;
    let prefix: &str = match current {
        Some(t) if t.kind == Kind::Str => sub(text, t.start + 1, caret),
        Some(t) => word(text, t),
        None => "",
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
    let enclosing_head: Option<&str> = match enclosing {
        Some(i) if i + 1 < tokens.len() && tokens[i + 1].kind == Kind::Head => Some(word(text, &tokens[i + 1])),
        _ => None,
    };
    let prior = enclosing.into_iter().flat_map(|i| scan::items(tokens, i))
        .filter(|j| tokens[*j].stop <= replace.0);
    let (prior_count, last) = prior.clone().fold((0, None), |(count, _), item| (count + 1, Some(item)));
    let present = prior.filter(|&i| tokens[i].kind == Kind::Kw).map(|i| word(text, &tokens[i]));
    let after_kw = last.filter(|&i| tokens[i].kind == Kind::Kw).map(|i| word(text, &tokens[i]));
    let kind: Option<&Entry> = enclosing_head.and_then(|h| kind_of(vocab, context, h));
    // the choices of the keyword before the caret
    let mut choices: Vec<Candidate> = Vec::new();
    if let (Some(kw), Some(k)) = (after_kw, kind) {
        let kw = &kw[1..];
        if let Some(f) = k.fields.iter().find(|f| f.name == kw) {
            if let param::KindView::Choice(options) = &f.kind {
                choices = options.iter().map(|o| mk(0, 0, Some(format!("\"{o}\"")), o, "choice", &format!("{} = {o}", f.label))).collect();
            }
        }
    }
    // an operator's text argument: after its keyword, or at its place among the positional ones
    if choices.is_empty() {
        if let Some(h) = enclosing_head {
            let argument: Option<flow_syntax::symbol::Symbol> = match after_kw {
                Some(kw) => flow_syntax::symbol::find(&kw[1..]),
                None if prior_count > 0 && present.clone().next().is_none() => {
                    op::find_ref(&vocab.ops, h, &ws_context(context)).and_then(|o| o.signature.pos.get(prior_count - 1).map(|(n, _)| *n))
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
            Some(h) if scan::BINDERS.contains(&h) => h == "fn" || prior_count % 2 == 0,
            _ => false,
        },
        _ => false,
    };
    // the value of :material, :camera or :active, from the document
    let valued: Vec<Candidate> = match (after_kw, current) {
        (_, Some(t)) if matches!(t.kind, Kind::Kw | Kind::Head | Kind::Meta | Kind::Str) => Vec::new(),
        (Some(":material"), _) => {
            names.materials.iter().map(|m| mk(0, 0, Some(format!("(ref {m})")), m, "material", &format!("(ref {m})"))).collect()
        }
        (Some(":camera"), _) => names.cameras.iter().map(|c| mk(0, 0, None, c, "camera", "a camera of the scene")).collect(),
        (Some(":active"), _) => names
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
                let key = format!(":{name}");
                if present.clone().any(|p| p == key) {
                    continue;
                }
                out.push(mk(if primary { 0 } else { 1 }, 0, None, &key, &ty, &doc));
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
        if let Some(h) = enclosing_head {
            if let Some(s) = workspace::op_signature(&vocab.ops, &ws_context(context), h) {
                for (k, t) in &s.kw {
                    out.push(mk(0, 0, None, &format!(":{k}"), &ty_name(t), &op_doc(h, &s)));
                }
            }
        }
        let of_form: &[&str] = match enclosing_head {
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
        // Score every head name first; mk (and op/kind docs) only for the top 48.
        let (_, defns, _) = scan::bound_where(text, tokens, caret, &|name| quality(prefix, name) != 0);
        let mut soft: Vec<Soft<'_>> = Vec::new();
        for d in defns {
            soft_push(&mut soft, prefix, 0, usage(text, tokens, d), d, SoftKind::Defn);
        }
        for (name, file) in imports {
            if names.graphs.contains(name) {
                continue;
            }
            soft_push(&mut soft, prefix, 0, 0, name, SoftKind::Import { file });
        }
        for k in &vocab.kinds {
            if k.context != context {
                continue;
            }
            soft_push(&mut soft, prefix, 0, usage(text, tokens, &k.qualified), &k.qualified, SoftKind::Kind { k });
        }
        for (s, doc) in specials() {
            soft_push(&mut soft, prefix, 1, usage(text, tokens, s), s, SoftKind::Form { doc });
        }
        for o in ops_borrowed(vocab, context) {
            soft_push(&mut soft, prefix, 2, usage(text, tokens, &o.name), &o.name, SoftKind::Op { op: o });
        }
        let ranked = soft_finish(soft);
        return ranked
            .into_iter()
            .map(|c| Completion { replace, insert: c.insert, label: c.name, detail: c.detail, doc: c.doc })
            .collect();
    } else if cur_kind == Some(Kind::Sym) {
        if after_kw == Some(":context") {
            contexts().iter().filter(|c| quality(prefix, c) != 0).map(|c| mk(0, 0, None, c, "context", &format!("{c} graph"))).collect()
        } else if enclosing_head == Some("ref") && prior_count <= 1 {
            let (_, _, graphs) = scan::bound_where(text, tokens, caret, &|name| quality(prefix, name) != 0);
            let mut all: Vec<&str> = graphs;
            for g in &names.graphs {
                if !all.contains(&g.as_str()) { all.push(g.as_str()); }
            }
            all.into_iter()
                .filter(|g| quality(prefix, g) != 0)
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
            let (bindings, defns, _) = scan::bound_where(text, tokens, caret, &|name| quality(prefix, name) != 0);
            let mut seen: Vec<&str> = Vec::new();
            let mut out: Vec<Candidate> = Vec::new();
            for (name, _) in bindings.iter().rev() {
                if seen.contains(name) { continue; }
                seen.push(*name);
                out.push(mk(0, 0, None, name, "binding", ""));
            }
            for (c, doc) in constants() {
                if quality(prefix, c) == 0 { continue; }
                out.push(mk(1, 0, None, c, "constant", doc));
            }
            for d in defns {
                out.push(mk(2, 0, None, d, "defn", &format!("{d} \u{b7} a function value")));
            }
            let mut ranked = rank(prefix, out);
            for c in &mut ranked {
                if c.detail == "binding" {
                    if let Some((_, doc)) = bindings.iter().rev().find(|(n, _)| *n == c.name) {
                        c.doc = doc.render(text, tokens);
                    }
                }
            }
            return ranked
                .into_iter()
                .map(|c| Completion { replace, insert: c.insert, label: c.name, detail: c.detail, doc: c.doc })
                .collect();
        }
    } else {
        Vec::new()
    };
    rank(prefix, candidates)
        .into_iter()
        .map(|c| Completion {
            replace,
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
    let kind = |name: &str| kind_of(vocab, context, name);
    let signature = |name: &str| workspace::op_signature(&vocab.ops, &ws_context(context), name);
    let bare = if w.len() > 1 { w[1..].to_string() } else { String::new() };
    let doc: Option<String> = match t.kind {
        Kind::Head => match (specials().iter().find(|(n, _)| *n == w), kind(&w), signature(&w)) {
            (Some((_, doc)), _, _) => Some(doc.clone()),
            (None, Some(k), _) => Some(format!("{} \u{b7} {}", k.qualified, kind_doc(k))),
            (None, None, Some(s)) => Some(op_doc(&w, &s)),
            (None, None, None) => {
                let (_, defns, _) = scan::bound(text, tokens, t.start);
                if defns.contains(&w) { Some(format!("{w} \u{b7} a function of this workspace")) } else { None }
            }
        },
        Kind::Kw => {
            let k = enclosing_head.and_then(kind);
            let s = enclosing_head.and_then(signature);
            match (k, s) {
                (Some(k), _) => match keywords_of(k).into_iter().find(|(name, ..)| *name == bare) {
                    Some((_, _, doc, _)) => Some(doc),
                    None => {
                        if k.slots.iter().any(|(n, _)| *n == bare) {
                            Some(format!("{bare} \u{b7} input of {}", k.qualified))
                        } else {
                            special_keyword(&w).map(|s| s.to_string())
                        }
                    }
                },
                (None, Some(s)) if s.kw.iter().any(|(n, _)| *n == bare) => {
                    let ty = s.kw.iter().find(|(n, _)| *n == bare).map(|(_, t)| ty_name(t)).unwrap_or_default();
                    Some(format!("{w} \u{b7} {ty}"))
                }
                _ => special_keyword(&w).map(|s| s.to_string()),
            }
        }
        Kind::Sym => match constants().iter().find(|(n, _)| *n == w) {
            Some((_, doc)) => Some(doc.to_string()),
            None => {
                let (bindings, defns, graphs) = scan::bound(text, tokens, t.start);
                match bindings.iter().rev().find(|(n, _)| *n == w) {
                    Some((_, doc)) => Some(format!("{w} \u{b7} {}", doc.render(text, tokens))),
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
          Kind::Meta => special_keyword(&bare).map(|s| s.to_string()),
        Kind::Str => match enclosing_head.and_then(kind) {
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
            found = Some(word(text, &tokens[i + 1]).to_owned());
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
pub fn color_chips(text: &str, out: &mut Vec<(usize, usize, pxui::Rgba)>) {
    out.clear();
    if !text.as_bytes().contains(&b'"') { return; }
    let lexed = lex(text);
    let bytes = text.as_bytes();
    for t in &lexed.0 {
        if t.kind != Kind::Str || t.stop - t.start < 9 || bytes[t.stop - 1] != b'"' {
            continue;
        }
        if let Ok(color) = pxui::Rgba::parse(sub(text, t.start + 1, t.stop - 1)) {
            out.push((t.start, t.stop, color));
        }
    }
}

/// Owns the language callbacks for a text pane. PXUI borrows them for each build.
#[derive(Clone)]
pub struct Language {
    pub colorize: Rc<pxui::ColorizeFn<'static>>,
    pub brackets: Rc<pxui::BracketsFn<'static>>,
    pub indent: Rc<pxui::IndentFn<'static>>,
    pub complete: Rc<pxui::CompleteFn<'static>>,
    pub describe: Rc<pxui::DescribeFn<'static>>,
    pub number_at: Rc<pxui::NumberAtFn<'static>>,
    pub context_at: Rc<pxui::ContextAtFn<'static>>,
    pub rewrite: Option<Rc<pxui::RewriteFn<'static>>>,
    pub comment: &'static str,
}

impl Language {
    pub fn as_pxui(&self) -> pxui::Language<'_> {
        let language = pxui::Language::DEFAULT
            .colorize(self.colorize.as_ref()).brackets(self.brackets.as_ref()).indent(self.indent.as_ref())
            .pairs(&[('(', ')'), ('[', ']'), ('{', '}'), ('"', '"')])
            .complete(self.complete.as_ref()).describe(self.describe.as_ref()).number_at(self.number_at.as_ref())
            .context_at(self.context_at.as_ref()).comment(self.comment);
        self.rewrite.as_deref().map_or(language, |rewrite| language.rewrite(rewrite))
    }
}

pub fn language(vocab: Option<Rc<Vocab>>, names: Option<Rc<Names>>, imports: Option<Vec<(String, String)>>, parinfer: bool, theme: Theme) -> Language {
    let vocab = vocab.unwrap_or_else(|| Rc::new(empty_vocab()));
    let names = names.unwrap_or_default();
    let imports = Rc::new(imports.unwrap_or_default());
    let complete_vocab = vocab.clone();
    Language {
        colorize: Rc::new(move |text, out| colorize_now(&theme, text, out)),
        brackets: Rc::new(brackets),
        indent: Rc::new(scan::indent),
        complete: Rc::new(move |text, caret| complete(&names, &imports, &complete_vocab, text, caret)),
        describe: Rc::new(move |text, byte, out| {
            let (a, b, text) = describe(&vocab, text, byte)?;
            out.push_str(&text);
            Some((a as usize, b as usize))
        }),
        number_at: Rc::new(|text, byte| number_at(text, byte).map(|(a, b)| (a as usize, b as usize))),
        context_at: Rc::new(context_at),
        comment: ";",
        rewrite: parinfer.then(|| {
            let rewrite: Rc<pxui::RewriteFn<'static>> = Rc::new(|text, caret| {
                match flow_syntax::parinfer::rewrite(text, caret as i64) {
                    (std::borrow::Cow::Borrowed(_), caret) => (None, caret as usize),
                    (std::borrow::Cow::Owned(text), caret) => (Some(text), caret as usize),
                }
            });
            rewrite
        }),
    }
}
