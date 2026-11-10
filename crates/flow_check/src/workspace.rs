use crate::{catalog, defnode, op, port_type};
use crate::term_arena::{self as packed, Draft, Node as PackedNode, SyntaxId, TermBuilder};
use flow_syntax::{context, lisp, macros, syntax::{self, Node as SNode, Syntax}, ty::{self, FnSignature, Ty}};
use param::{self, FieldView};
use rays_diagnostic::{self as diagnostic, Diagnostic, Severity, Span};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use std::rc::Rc;
use flow_syntax::symbol::{self, Symbol};


pub type Path = Vec<flow_syntax::symbol::Symbol>;
pub type Paths = BTreeSet<Path>;
pub type Context = context::Context;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern { Name(String), Seq(Vec<Pattern>), Keys(Vec<String>) }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticPolicy { First, RangeLast }
pub use crate::term_arena::{Node, Term, TermId};
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopKind { For, Fold, Scan, Sum }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HofKind { Map, Filter, Reduce, SortBy, Iterate }

/// Borrowed child row IDs, in authored traversal order.
pub fn children(term: &Term) -> &[TermId] { term.children() }

#[derive(Clone, Debug, PartialEq)]
pub struct Graph { pub name:String, pub context:Context, pub inputs:Vec<(String,Ty,Option<Term>)>, pub body:Term, pub form:Syntax }
#[derive(Clone, Debug, PartialEq)]
pub struct NodeDefinition { pub kind:catalog::Kind, pub slots:Vec<catalog::Slot>, pub keyword_inputs:Vec<String>, pub fields:Vec<FieldView>, pub label:String, pub category:Vec<String>, pub description:Option<String>, pub operation:String, pub version:i64, pub body:Graph, pub validations:Vec<(Term,String,String)>, pub normalizers:Vec<(String,Term)> }
#[derive(Clone, Debug, PartialEq)]
pub enum PackedForm { Candidate(Arc<Paths>), Refused(Vec<Diagnostic>) }
#[derive(Clone)]
enum CheckPackedForm { Candidate(Arc<BTreeSet<CheckPath>>), Refused(Vec<Diagnostic>) }
#[derive(Clone)]
pub struct Workspace {
    pub name:String, pub graphs:Arc<Vec<Graph>>, pub defs:Arc<Vec<Graph>>, pub nodes:Arc<Vec<Arc<NodeDefinition>>>, pub macros:Arc<Vec<Syntax>>, pub source:Arc<Vec<Syntax>>,
    pub live:Paths, pub invariant:Paths, pub approx:Paths, pub packed:Arc<Vec<(Path,PackedForm)>>, pub packed_roots:Vec<(Syntax,Path)>,
    pub approx_reasons:Arc<Vec<(Path,Vec<Diagnostic>)>>, pub kind_fns:Vec<(String,(String,Context,Vec<catalog::Slot>))>, pub ops:Arc<Vec<op::Op>>,
}

/// Capacity-based lower bound for retained checked terms and syntax across shared workspaces.
/// Catalog/operator closures, tree-map nodes and allocator/control-block overhead stay unattributed.
#[derive(Default)]
pub struct Memory {
    pub syntax: syntax::Memory,
    checked: syntax::Memory,
}

impl Memory {
    pub fn checked_bytes(&self) -> usize { self.checked.bytes() }
    pub fn into_allocations(self) -> (Vec<(usize, usize)>, Vec<(usize, usize)>) {
        (self.syntax.into_allocations(), self.checked.into_allocations())
    }

    /// Retained syntax and checked arenas; catalog closures and map nodes remain excluded.
    pub fn prelude(&mut self, prelude: &Prelude) {
        let checker = &prelude.checker;
        for definition in checker.defs.values().chain(checker.graphs.values()) {
            self.syntax.add(&definition.form);
        }
        self.syntax.forms(&checker.macro_forms);
        for graph in checker.def_terms.values().chain(checker.graph_terms.values()) { self.graph(graph); }
        for node in &checker.nodes {
            self.graph(&node.body);
            for (term, _, _) in &node.validations { self.term(term); }
            for (_, term) in &node.normalizers { self.term(term); }
        }
    }

    fn shared<T>(&mut self, value: &Arc<T>) -> bool {
        self.checked.allocation(Arc::as_ptr(value), std::mem::size_of::<T>())
    }

    fn buffer<T>(&mut self, values: &Vec<T>) -> bool {
        self.checked.allocation(values.as_ptr(), values.capacity() * std::mem::size_of::<T>())
    }

    fn name(&mut self, name: &String) { self.checked.allocation(name.as_ptr(), name.capacity()); }

    fn names(&mut self, names: &Vec<String>) {
        if self.buffer(names) { for name in names { self.name(name); } }
    }


    fn ty(&mut self, ty: &Ty) {
        match ty {
            Ty::Named(name) => self.name(name),
            Ty::List(element) | Ty::Array(element) => {
                if self.checked.allocation(element.as_ref() as *const Ty, std::mem::size_of::<Ty>()) { self.ty(element); }
            }
            Ty::Record(fields) => {
                if self.buffer(fields) { for (_, ty) in fields { self.ty(ty); } }
            }
            Ty::Fn(Some(signature)) => {
                if self.checked.allocation(signature.as_ref() as *const FnSignature, std::mem::size_of::<FnSignature>()) {
                    if self.buffer(&signature.params) { for ty in &signature.params { self.ty(ty); } }
                    self.ty(&signature.result);
                }
            }
            _ => (),
        }
    }


    pub fn workspace(&mut self, workspace: &Arc<Workspace>) {
        if !self.shared(workspace) { return; }
        self.name(&workspace.name);
        for graphs in [&workspace.graphs, &workspace.defs] {
            if self.shared(graphs) {
                self.buffer(graphs);
                for graph in graphs.iter() { self.graph(graph); }
            }
        }
        if self.shared(&workspace.nodes) {
            self.buffer(&workspace.nodes);
            for node in workspace.nodes.iter() {
                if !self.shared(node) { continue; }
                self.names(&node.keyword_inputs);
                self.buffer(&node.slots);
                self.buffer(&node.fields);
                self.name(&node.label);
                self.names(&node.category);
                if let Some(description) = &node.description { self.name(description); }
                self.name(&node.operation);
                self.graph(&node.body);
                self.buffer(&node.validations);
                self.buffer(&node.normalizers);
                for (term, code, message) in &node.validations {
                    self.term(term); self.name(code); self.name(message);
                }
                for (name, term) in &node.normalizers { self.name(name); self.term(term); }
            }
        }
        self.syntax.shared_forms(&workspace.source);
        self.syntax.shared_forms(&workspace.macros);
        self.buffer(&workspace.packed_roots);
        for (form, path) in &workspace.packed_roots { self.syntax.add(form); self.buffer(path); }
    }

    fn graph(&mut self, graph: &Graph) {
        self.name(&graph.name);
        self.buffer(&graph.inputs);
        for (name, ty, default) in &graph.inputs {
            self.name(name); self.ty(ty);
            if let Some(default) = default { self.term(default); }
        }
        self.syntax.add(&graph.form);
        self.term(&graph.body);
    }

    fn term(&mut self, term: &Term) {
        term.arena().attribute(&mut self.checked, &mut self.syntax);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpSignature { pub pos:Vec<(Symbol,Ty)>,pub opt:Vec<(Symbol,Ty)>,pub rest:Option<(Symbol,Ty)>,pub kw:Vec<(Symbol,Ty)> }
#[derive(Clone)]
pub struct Prelude { checker: Rc<Checker> }
pub fn prelude_ops_match(prelude: &Prelude, ops: &[op::Op]) -> bool { op::same_declarations(&prelude.checker.ops, ops) }
pub const MAX_ITERATIONS:usize=4096;
pub const BODY_CONTEXT_OPS:&[&str]=&["sop/context_seed","sop/provenance_base","sop/context_frame","sop/context_time"];
static SPECIAL: std::sync::LazyLock<Vec<&str>> = std::sync::LazyLock::new(|| crate::registry::FORMS.iter().map(|entry| entry.0).collect());
pub fn special_forms()-> &'static [&'static str] { &SPECIAL }
pub fn context_name(ctx:&Context)->String { context::name(*ctx).to_owned() }
pub fn max_iterations()->usize { MAX_ITERATIONS }
pub fn value_ops()->Vec<String> { op::all().iter().filter(|o|o.ctx==context::value()).map(|o|o.name.clone()).collect() }
pub fn op_signature(ops:&[op::Op],ctx:&Context,name:&str)->Option<OpSignature>{op::find(ops,name,ctx).map(|o|OpSignature{pos:o.signature.pos.clone(),opt:o.signature.opt.clone(),rest:o.signature.rest.clone(),kw:o.signature.kw.clone()})}
pub fn op_choices(ops:&[op::Op],name:&str,argument:&str)->Vec<String>{op::find(ops,name,&context::value()).and_then(|o|o.choices.iter().find(|(n,_)|n==argument).map(|(_,v)|v.clone())).unwrap_or_default()}
pub fn name_taken(name:&str)->bool { SPECIAL.contains(&name) || flow_syntax::symbol::reserved(name) || ty::names().iter().any(|(n,_)|n==name) || op::find(&[],name,&context::value()).is_some() }

pub fn skip_tuples(form: &Syntax) -> Option<Vec<Vec<usize>>> {
    fn number(form: &Syntax, id: syntax::SyntaxId) -> Option<usize> {
        let SNode::Num(name) = form.arena().row(id).node else { return None; };
        let value = form.name(name).parse::<i64>().ok()?;
        if value < 0 { None } else { usize::try_from(value).ok() }
    }
    fn tuple(form: &Syntax, id: syntax::SyntaxId) -> Option<Vec<usize>> {
        match form.arena().row(id).node {
            SNode::Vec(range) if !form.range(range).is_empty() => form.range(range).iter().map(|&id| number(form, id)).collect(),
            _ => number(form, id).map(|value| vec![value]),
        }
    }
    match form.node {
        SNode::Vec(range) => form.range(range).iter().map(|&id| tuple(form, id)).collect(),
        _ => None,
    }
}

pub fn kind_calls(term: &Term) -> Vec<String> {
    let arena = term.arena();
    let mut out = BTreeSet::new();
    let mut stack = vec![term.row_id()];
    while let Some(id) = stack.pop() {
        if let Node::Call(call) = arena.row(id).node { out.insert(arena.name(arena.call(call).kind).to_owned()); }
        stack.extend_from_slice(arena.children(id));
    }
    out.into_iter().collect()
}

pub fn bypass_input(term: &Term) -> Option<Term> {
    term.arena().bypass_input(term.row_id()).map(|id| term.child(id))
}

pub fn slot_ty(slot:Option<&catalog::Slot>,kind:&catalog::Kind)->Ty { if let Some(t)=slot.and_then(|s|s.ty.as_ref()){return t.clone()} match kind.qualified.as_str(){"scene/world"=>ty::world(),"scene/root"=>ty::scene(),_ if kind.context==context::scene()=>ty::geometry(),_=>context::result(kind.context)} }
pub fn group_reader(p:&catalog::Parameter)->bool { p.name=="group" }
pub fn group_writer(k:&catalog::Kind,p:&catalog::Parameter)->bool { p.name=="name"&&k.qualified.starts_with("sop/group_") }

fn err(code:&str,message:String,span:Option<Span>)->Diagnostic { diagnostic::error(code.to_owned(),message,span,None,None,Vec::new(),Vec::new()) }
fn add_to(diags: &mut Vec<Diagnostic>, severity: Severity, form: &Syntax, code: &str, message: String) {
    let span = if form.span.start == 0 && form.span.finish == 0 { None } else { Some(form.span.clone()) };
    let mut diagnostic = match severity {
        Severity::ErrorSeverity => err(code, message, span),
        Severity::Warning => diagnostic::warning(span, code.to_owned(), message),
    };
    if let Some(name) = syntax::head(form) { diagnostic.set_signature(crate::registry::signature(name)); }
    diags.push(diagnostic);
}
fn ty_port(t:&Ty)->Option<port_type::PortType>{match t{Ty::Float=>Some(port_type::PortType::Float),Ty::Int=>Some(port_type::PortType::Int),Ty::Bool=>Some(port_type::PortType::Bool),Ty::Vec3=>Some(port_type::PortType::Vec3),Ty::Named(n) if n=="geometry"=>Some(port_type::PortType::Geometry),Ty::Named(n) if n=="image"=>Some(port_type::PortType::Image),Ty::Fn(Some(s))=>Some(port_type::PortType::Fn((**s).clone())),_=>None}}
fn port_ty(t:&port_type::PortType)->Ty {match t{port_type::PortType::Float=>Ty::Float,port_type::PortType::Int=>Ty::Int,port_type::PortType::Bool=>Ty::Bool,port_type::PortType::Vec3=>Ty::Vec3,port_type::PortType::Geometry=>ty::geometry(),port_type::PortType::Image=>ty::image(),port_type::PortType::Fn(s)=>Ty::Fn(Some(Box::new(s.clone())))}}

fn is_color(parameter: &catalog::Parameter) -> bool {
    parameter.ty == Some(port_type::PortType::Vec3)
        && parameter.fields.len() == 3
        && parameter.fields[0].0.ends_with("_r")
        && parameter.fields[1].0.ends_with("_g")
        && parameter.fields[2].0.ends_with("_b")
}

fn parameter_ty(parameter: &catalog::Parameter) -> Ty {
    match &parameter.ty {
        Some(port_type::PortType::Vec3) if is_color(parameter) => Ty::Color,
        Some(port) => port_ty(port),
        None => Ty::Text,
    }
}

fn hex_colour(bytes: &[u8]) -> bool {
    matches!(bytes.len(), 4 | 7 | 9)
        && bytes.first() == Some(&b'#')
        && bytes[1..].iter().all(u8::is_ascii_hexdigit)
}

fn show(ty: &Ty) -> String {
    ty::to_string(ty).replace("list:", "list of ")
}

fn shape_ty(ty: &Ty) -> bool {
    match ty {
        Ty::Named(_name) => ty::shape(ty),
        Ty::List(element) => shape_ty(element),
        Ty::Record(fields) => fields.iter().any(|(_, ty)| shape_ty(ty)),
        _ => false,
    }
}

fn cooked(ty: &Ty) -> bool {
    ty::is_cooked(ty)
}


fn path_text(path: &[Symbol]) -> String {
    symbol::join_path(path, "/")
}

fn num_param(source: &str) -> Option<param::Value> {
    let number = syntax::number_value(source)?;
    if !source.contains('.') && !source.contains('e') && !source.contains('E') {
        if let Ok(value) = source.parse::<i64>() {
            return Some(param::Value::Int(value));
        }
    }
    Some(param::Value::Float(number))
}

fn opaque_head(name: &str) -> bool {
    [
        "fn", "map", "filter", "reduce", "sort_by", "iterate", "if", "cond", "case",
        "with_error", "error_rows", "exact",
    ]
    .contains(&name)
}

fn list_fields(ty: &Ty) -> Vec<Path> {
    match ty {
        Ty::List(_) => vec![Vec::new()],
        Ty::Record(fields) => fields
            .iter()
            .flat_map(|(name, ty)| {
                list_fields(ty).into_iter().map(|mut path| {
                    path.insert(0, *name);
                    path
                })
            })
            .collect(),
        _ => Vec::new(),
    }
}

fn geometry_accumulator(ty: &Ty) -> bool {
    match ty {
        ty if *ty == ty::geometry() => true,
        Ty::Record(fields) => {
            fields.iter().any(|(_, ty)| shape_ty(ty))
                && fields.iter().all(|(_, ty)| !shape_ty(ty) || geometry_accumulator(ty))
        }
        _ => false,
    }
}

fn packed_array(ty: &Ty) -> bool {
    flow_syntax::packed_ops::array_width(ty).is_some()
}


fn bypass_input_id(terms: &TermBuilder, term: TermId) -> Option<TermId> {
    let syntax = terms.syntax();
    let form = syntax.row(terms.row(term).form);
    let mut first = None;
    let mut match_form = false;
    if let SNode::List(range) = form.node {
        let forms = syntax.range_ids(range);
        if !forms.is_empty() {
            match_form = true;
            let mut rest = &forms[1..];
            while let Some((&id, tail)) = rest.split_first() {
                if matches!(syntax.row(id).node, SNode::Kw(_)) && !tail.is_empty() { rest = &tail[1..]; }
                else { first = Some(syntax.row(id)); break; }
            }
            first?;
        }
    }
    let selected = |id: TermId| {
        if !match_form { return true; }
        let candidate = syntax.row(terms.row(id).form);
        let first = first.unwrap();
        candidate.id == first.id && candidate.span == first.span
    };
    match terms.node(term) {
        PackedNode::Call(id) => terms.field_ids(terms.call(id).args).iter().map(|field| field.term).find(|&id| selected(id)),
        PackedNode::Op(id) => terms.field_ids(terms.op(id).args).iter().map(|field| field.term).find(|&id| selected(id)),
        PackedNode::CallFn(id) => terms.term_ids(terms.call_fn(id).args).iter().copied().find(|&id| selected(id)),
        _ => None,
    }
}

fn packed_body(ops: &Arc<Vec<op::Op>>, terms: &TermBuilder, term: TermId) -> bool {
    let packed = |name: &str| op::find_checked(ops, name, &context::value()).and_then(|(_, metadata)| metadata.packed_kind);
    let body = |term: TermId| packed_body(ops, terms, term);
    match terms.node(term) {
        PackedNode::Lit(id) => matches!(terms.literal(id), packed::Literal::Int(_) | packed::Literal::Float(_) | packed::Literal::Bool(_)),
        PackedNode::Time | PackedNode::Text(_) => true,
        PackedNode::RefBinding(_) => *terms.ty(term) == Ty::Text || flow_syntax::packed_ops::scalar_width(terms.ty(term)).is_some(),
        PackedNode::Vec(range) => {
            let items = terms.term_ids(range);
            matches!(items.len(), 2 | 3 | 4) && items.iter().all(|&id| body(id))
        }
        PackedNode::Get(id) => {
            let value = terms.get(id);
            let field = terms.name(value.name);
            let scalar_field = match terms.ty(value.value) {
                Ty::Vec2 => ["x", "y"].contains(&field), Ty::Vec3 => ["x", "y", "z"].contains(&field),
                Ty::Vec4 => ["x", "y", "z", "w"].contains(&field),
                Ty::Record(fields) => fields.iter().any(|(name, _)| name == field), _ => false,
            };
            scalar_field && body(value.value)
        }
        PackedNode::Record(range) => terms.field_ids(range).iter().all(|field| body(field.term)),
        PackedNode::CallFn(id) => {
            let call = terms.call_fn(id);
            call.body.is_some_and(|specialized| terms.term_ids(call.args).iter().all(|&id| body(id)) && body(specialized))
        }
        PackedNode::Op(id) => {
            let operator = terms.op(id);
            let args = terms.field_ids(operator.args);
            let Some(capability) = packed(terms.name(operator.op)) else { return false; };
            match capability {
                op::PackedKind::Frame => args.is_empty() && matches!(terms.ty(term), Ty::Float | Ty::Int | Ty::Bool),
                op::PackedKind::Noise3 => args.iter().all(|field| {
                    let literal = match terms.node(field.term) { PackedNode::Lit(id) => Some(terms.literal(id)), _ => None };
                    match (terms.name(field.name), literal) {
                        ("position", _) => body(field.term), ("seed", Some(packed::Literal::Int(_))) => true,
                        ("octaves", Some(packed::Literal::Int(value))) => flow_syntax::packed_ops::supported_noise_octaves(value),
                        ("lacunarity" | "gain", Some(packed::Literal::Float(_))) => true, _ => false,
                    }
                }),
                op::PackedKind::Length | op::PackedKind::Normalize => matches!(args, [value] if *terms.ty(value.term) == Ty::Vec3 && body(value.term)),
                op::PackedKind::Dot | op::PackedKind::Cross => args.len() == 2 && args.iter().all(|value| *terms.ty(value.term) == Ty::Vec3 && body(value.term)),
                op::PackedKind::Exact => matches!(args, [value] if body(value.term)),
                op::PackedKind::Fma | op::PackedKind::CheckedFma => args.len() == 3 && args.iter().all(|value| body(value.term)),
                op::PackedKind::Trace => args.len() == 6 && args.iter().all(|value| body(value.term)),
                op::PackedKind::Stage => args.iter().all(|value| body(value.term)),
                op::PackedKind::CheckedMul => args.len() == 2 && args.iter().all(|value| body(value.term)),
                op::PackedKind::Gather => matches!(args, [array,index] if (matches!(terms.ty(array.term), Ty::Array(element) if **element == Ty::Text) || packed_array(terms.ty(array.term))) && body(index.term)),
                op::PackedKind::Binary(_) | op::PackedKind::Integer(_) | op::PackedKind::IntegerCompare(_) | op::PackedKind::RandAt => args.len() == 2 && args.iter().all(|value| body(value.term)),
                op::PackedKind::Unary(_) | op::PackedKind::Convert(_) => args.len() == 1 && args.iter().all(|value| body(value.term)),
                op::PackedKind::ConstantOnly => false,
            }
        }
        PackedNode::Let(id) => {
            let node = terms.let_node(id);
            terms.binding_ids(node.bindings).iter().all(|binding| matches!(terms.pattern_node(binding.pattern), packed::PatternNode::Name(_)) && body(binding.term)) && body(node.body)
        }
        PackedNode::If(id) => { let node = terms.if_node(id); body(node.test) && body(node.yes) && body(node.no) }
        PackedNode::Cond(id) => { let node = terms.cond(id); body(node.default) && terms.arm_ids(node.arms).iter().all(|arm| body(arm.test) && body(arm.body)) }
        PackedNode::Case(id) => {
            let node = terms.case(id);
            body(node.value) && body(node.default) && terms.case_arm_ids(node.arms).iter().all(|arm| {
                let literal = match terms.syntax().row(arm.form).node {
                    SNode::Num(_) | SNode::Str(_) => true,
                    SNode::Sym(name) => matches!(terms.syntax().name(name), "true" | "false"), _ => false,
                };
                literal && body(arm.body)
            })
        }
        PackedNode::Expanded(id) => body(terms.expanded(id).body),
        PackedNode::Bypass(call) => bypass_input_id(terms, call).is_some_and(|input| {
            (terms.ty(input) == terms.ty(term) || (*terms.ty(term) == Ty::Float && matches!(terms.ty(input), Ty::Int | Ty::Bool))
                || (matches!(terms.ty(term), Ty::Vec2 | Ty::Vec3 | Ty::Vec4) && matches!(terms.ty(input), Ty::Int | Ty::Float))) && body(input)
        }),
        _ => false,
    }
}

#[derive(Clone)]
struct Info {
    ty: Ty,
    live: bool,
    live_len: bool,
    len: Option<usize>,
    callable: Option<Rc<Callable>>,
    vary: BTreeSet<usize>,
    groups: BTreeSet<String>,
    packed: BTreeSet<CheckPath>,
    approx: bool,
    approx_sources: BTreeSet<CheckPath>,
    state_dep: bool,
    list_fields: Vec<Path>,
}

impl Info {
    fn leaf(ty: Ty) -> Self {
        Self {
            ty,
            live: false,
            live_len: false,
            len: None,
            callable: None,
            vary: BTreeSet::new(),
            groups: BTreeSet::new(),
            packed: BTreeSet::new(),
            approx: false,
            approx_sources: BTreeSet::new(),
            state_dep: false,
            list_fields: Vec::new(),
        }
    }

    /// Equal for checking: a callable compares by its closure's identity or its name.
    fn same(&self, other: &Info) -> bool {
        let callable = match (&self.callable, &other.callable) {
            (None, None) => true,
            (Some(a), Some(b)) => Rc::ptr_eq(a, b) || match (&**a, &**b) {
                (Callable::Local(a), Callable::Local(b)) => Rc::ptr_eq(a, b),
                (Callable::Def(a), Callable::Def(b)) | (Callable::Op(a), Callable::Op(b)) => a == b,
                (Callable::Kind(a), Callable::Kind(b)) => a == b,
                _ => false,
            },
            _ => false,
        };
        callable && self.ty == other.ty && self.live == other.live && self.live_len == other.live_len && self.len == other.len
            && self.vary == other.vary && self.groups == other.groups && self.packed == other.packed && self.approx == other.approx
            && self.approx_sources == other.approx_sources && self.state_dep == other.state_dep && self.list_fields == other.list_fields
    }

    fn derive(ty: Ty, values: &[Info]) -> Self {
        let mut result = Self::leaf(ty);
        for value in values {
            result.live |= value.live;
            result.state_dep |= value.state_dep;
            result.vary.extend(value.vary.iter().copied());
            result.groups.extend(value.groups.iter().cloned());
            result.packed.extend(value.packed.iter().cloned());
            result.approx_sources.extend(value.approx_sources.iter().cloned());
            result.list_fields.extend(value.list_fields.iter().cloned());
        }
        result.list_fields.sort();
        result.list_fields.dedup();
        result
    }
}

/// Lexical bindings in stack order.
#[derive(Clone, Default)]
struct ScopeStack {
    layers: Rc<Vec<Rc<BTreeMap<String, Info>>>>,
}
impl ScopeStack {
    fn push(&self) -> Self {
        let mut layers = self.layers.as_ref().clone();
        layers.push(Rc::new(BTreeMap::new()));
        Self { layers: Rc::new(layers) }
    }
    fn from_map(bindings: BTreeMap<String, Info>) -> Self {
        Self { layers: Rc::new(vec![Rc::new(bindings)]) }
    }
    fn snapshot(&self) -> Self {
        self.clone()
    }
    fn get(&self, name: &str) -> Option<&Info> {
        for map in self.layers.iter().rev() {
            if let Some(value) = map.get(name) {
                return Some(value);
            }
        }
        None
    }
    fn contains_key(&self, name: &str) -> bool {
        self.get(name).is_some()
    }
    fn is_empty(&self) -> bool {
        self.layers.is_empty() || self.layers.iter().all(|map| map.is_empty())
    }
    fn binding_names(&self) -> Vec<String> {
        let mut names = BTreeSet::new();
        for map in &*self.layers {
            names.extend(map.keys().cloned());
        }
        names.into_iter().collect()
    }
    fn top_mut(&mut self) -> &mut BTreeMap<String, Info> {
        let layers = Rc::make_mut(&mut self.layers);
        if layers.is_empty() {
            layers.push(Rc::new(BTreeMap::new()));
        }
        let map = layers.last_mut().expect("scope stack layer");
        Rc::make_mut(map)
    }
}

#[derive(Clone)]
enum Callable{Local(Rc<Closure>),Def(String),Op(String),Kind(catalog::Kind)}
#[derive(Clone)]
struct Closure{params:Vec<(Syntax,Option<Ty>)>,body:Syntax,env:ScopeStack,ctx:Context,path:CheckPath,name:String}
#[derive(Clone)]
struct Sig{name:String,ctx:Context,params:Vec<(String,Ty,Option<Syntax>)>,body:Syntax,form:Syntax}
#[derive(Clone)]
struct Cx{env:ScopeStack,ctx:Context,path:CheckPath,scope:String,in_def:bool,stack:Vec<String>,depth:usize,zone:Option<usize>,zbody:bool}
impl Cx {
    /// A clone at another path, without copying the path it replaces.
    fn with_path(&self, path: CheckPath) -> Cx {
        Cx { env: self.env.clone(), ctx: self.ctx, path, scope: self.scope.clone(), stack: self.stack.clone(), ..*self }
    }
}
#[derive(Clone)]
struct Arg{key:Option<String>,form:Syntax,term:TermId,info:Info}
/// A `defn` body checked at a call site with these inputs. Repeating the check would add the
/// same terms (fresh rows), the same idempotent marks and diagnostics, and `steps` more steps,
/// as long as nothing counted (`Checker::events`) and the call stack and depth agree on what the
/// body probed: no name in `probes` on the stack, closures `reach` levels deeper within the limit.
/// `T` is the body: a row of the current builder, or a sealed term once its builder finished.
/// A closure's memo holds the closure, so its address (in the key) is not reused.
#[derive(Clone)]
struct CallMemo<T> { env: Vec<Info>, probes: Vec<String>, reach: Option<usize>, steps: usize, term: T, value: Info, closure: Option<Rc<Closure>> }
type CallMemos<T> = BTreeMap<String, Vec<CallMemo<T>>>;

/// Paths belong to the checker (including its immutable prelude prefix), never a swapped TermBuilder.
type CheckPath = usize;

struct Checker {
    base: Option<Rc<Checker>>,
    paths: Vec<Arc<[Symbol]>>,
    path_index: flow_syntax::arena::FastMap<Arc<[Symbol]>, CheckPath>,
    terms: TermBuilder,
    catalog: Rc<catalog::Catalog>,
    ops: Arc<Vec<op::Op>>,
    library: bool,
    call_sites: bool,
    diags: Vec<Diagnostic>,
    live: BTreeSet<CheckPath>,
    invariant: BTreeSet<CheckPath>,
    packed: BTreeMap<CheckPath, CheckPackedForm>,
    roots: Vec<(Syntax, CheckPath)>,
    defs: BTreeMap<String, Sig>,
    graphs: BTreeMap<String, Sig>,
    macros: BTreeMap<String, Syntax>,
    macro_forms: Vec<Syntax>,
    node_defs: BTreeMap<String, defnode::Defnode>,
    nodes: Vec<Arc<NodeDefinition>>,
    graph_terms: BTreeMap<String, Graph>,
    ginfo: BTreeMap<String, Info>,
    def_terms: BTreeMap<String, Graph>,
    kind_fns: BTreeMap<String, (String, Context, Vec<catalog::Slot>)>,
    inline: BTreeMap<CheckPath, usize>,
    input_paths: BTreeMap<CheckPath, BTreeMap<(i64, String), CheckPath>>,
    checked_defs: BTreeSet<String>,
    graph_order: Vec<String>,
    def_order: Vec<String>,
    node_order: Vec<String>,
    declared_names: BTreeSet<String>,
    steps: usize,
    count: usize,
    children: usize,
    zone_id: usize,
    graph_stack: Vec<String>,
    /// Call-site bodies checked into `terms`; taken and restored with it.
    calls: CallMemos<TermId>,
    /// Call-site bodies of finished builders, and those the prelude kept for its workspaces.
    /// ponytail: kept for the checker's (and a cached prelude's) lifetime; bound it if retained
    /// memory matters.
    kept: CallMemos<Term>,
    inherited: Rc<CallMemos<Term>>,
    /// Counter-driven paths, zone ids, graph references, budget and recursion or depth failures:
    /// a body that bumps this checks differently when repeated, so it is not memoised.
    events: usize,
    /// The deepest closure call and the definitions checked against the stack, since the
    /// innermost memoised call began.
    depth_seen: Option<usize>,
    probes: Vec<String>,
}

impl Checker {
    fn new(catalog: Rc<catalog::Catalog>, ops: Arc<Vec<op::Op>>, library: bool, call_sites: bool) -> Self {
        Self {
            base: None, paths: Vec::new(), path_index: Default::default(),
            terms: TermBuilder::new(),
            catalog,
            ops,
            library,
            call_sites,
            diags: Vec::new(),
            live: BTreeSet::new(),
            invariant: BTreeSet::new(),
            packed: BTreeMap::new(),
            roots: Vec::new(),
            defs: BTreeMap::new(),
            graphs: BTreeMap::new(),
            macros: BTreeMap::new(),
            macro_forms: Vec::new(),
            node_defs: BTreeMap::new(),
            nodes: Vec::new(),
            graph_terms: BTreeMap::new(),
            ginfo: BTreeMap::new(),
            def_terms: BTreeMap::new(),
            kind_fns: BTreeMap::new(),
            inline: BTreeMap::new(),
            input_paths: BTreeMap::new(),
            checked_defs: BTreeSet::new(),
            graph_order: Vec::new(),
            def_order: Vec::new(),
            node_order: Vec::new(),
            declared_names: BTreeSet::new(),
            steps: 0,
            count: 0,
            children: 0,
            zone_id: 0,
            graph_stack: Vec::new(),
            calls: BTreeMap::new(),
            kept: BTreeMap::new(),
            inherited: Rc::default(),
            events: 0,
            depth_seen: None,
            probes: Vec::new(),
        }
    }
    fn fork(base: Rc<Checker>) -> Self {
        let mut checker = Self::new(base.catalog.clone(), base.ops.clone(), base.library, base.call_sites);
        checker.diags = base.diags.clone();
        checker.steps = base.steps; checker.count = base.count; checker.children = base.children;
        checker.zone_id = base.zone_id; checker.events = base.events;
        checker.inherited = base.inherited.clone();
        checker.base = Some(base);
        checker
    }
    fn path_names(&self, path: CheckPath) -> &Arc<[Symbol]> {
        let prefix = self.base.as_ref().map_or(0, |base| base.paths.len());
        if path < prefix { &self.base.as_ref().unwrap().paths[path] } else { &self.paths[path - prefix] }
    }
    fn path(&mut self, names: Path) -> CheckPath {
        if let Some(id) = self.path_index.get(names.as_slice()) { return *id; }
        if let Some(id) = self.base.as_ref().and_then(|base| base.path_index.get(names.as_slice())) { return *id; }
        let id = self.base.as_ref().map_or(0, |base| base.paths.len()) + self.paths.len();
        let names: Arc<[Symbol]> = names.into();
        self.paths.push(names.clone()); self.path_index.insert(names, id); id
    }
    fn child_path(&mut self, parent: CheckPath, name: Symbol) -> CheckPath {
        let mut names = self.path_names(parent).to_vec(); names.push(name); self.path(names)
    }
    fn packed_update(&mut self, path: CheckPath, next: CheckPackedForm) {
        let old = self.packed.get(&path).or_else(|| self.base.as_ref()?.packed.get(&path)).cloned();
        let merged=match old { Some(old)=>Self::merge_packed(old,next), None=>next };
        self.packed.insert(path, merged);
    }
    fn defs_get(&self, name: &str) -> Option<&Sig> { self.defs.get(name).or_else(|| self.base.as_ref()?.defs.get(name)) }
    fn graphs_get(&self, name: &str) -> Option<&Sig> { self.graphs.get(name).or_else(|| self.base.as_ref()?.graphs.get(name)) }
    fn node_defs_get(&self, name: &str) -> Option<&defnode::Defnode> { self.node_defs.get(name).or_else(|| self.base.as_ref()?.node_defs.get(name)) }
    fn ginfo_get(&self, name: &str) -> Option<&Info> { self.ginfo.get(name).or_else(|| self.base.as_ref()?.ginfo.get(name)) }
    fn def_terms_get(&self, name: &str) -> Option<&Graph> { self.def_terms.get(name).or_else(|| self.base.as_ref()?.def_terms.get(name)) }
    fn macros_contains(&self, name: &str) -> bool { self.macros.contains_key(name) || self.base.as_ref().is_some_and(|base| base.macros.contains_key(name)) }
    fn declared_names_contains(&self, name: &str) -> bool { self.declared_names.contains(name) || self.base.as_ref().is_some_and(|base| base.declared_names.contains(name)) }
    fn checked_defs_contains(&self, name: &str) -> bool { self.checked_defs.contains(name) || self.base.as_ref().is_some_and(|base| base.checked_defs.contains(name)) }
 fn make_term(&mut self, form: &Syntax, ty: Ty, node: Draft) -> TermId {
     let form = self.terms.import_form(form);
     self.terms.push(None, &ty, form, node)
 }
 fn set_path(&mut self, term: TermId, path: CheckPath) -> TermId { let path=self.path_names(path).clone(); self.terms.with_path(term, Some(&path)) }
 fn term_form(&self, term: TermId) -> &Syntax { self.terms.source_form(term).expect("checked term source form") }
 fn add(&mut self,sev:Severity,form:&Syntax,code:&str,msg:String){add_to(&mut self.diags,sev,form,code,msg)}
 fn error(&mut self,f:&Syntax,c:&str,m:&str){self.add(Severity::ErrorSeverity,f,c,m.to_owned())}
 fn warn(&mut self,f:&Syntax,c:&str,m:&str){self.add(Severity::Warning,f,c,m.to_owned())}
 fn warn_owned(&mut self,f:&Syntax,c:&str,m:String){self.add(Severity::Warning,f,c,m)}
 fn inline_path(&mut self,cx:&Cx,seg:&str)->CheckPath {
     self.events+=1;
     let key=self.child_path(cx.path,symbol::intern(seg));
     let n=self.inline.get(&key).or_else(||self.base.as_ref()?.inline.get(&key)).copied().unwrap_or(0);
     self.inline.insert(key,n+1);
     if n==0 {key} else {self.child_path(cx.path,format!("{seg}~{n}").into())}
 }
 fn merge_packed(old: CheckPackedForm, next: CheckPackedForm) -> CheckPackedForm {
     match (old, next) {
         (CheckPackedForm::Candidate(mut left), CheckPackedForm::Candidate(right)) => {
             if !right.is_subset(&left) { Arc::make_mut(&mut left).extend(right.iter().cloned()); }
             CheckPackedForm::Candidate(left)
         }
         (CheckPackedForm::Refused(mut left), CheckPackedForm::Refused(right)) => {
             for diagnostic in right {
                 if !left.iter().any(|old| old.code == diagnostic.code && old.span == diagnostic.span && old.message == diagnostic.message) {
                     left.push(diagnostic);
                 }
             }
             CheckPackedForm::Refused(left)
         }
         (refusal @ CheckPackedForm::Refused(_), _) => refusal,
         (candidate @ CheckPackedForm::Candidate(_), CheckPackedForm::Refused(_)) => candidate,
     }
 }

 fn mark(&mut self, path: CheckPath, value: &Info) {
     if value.live {
         self.live.insert(path);
     }
     if !value.packed.is_empty() {
         let next = CheckPackedForm::Candidate(Arc::new(value.packed.clone()));
         self.packed_update(path, next);
     }
 }

 fn add_root(&mut self, form: &Syntax, path: CheckPath, refusal: Option<Diagnostic>) {
     if !self.base.iter().flat_map(|base|base.roots.iter()).chain(self.roots.iter()).any(|(old, owner)| old.id == form.id && owner == &path) {
         self.roots.push((form.clone(), path));
     }
     let next = match refusal {
         Some(reason) => CheckPackedForm::Refused(vec![reason]),
         None => CheckPackedForm::Candidate(Arc::new([path].into_iter().collect())),
     };
     self.packed_update(path, next);
     let producer = CheckPackedForm::Candidate(Arc::new([path].into_iter().collect()));
     self.packed_update(path, producer);
 }

 fn fail_term(&mut self, form: &Syntax, code: &str, message: &str) -> (TermId, Info) {
     self.error(form, code, message);
     (self.make_term(form, Ty::Any, Draft::Nil), Info::leaf(Ty::Any))
 }

 fn exact_sink(&mut self, cx: &Cx, form: &Syntax, sink: &str, value: &Info) {
     let mut sources = value.approx_sources.clone();
     if value.approx && sources.is_empty() {
         sources.insert(cx.path);
     }
     if !sources.is_empty() {
         let producers = sources.iter().map(|path| self.path_names(*path).to_vec()).collect::<Paths>().iter().map(|path|path_text(path)).collect::<Vec<_>>().join(", ");
         self.error(form, "E_APPROX_SINK", &format!("Approximate values require (exact x) before catalog calls, exports, state or cache keys. Producer: {producers}; sink: {} ({sink}).", path_text(self.path_names(cx.path))));
     }
 }

 fn budget(&self) -> usize { 400_000 * (self.count + self.children).max(1) }
 fn infer(&mut self, cx: &Cx, form: &Syntax) -> (TermId, Info) {
     self.steps += 1;
     if self.steps > self.budget() {
         self.events += 1;
         return self.fail_term(form, "E_BUDGET", "The workspace is too large to check: call-site typing exceeded its budget. Reduce nested function calls.");
     }
     let mut result = if !form.meta().is_empty() {
         if let Some(metadata) = form.meta().iter().find(|name| name.as_str() != "bypass") {
             self.fail_term(form, "E_META", &format!("Unknown metadata ^:{metadata}. The only metadata is ^:bypass."))
         } else {
             self.bypass(cx, form)
         }
     } else {
         self.infer_plain(cx, form)
     };
     if result.1.approx && result.1.approx_sources.is_empty() {
         result.1.approx_sources.insert(cx.path);
     }
     result
 }

 fn infer_plain(&mut self, cx: &Cx, form: &Syntax) -> (TermId, Info) {
    match form.node {
        SNode::Num(raw) => match num_param(form.name(raw)) {
            Some(param::Value::Int(value)) => (self.make_term(form, Ty::Int, Draft::Lit(param::Value::Int(value))), Info::leaf(Ty::Int)),
            Some(param::Value::Float(value)) => (self.make_term(form, Ty::Float, Draft::Lit(param::Value::Float(value))), Info::leaf(Ty::Float)),
            _ => self.fail_term(form, "E_TYPE", "Nonfinite number."),
        },
        SNode::Str(bytes) => (self.make_term(form, Ty::Text, Draft::Text(form.shared_bytes(bytes))), Info::leaf(Ty::Text)),
        SNode::Kw(key) => self.fail_term(form, "E_KEYWORD", &format!("Keyword :{} has no call to belong to.", form.name(key))),
        SNode::Sym(name) => self.symbol(cx, form, form.name(name)),
        SNode::Vec(items) => self.vector(cx, form, form.range(items)),
        SNode::Map(items) => self.record(cx, form, form.range(items), "A record"),
        SNode::Quote(_, _) => self.fail_term(form, "E_QUOTE", "Quotes belong in a defmacro template (`…)."),
        SNode::List(items) if form.range(items).is_empty() => self.fail_term(form, "E_EXPR", "Expected an expression."),
        SNode::List(items) => {
            let items = form.range(items);
            match form.arena().row(items[0]).node {
                SNode::Sym(head) => self.call(cx, form, form.name(head), &items[1..]),
                _ => self.fail_term(form, "E_CALL", "A call head is a name."),
            }
        }
    }
}

 fn bypass(&mut self, cx: &Cx, form: &Syntax) -> (TermId, Info) {
     let SNode::List(items) = &form.node else {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {}: only operator and function calls pass an input through.", lisp::flat(form)));
     };
     let items = form.range(*items);
    let Some(SNode::Sym(head)) = items.first().map(|item| form.arena().row(*item).node) else {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {}: only operator and function calls pass an input through.", lisp::flat(form)));
     };
     let head = form.name(head);
    if head == "if" && items.len() == 4 {
         let (_, checked) = self.infer_plain(cx, form);
         let (mut branch, mut value) = self.branch(cx, "then", &form.child(items[2]));
         branch = self.terms.with_type(branch, &checked.ty);
         branch = self.terms.with_form(branch, form);
         value.ty = checked.ty;
         return (branch, value);
     }
     if SPECIAL.contains(&head) || self.macros_contains(head) {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {head}: only operator and function calls pass an input through."));
     }
     let args = &items[1..];
     let first_positional = { let mut i = 0; loop { match args.get(i) { None => break false, Some(id) if matches!(form.arena().row(*id).node, SNode::Kw(_)) && i + 1 < args.len() => i += 2, Some(_) => break true } } };
    if !first_positional {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {head}: it has no positional input to pass through."));
     }
     let (call, value) = self.infer_plain(cx, form);
     let Some(input) = bypass_input_id(&self.terms, call) else {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {head}: only operator and function calls pass an input through."));
     };
     if !ty::fits(self.terms.ty(input), &value.ty) || ty::has_fn(self.terms.ty(input)) {
         self.error(form, "E_BYPASS", &format!("can't bypass {head}: its input is {} but it returns {}.", show(self.terms.ty(input)), show(&value.ty)));
     }
     (self.make_term(form, value.ty.clone(), Draft::Bypass(call)), value)
 }
 fn symbol(&mut self,cx:&Cx,f:&Syntax,s:&str)->(TermId,Info){match s{"t"=>{let mut v=Info::leaf(Ty::Float);v.live=true;(self.make_term(f,Ty::Float,Draft::Time),v)},"pi"=>(self.make_term(f,Ty::Float,Draft::Lit(param::Value::Float(std::f64::consts::PI))),Info::leaf(Ty::Float)),"true"|"false"=>(self.make_term(f,Ty::Bool,Draft::Lit(param::Value::Bool(s=="true"))),Info::leaf(Ty::Bool)),"nil"=>(self.make_term(f,ty::geometry(),Draft::Nil),Info::leaf(ty::geometry())),_=>{
     let mut parts=s.split('.');let name=parts.next().unwrap_or(s);let fs=parts.map(str::to_owned).collect::<Vec<_>>();
     if let Some(mut v)=cx.env.get(name).cloned(){let mut t=self.make_term(f,v.ty.clone(),Draft::RefBinding(name.into(),fs.clone()));for field in &fs{v=self.field(f,&v,field,name);}t=self.terms.with_type(t,&v.ty);(t,v)}else if !fs.is_empty()&&["t","pi","true","false","nil"].contains(&name){self.fail_term(f,"E_FIELD",&format!("{name} is a constant; it has no .{}.",fs.join(".")))}else{self.error(f,"E_UNBOUND",&format!("{name} is not bound in {}.{}",cx.scope,if cx.env.is_empty(){String::new()}else{format!(" Bound here: {}.",cx.env.binding_names().join(", "))}));self.fail_term_no_diag(f)}
   }}}
 fn fail_term_no_diag(&mut self,f:&Syntax)->(TermId,Info){(self.make_term(f,Ty::Any,Draft::Nil),Info::leaf(Ty::Any))}
 fn field(&mut self, form: &Syntax, value: &Info, name: &str, base: &str) -> Info {
     let ty = match &value.ty {
         Ty::Any => Ty::Any,
         Ty::Vec2 if ["x", "y"].contains(&name) => Ty::Float,
         Ty::Vec3 if ["x", "y", "z"].contains(&name) => Ty::Float,
         Ty::Vec4 if ["x", "y", "z", "w"].contains(&name) => Ty::Float,
         Ty::Record(fields) => match fields.iter().find(|(field, _)| field == name) {
             Some((_, ty)) => ty.clone(),
             None => {
                 self.error(form, "E_FIELD", &format!("{base} has no field {name}. Fields: {}.", if fields.is_empty() { "none".into() } else { symbol::join_path(&fields.iter().map(|(field, _)| *field).collect::<Vec<_>>(), ", ") }));
                 Ty::Any
             }
         },
         _ => {
             self.error(form, "E_FIELD", &format!("{base} has no output {name}. A vec3 has .x .y .z; a record has its fields."));
             Ty::Any
         }
     };
     let mut result = Info::derive(ty, std::slice::from_ref(value));
     result.list_fields = value.list_fields.iter().filter_map(|path| {
         (path.first().is_some_and(|field| field == name)).then(|| path[1..].to_vec())
     }).collect();
     result.live_len = result.list_fields.iter().any(Vec::is_empty);
     result.len = None;
     result.callable = None;
     result
 }
 fn vector(&mut self,cx:&Cx,f:&Syntax,xs:&[SyntaxId])->(TermId,Info){let ty=match xs.len(){2=>Ty::Vec2,3=>Ty::Vec3,4=>Ty::Vec4,_=>{return self.fail_term(f,"E_VECTOR",&format!("A vector has 2, 3 or 4 components; this one has {}.",xs.len()))}};let mut ts=Vec::new();let mut vs=Vec::new();for id in xs{let x=&f.child(*id);let(t,v)=self.infer(cx,x);if !matches!(v.ty,Ty::Int|Ty::Float|Ty::Any){self.error(x,"E_TYPE",&format!("Vector components are numbers; got {}.",show(&v.ty)));}ts.push(t);vs.push(v);}let v=Info::derive(ty.clone(),&vs);(self.make_term(f,ty,Draft::Vec(ts)),v)}
 fn record(&mut self, cx: &Cx, form: &Syntax, items: &[SyntaxId], what: &str) -> (TermId, Info) {
     if items.len() % 2 != 0 {
         return self.fail_term(form, "E_RECORD", &format!("{what} is :key value pairs; a key has no value."));
     }
     let mut fields = Vec::new();
     let mut terms = Vec::new();
     let mut values = Vec::new();
     let mut list_fields = Vec::new();
     let mut seen = BTreeSet::new();
     let mut valid = true;
     for pair in items.chunks_exact(2) {
         let key = &form.child(pair[0]);
         let SNode::Kw(name) = key.node else {
             self.error(key, "E_RECORD", &format!("{what} keys are keywords like :size; got {}.", lisp::flat(key)));
             valid = false;
             continue;
         };
         let name = symbol::intern(key.name(name));
        if !seen.insert(name) {
             self.error(key, "E_RECORD", &format!("{what} gives :{name} twice."));
             valid = false;
             continue;
         }
         let (term, value) = self.infer(cx, &form.child(pair[1]));
         self.no_fn(&form.child(pair[1]), &value.ty, &format!("Record field :{name}"));
         fields.push((name.into(), self.terms.ty(term).clone()));
         terms.push((name, term));
         if value.live_len {
             list_fields.push(vec![name.into()]);
         }
         list_fields.extend(value.list_fields.iter().map(|path| {
             let mut path = path.clone();
             path.insert(0, name.into());
             path
         }));
         values.push(value);
     }
     if !valid {
         return (self.make_term(form, Ty::Any, Draft::Nil), Info::leaf(Ty::Any));
     }
     list_fields.sort();
     list_fields.dedup();
     let ty = Ty::Record(fields);
     let mut value = Info::derive(ty.clone(), &values);
     value.list_fields = list_fields;
     value.live_len = value.list_fields.iter().any(Vec::is_empty);
     (self.make_term(form, ty, Draft::Record(terms)), value)
 }
 fn no_fn(&mut self,f:&Syntax,t:&Ty,what:&str){if ty::has_fn(t){self.error(f,"E_FN_ESCAPES",&format!("{what} is a function; a function value cannot be stored or returned (E_FN_ESCAPES). Call it where it is bound."))}}
 fn input(&mut self, cx: &Cx, form: &Syntax, key: &str) -> (TermId, Info) {
     let head = syntax::head(form).unwrap_or("");
     if !head.contains('/') && !self.macros_contains(head) && !opaque_head(head) {
         return self.infer(cx, form);
     }
     let memo = (form.id, key.to_owned());
     let path = if let Some(path) = self.input_paths.get(&cx.path).and_then(|paths| paths.get(&memo)).or_else(|| self.base.as_ref()?.input_paths.get(&cx.path)?.get(&memo)) {
         *path
     } else {
         let mut stem = self.path_names(cx.path).to_vec();
         let leaf = stem.pop().unwrap_or_else(|| "@result".into());
         stem.push(format!("{leaf}#{key}").into());
         let stem_id = self.path(stem.clone());
         let previous = self.base.as_ref().and_then(|base| base.inline.get(&stem_id)).copied().unwrap_or(0);
         let count = self.inline.entry(stem_id).or_insert(previous);
         let path = if *count == 0 {
             stem
         } else {
             let mut unique = stem;
             if let Some(leaf) = unique.last_mut() {
                 *leaf = format!("{leaf}~{}", *count).into();
             }
             unique
         };
         *count += 1;
         let path = self.path(path);
         self.input_paths.entry(cx.path).or_default().insert(memo, path);
         path
     };
     if opaque_head(head) {
         return self.binding(cx, form, path);
     }
     let inner = cx.with_path(path);
     let (term, value) = self.infer(&inner, form);
     self.mark(path, &value);
     let term = if *self.terms.ty(term) == ty::drawing() || *self.terms.ty(term) == ty::image() {
         self.set_path(term, path)
     } else {
         term
     };
     (term, value)
 }
 fn bind(&mut self, cx: &Cx, pattern: &Syntax, value: Info, marker_text: &str, env: &mut BTreeMap<String, Info>) {
     if !matches!(&pattern.node, SNode::Sym(_)) {
         let mut path = self.path_names(cx.path).to_vec();
         path.push(format!("{marker_text}{}", Self::pattern_key(pattern)).into());
         let path=self.path(path);
         self.mark(path, &value);
     }
     match &pattern.node {
         SNode::Sym(name) => {
             let name = pattern.name(*name).to_owned();
             let mut path = self.path_names(cx.path).to_vec();
             path.push(format!("{marker_text}{name}").into());
             let path=self.path(path);
             self.mark(path, &value);
             env.insert(name, value);
         }
         SNode::Vec(patterns) => {
             let patterns = pattern.range(*patterns);
             let width = match &value.ty {
                 Ty::Vec2 => Some(2),
                 Ty::Vec3 => Some(3),
                 Ty::Vec4 => Some(4),
                 _ => None,
             };
             if let Some(width) = width {
                 if patterns.len() > width {
                     self.error(pattern, "E_PATTERN", &format!("{} needs {} elements; a {} has {}.", Self::pattern_key(pattern), patterns.len(), show(&value.ty), width));
                 }
             } else if !matches!(&value.ty, Ty::List(_) | Ty::Array(_) | Ty::Any) {
                 self.error(pattern, "E_PATTERN", &format!("{} destructures a list or vec3; got {}.", Self::pattern_key(pattern), show(&value.ty)));
             } else if value.len.is_some_and(|length| length < patterns.len()) {
                 self.error(pattern, "E_PATTERN", &format!("{} needs {} elements; the list has {}.", Self::pattern_key(pattern), patterns.len(), value.len.unwrap_or(0)));
             }
             let element = if width.is_some() { Ty::Float } else { ty::elem(&value.ty).unwrap_or(Ty::Any) };
             let mut part = value;
             part.ty = element;
             part.live_len = false;
             part.len = None;
             part.callable = None;
             for id in patterns {
                 self.bind(cx, &pattern.child(*id), part.clone(), marker_text, env);
             }
         }
         SNode::Map(items) => {
             let items = pattern.range(*items);
             if items.len() == 2 && matches!(pattern.arena().row(items[0]).node, SNode::Kw(key) if pattern.name(key) == "keys") {
                 if !matches!(&value.ty, Ty::Record(_) | Ty::Any) {
                     self.error(pattern, "E_PATTERN", &format!("{} destructures a record; got {}.", Self::pattern_key(pattern), show(&value.ty)));
                 }
                 if let SNode::Vec(keys) = pattern.arena().row(items[1]).node {
                     for id in pattern.range(keys) {
                         let key = pattern.child(*id);
                         if let SNode::Sym(name) = key.node {
                             let field = self.field(&key, &value, key.name(name), "pattern");
                             self.bind(cx, &key, field, marker_text, env);
                         }
                     }
                 }
             }
         }
         _ => (),
     }
 }

 fn check_name(&mut self, cx: &Cx, at: &Syntax, name: &str, seen: &mut BTreeSet<String>, what: &str, zone: bool) -> bool {
     let reserved = (SPECIAL.contains(&name) || ["t", "pi", "true", "false", "nil"].contains(&name)) && name != "state";
     if !macros::valid_name(name) || reserved {
         if zone {
             self.error(at, "E_BINDING", &format!("{what} binds names; {name} is not a valid one."));
         } else {
             self.error(at, "E_BINDING", &format!("Invalid binding name {name}{}.", if name == "t" { ": t is the context time" } else { "" }));
         }
         false
     } else if seen.contains(name) {
         self.error(at, "E_DUPLICATE", &format!("{name} is bound twice in one {what}."));
         false
     } else if cx.env.contains_key(name) {
         if zone {
             self.error(at, "E_SHADOW", &format!("{name} shadows an outer name. Pick another loop name."));
         } else {
             self.error(at, "E_SHADOW", &format!("{name} shadows an outer name in {}. Rename it; the graph cannot show two nodes called {name} on one wire.", cx.scope));
         }
         false
     } else {
         seen.insert(name.to_owned());
         true
     }
 }

 fn check_pattern(&mut self, cx: &Cx, pattern: &Syntax, seen: &mut BTreeSet<String>, _env: &ScopeStack, what: &str) -> bool {
     let zone = matches!(what, "for" | "fold" | "scan" | "sum" | "state");
     let shape = |p: &Syntax| if zone {
         format!("{what} binds names; {} is not a name or pattern.", Self::pattern_key(p))
     } else {
         format!("Invalid binding name {}. Bind a name, [a b] or {{:keys [a b]}}.", Self::pattern_key(p))
     };
     match &pattern.node {
         SNode::Sym(name) => self.check_name(cx, pattern, pattern.name(*name), seen, what, zone),
         SNode::Vec(items) if !pattern.range(*items).is_empty() => {
             let items = pattern.range(*items);
             let mut ok = true;
             for item in items { ok &= self.check_pattern(cx, &pattern.child(*item), seen, _env, what); }
             ok
         }
         SNode::Map(range) if {
             let items = pattern.range(*range);
             items.len() == 2 && matches!(pattern.arena().row(items[0]).node, SNode::Kw(k) if pattern.name(k) == "keys")
                 && matches!(pattern.arena().row(items[1]).node, SNode::Vec(ks) if !pattern.range(ks).is_empty())
         } => {
             let items = pattern.range(*range);
             let SNode::Vec(keys) = pattern.arena().row(items[1]).node else { unreachable!() };
             let mut ok = true;
             for id in pattern.range(keys) {
                 let key = pattern.child(*id);
                 match key.node {
                     SNode::Sym(name) => ok &= self.check_name(cx, &key, key.name(name), seen, what, zone),
                     _ => { ok = false; let m = shape(&key); self.error(&key, "E_PATTERN", &m); }
                 }
             }
             ok
         }
         _ => { let m = shape(pattern); self.error(pattern, "E_PATTERN", &m); false }
     }
 }
 fn pattern(p: &Syntax) -> Pattern {
     match p.node {
         SNode::Sym(n) => Pattern::Name(p.name(n).to_owned()),
         SNode::Vec(xs) => Pattern::Seq(p.range(xs).iter().map(|id| Self::pattern(&p.child(*id))).collect()),
         SNode::Map(xs) if p.range(xs).len() == 2 => {
             if let SNode::Vec(ks) = p.arena().row(p.range(xs)[1]).node {
                 Pattern::Keys(p.range(ks).iter().filter_map(|id| match p.arena().row(*id).node {
                     SNode::Sym(n) => Some(p.name(n).to_owned()), _ => None,
                 }).collect())
             } else { Pattern::Name("_".into()) }
         }
         _ => Pattern::Name("_".into()),
     }
 }

 fn binding(&mut self, cx: &Cx, x: &Syntax, id: CheckPath) -> (TermId, Info) {
     let inner = cx.with_path(id);
     let head = match x.node {
         SNode::List(range) if x.meta().is_empty() => {
             let items = x.range(range);
             match items.first().map(|id| x.arena().row(*id).node) {
                 Some(SNode::Sym(h)) => Some((x.name(h), &items[1..])), _ => None,
             }
         }, _ => None,
     };
     let (mut term, mut value) = match head {
         Some((h @ ("for" | "fold" | "scan" | "sum"), args)) => self.zone(&inner, x, h, args, Some(id)),
         Some(("let" | "let*", args)) => self.let_scope(&inner, x, args, id),
         Some(("state", args)) => self.state(&inner, x, args, Some(id)),
         Some(("fn", args)) => self.make_fn(&inner, x, args, "fn", Some(id)),
         _ => self.infer(&inner, x),
     };
     if value.approx && value.approx_sources.is_empty() { value.approx_sources.insert(id); }
     self.mark(id, &value);
     term = self.set_path(term, id);
     (term, value)
 }
 fn result_in(&mut self, cx: &Cx, form: &Syntax, base: &CheckPath) -> (TermId, Info) {
     let path = self.child_path(*base, "@result".into());
     let (term, value) = self.binding(cx, form, path);
     self.no_fn(form, &value.ty, &format!("The result of {}", cx.scope));
     (term, value)
 }
 fn body(&mut self, cx: &Cx, form: &Syntax) -> (TermId, Info) {
     if let SNode::List(items) = &form.node {
         let items = form.range(*items);
         if items.first().is_some_and(|id| matches!(form.arena().row(*id).node, SNode::Sym(name) if matches!(form.name(name), "let" | "let*")))
             && form.meta().is_empty()
         {
             return self.let_scope(cx, form, &items[1..], cx.path);
         }
     }
     let path = self.child_path(cx.path, "@result".into());
     let (term, value) = self.binding(cx, form, path);
     self.no_fn(form, &value.ty, &format!("The result of {}", cx.scope));
     (term, value)
 }

 fn branch_test(&mut self, cx: &Cx, key: &str, form: &Syntax) -> (TermId, Info) {
     let mut path = self.path_names(cx.path).to_vec();
     if let Some(last) = path.pop() {
         path.push(format!("{last}#{key}").into());
     } else {
         path.push(format!("@result#{key}").into());
     }
     {let path=self.path(path);self.binding(cx, form, path)}
 }

 fn branch(&mut self, cx: &Cx, key: &str, form: &Syntax) -> (TermId, Info) {
     let mut path = self.path_names(cx.path).to_vec();
     if let Some(last) = path.pop() {
         path.push(format!("{last}#{key}").into());
     } else {
         path.push(format!("@result#{key}").into());
     }
     let path = self.path(path);
     let inner = cx.with_path(path);
     let (mut term, value) = self.body(&inner, form);
     if syntax::head(form) .is_none_or(|head| !matches!(head, "let" | "let*")) {
         term = self.make_term(form, value.ty.clone(), Draft::Let(Vec::new(), term));
     }
     term = self.set_path(term, path);
     self.mark(path, &value);
     (term, value)
 }
 fn call(&mut self,cx:&Cx,f:&Syntax,name:&str,args:&[SyntaxId])->(TermId,Info){match name{
   "let"|"let*"=>{let base=self.inline_path(cx,"~let");self.let_scope(cx,f,args,base)},"state"=>self.state(cx,f,args,None),"for"|"fold"|"scan"|"sum"=>self.zone(cx,f,name,args,None),"fn"=>self.make_fn(cx,f,args,"fn",None),
   "if"=>self.if_form(cx,f,args),"cond"=>self.cond_form(cx,f,name,args),"case"=>self.cond_form(cx,f,name,args),"values"=>self.record(cx,f,args,"values"),"list"=>self.list(cx,f,args),"concat"=>self.concat(cx,f,args),"str"=>self.str_form(cx,f,args),"get"=>self.get(cx,f,args),"assoc"=>self.assoc(cx,f,args),"map"|"filter"|"reduce"|"sort_by"=>self.hof(cx,f,name,args),"iterate"=>self.iterate(cx,f,args),"ref"=>self.ref_form(cx,f,args),"with_error"=>self.with_error(cx,f,args),"error_rows"=>self.error_rows(cx,f,args),
   "quote"|"quasiquote"|"unquote"|"unquote-splicing"=>self.fail_term(f,"E_QUOTE",&format!("{name} belongs in a defmacro template (`…).")),"workspace"|"graph"|"defn"|"defnode"|"defmacro"=>self.fail_term(f,"E_FORM",&format!("{name} is a top-level form of a workspace.")),
   _=>self.call_named(cx,f,name,args)
 }}
 fn let_scope(&mut self,cx:&Cx,f:&Syntax,args:&[SyntaxId],base:CheckPath)->(TermId,Info){
     if args.len()!=2{return self.fail_term(f,"E_LET","let* needs [name expression …] and one result.")}
     let bindings=f.child(args[0]);
     let SNode::Vec(bs)=bindings.node else{return self.fail_term(f,"E_LET","let* needs [name expression …] and one result.")};
     let bs=bindings.range(bs);
     if bs.len()%2!=0{return self.fail_term(f,"E_LET","let* needs [name expression …] and one result.")}
     let mut env = cx.env.push();
     let mut out=Vec::new();let mut seen=BTreeSet::new();
     for pair in bs.chunks_exact(2){
         let p=&f.child(pair[0]);let expr=&f.child(pair[1]);
         if !self.check_pattern(cx,p,&mut seen,&env,"let*"){continue}
         let path=self.child_path(base,Self::pattern_key(p).into());
         let mut inner=cx.with_path(path);inner.env=env.snapshot();inner.zbody=false;
         let(t,v)=if expr.meta().is_empty()&&matches!(p.node,SNode::Sym(_))&&syntax::head(expr)==Some("fn"){
             let args=expr.children();
             self.make_fn(&inner,expr,&args[1..],Self::pattern_key(p).as_str(),Some(path))
         }else{self.binding(&inner,expr,path)};
         let t=self.set_path(t,path);self.mark(path,&v);
         if cx.zbody&&!cx.zone.is_some_and(|z|v.vary.contains(&z)){self.invariant.insert(path);}
         {let bcx=cx.with_path(base);self.bind(&bcx,p,v.clone(),"",env.top_mut());}
         out.push((Self::pattern(p),t));
     }
     let mut inner=cx.clone();inner.env=env.snapshot();inner.zbody=false;
     let(t,v)=self.result_in(&inner,&f.child(args[1]),&base);
     (self.make_term(f,v.ty.clone(),Draft::Let(out,t)),v)
 }

 fn pattern_key(p:&Syntax)->String{if let SNode::Sym(s)=p.node{p.name(s).to_owned()}else{lisp::flat(p)}}

 fn list(&mut self,cx:&Cx,f:&Syntax,args:&[SyntaxId])->(TermId,Info){let mut ts=Vec::new();let mut vs=Vec::new();let mut elem=Ty::Any;for id in args{let a=&f.child(*id);let(t,v)=self.infer(cx,a);self.no_fn(a,&v.ty,"A list element");elem=ty::join(&elem,&v.ty).unwrap_or_else(||{self.error(a,"E_TYPE",&format!("List elements share one type; got {} and {}.",show(&elem),show(&v.ty)));elem.clone()});ts.push(t);vs.push(v)}let mut v=Info::derive(Ty::List(Box::new(elem.clone())),&vs);v.len=Some(args.len());(self.make_term(f,Ty::List(Box::new(elem)),Draft::ListLit(ts)),v)}
 fn concat(&mut self,cx:&Cx,f:&Syntax,args:&[SyntaxId])->(TermId,Info){let mut ts=Vec::new();let mut vs=Vec::new();let mut el=Ty::Any;let mut len=Some(0usize);for id in args{let a=&f.child(*id);let(t,v)=self.infer(cx,a);if let Some(e)=ty::elem(&v.ty){el=ty::join(&el,&e).unwrap_or_else(||{self.error(a,"E_TYPE","concat joins lists of one type.");el.clone()});}else{self.error(a,"E_TYPE",&format!("concat joins lists; got {}.",show(&v.ty)));}len=match(len,v.len){(Some(x),Some(y))=>x.checked_add(y),_=>None};ts.push(t);vs.push(v)}let ty=Ty::List(Box::new(el));let mut v=Info::derive(ty.clone(),&vs);v.len=len;(self.make_term(f,ty,Draft::ListOp("concat".into(),ts)),v)}
 fn str_form(&mut self,cx:&Cx,f:&Syntax,args:&[SyntaxId])->(TermId,Info){let mut ts=Vec::new();let mut vs=Vec::new();for id in args{let a=&f.child(*id);let(t,v)=self.infer(cx,a);ts.push(t);vs.push(v)}(self.make_term(f,Ty::Text,Draft::Str(ts)),Info::derive(Ty::Text,&vs))}
 fn get(&mut self,cx:&Cx,f:&Syntax,args:&[SyntaxId])->(TermId,Info){
     if args.len()!=2{return self.fail_term(f,"E_ARGS","get is (get record :field).")};
     let record=f.child(args[0]);let key=f.child(args[1]);let(t,v)=self.infer(cx,&record);
     let SNode::Kw(field)=key.node else{return self.fail_term(f,"E_ARGS","get is (get record :field).")};
     let field=key.name(field);
     if !matches!(v.ty,Ty::Record(_)|Ty::Any){self.error(&record,"E_TYPE",&format!("get reads a record field; got {}.",show(&v.ty)));}
     let out=self.field(f,&v,field,&Self::pattern_key(&record));
     (self.make_term(f,out.ty.clone(),Draft::Get(t,field.to_owned())),out)
 }

 fn assoc(&mut self, cx: &Cx, f: &Syntax, args: &[SyntaxId]) -> (TermId, Info) {
     if args.len() < 3 || args.len() % 2 == 0 { return self.fail_term(f, "E_ARGS", "assoc is (assoc record :field value …)."); }
     let (rt, rv) = self.infer(cx, &f.child(args[0]));
     match rv.ty.clone() {
         Ty::Any => {
             let mut poison = Info::leaf(Ty::Any);
             poison.live = rv.live;
             (self.make_term(f, Ty::Any, Draft::Assoc(rt, Vec::new())), poison)
         }
         Ty::Record(mut fs) => {
             let mut lengths = rv.list_fields.clone();
             let mut vs = vec![rv];
             let mut updates: Vec<(Symbol, TermId)> = Vec::new();
             for pair in args[1..].chunks_exact(2) {
                 let (k, value) = (&f.child(pair[0]), &f.child(pair[1]));
                 match &k.node {
                     SNode::Kw(n) => {
                         let n = k.name(*n);
                         let (t, v) = self.infer(cx, value);
                         self.no_fn(value, &v.ty, &format!("Record field :{n}"));
                         match fs.iter().find(|(name, _)| name == n) {
                             Some((_, want)) => {
                                 if !ty::fits(&v.ty, want) {
                                     let m = format!("assoc :{n} is {}; got {}.", show(want), show(&v.ty));
                                     self.error(value, "E_TYPE", &m);
                                 }
                             }
                             None => fs.push((n.into(), v.ty.clone())),
                         }
                         lengths.retain(|p| p.first().map(|name| name.as_str()) != Some(n));
                         let fields = if v.live_len { let mut x = vec![Vec::new()]; x.extend(v.list_fields.iter().cloned()); x } else { v.list_fields.clone() };
                         lengths.extend(fields.into_iter().map(|p| { let mut q = vec![symbol::intern(n)]; q.extend(p); q }));
                         updates.push((symbol::intern(n), t));
                         vs.push(v);
                     }
                     _ => self.error(k, "E_ARGS", &format!("assoc keys are keywords; got {}.", Self::pattern_key(k))),
                 }
             }
             let ty = Ty::Record(fs);
             let mut info = Info::derive(ty.clone(), &vs);
             info.list_fields = lengths;
             (self.make_term(f, ty, Draft::Assoc(rt, updates)), info)
         }
         t => self.fail_term(f, "E_TYPE", &format!("assoc updates a record; got {}.", show(&t))),
     }
 }
 fn if_form(&mut self, cx: &Cx, form: &Syntax, args: &[SyntaxId]) -> (TermId, Info) {
     if args.len() != 3 {
         return self.fail_term(form, "E_NO_ELSE", "if takes a condition, a then and an else.");
     }
     let (condition, condition_value) = self.branch_test(cx, "test", &form.child(args[0]));
     if !ty::fits(&condition_value.ty, &Ty::Bool) {
         self.error(&form.child(args[0]), "E_TYPE", &format!("if needs a bool condition, got {}.", show(&condition_value.ty)));
     }
     let (then_term, then_value) = self.branch(cx, "then", &form.child(args[1]));
     let (else_term, else_value) = self.branch(cx, "else", &form.child(args[2]));
     if !ty::fits(&then_value.ty, &else_value.ty) && !ty::fits(&else_value.ty, &then_value.ty) {
         self.error(form, "E_TYPE", &format!("Both branches of if must have one type: {} and {}.", show(&then_value.ty), show(&else_value.ty)));
     }
     self.no_fn(&form.child(args[1]), &then_value.ty, "An if branch");
     self.no_fn(&form.child(args[2]), &else_value.ty, "An if branch");
     let ty = ty::join(&then_value.ty, &else_value.ty).unwrap_or_else(|| then_value.ty.clone());
     if condition_value.live && shape_ty(&ty) {
         self.error(form, "E_TIME_BRANCH", "A branch picks between shapes by t. The network keeps its shape while playing; pick a value instead.");
     }
     let mut value = Info::derive(ty.clone(), &[condition_value.clone(), then_value.clone(), else_value.clone()]);
     let mut lf = then_value.list_fields.clone();
     lf.extend(else_value.list_fields.iter().cloned());
     if condition_value.live {
         lf.extend(list_fields(&ty));
     }
     lf.sort();
     lf.dedup();
     value.approx = condition_value.approx || then_value.approx || else_value.approx;
     value.live_len = then_value.live_len || else_value.live_len || lf.iter().any(Vec::is_empty);
     value.list_fields = lf;
     (self.make_term(form, ty, Draft::If(condition, then_term, else_term)), value)
 }

 fn cond_form(&mut self, cx: &Cx, form: &Syntax, name: &str, args: &[SyntaxId]) -> (TermId, Info) {
     let is_case = name == "case";
     let (scrutinee, arms_form) = if is_case {
         let Some((scrutinee, rest)) = args.split_first() else {
             return self.fail_term(form, "E_COND", "case is (case value literal result … :else result).");
         };
         (Some(scrutinee), rest)
     } else {
         (None, args)
     };
     if arms_form.is_empty() || arms_form.len() % 2 != 0 {
         return self.fail_term(form, "E_COND", if is_case {
             "case is (case value literal result … :else result)."
         } else {
             "cond is (cond test value … :else value)."
         });
     }
     let pairs = arms_form.chunks_exact(2).collect::<Vec<_>>();
     if !matches!(form.arena().row(pairs[pairs.len() - 1][0]).node, SNode::Kw(key) if form.name(key) == "else") {
         return self.fail_term(form, "E_NO_ELSE", &format!("{name} needs a final :else arm, so it always has a value."));
     }
     let scrutinee = scrutinee.map(|id| self.branch_test(cx, "of", &form.child(*id)));
     let mut test_live = scrutinee.as_ref().is_some_and(|(_, value)| value.live);
     let mut values = scrutinee.iter().map(|(_, value)| value.clone()).collect::<Vec<_>>();
     let mut conditional_arms = Vec::new();
     let mut case_arms = Vec::new();
     let mut default = None;
     let mut result_ty: Option<Ty> = None;
     for (index, pair) in pairs.iter().enumerate() {
         let test = &form.child(pair[0]);
         let result_form = &form.child(pair[1]);
         let is_else = matches!(test.node, SNode::Kw(key) if test.name(key) == "else");
         if is_else && index + 1 != pairs.len() {
             self.error(test, "E_COND", &format!(":else is the last arm of {name}."));
         }
         let test_term = if is_else {
             None
         } else if let Some((_, scrutinee_value)) = &scrutinee {
             let literal = matches!(&test.node, SNode::Num(_) | SNode::Str(_))
                 || matches!(test.node, SNode::Sym(name) if test.name(name) == "true" || test.name(name) == "false");
             if literal {
                 let (term, value) = self.infer(cx, test);
                 if !ty::fits(&value.ty, &scrutinee_value.ty) && !ty::fits(&scrutinee_value.ty, &value.ty) {
                     self.error(test, "E_TYPE", &format!("case compares {} with the {} literal {}.", show(&scrutinee_value.ty), show(&value.ty), lisp::flat(test)));
                 }
                 Some(term)
             } else {
                 self.error(test, "E_CASE", &format!("case matches literal numbers, text or booleans; got {}.", lisp::flat(test)));
                 None
             }
         } else {
             let (term, value) = self.branch_test(cx, &format!("test{index}"), test);
             if !ty::fits(&value.ty, &Ty::Bool) {
                 self.error(test, "E_TYPE", &format!("cond tests are bool; got {}.", show(&value.ty)));
             }
             test_live |= value.live;
             values.push(value);
             Some(term)
         };
         let key = if is_else { "else".to_owned() } else if index == 0 { "then".to_owned() } else { format!("then~{}", index + 1) };
         let (result, value) = self.branch(cx, &key, result_form);
         self.no_fn(result_form, &value.ty, &format!("A {name} arm"));
         result_ty = Some(match result_ty {
             None => value.ty.clone(),
             Some(previous) => match ty::unify(&previous, &value.ty) {
                 Some(ty) => ty,
                 None => {
                     self.error(result_form, "E_TYPE", &format!("All arms of {name} must have one type: {} and {}.", show(&previous), show(&value.ty)));
                     previous
                 }
             },
         });
         values.push(value);
         if is_else {
             default = Some(result);
         } else if let Some(test_term) = test_term {
             if is_case {
                 case_arms.push((self.terms.import_form(test), result));
             } else {
                 conditional_arms.push((test_term, result));
             }
         }
     }
     let ty = result_ty.unwrap_or(Ty::Any);
     let default = default.unwrap_or_else(|| self.make_term(form, Ty::Any, Draft::Nil));
     if test_live && shape_ty(&ty) {
         self.error(form, "E_TIME_BRANCH", &format!("A branch in {} picks between shapes by t. The network keeps its shape while playing; pick a value instead.", path_text(self.path_names(cx.path))));
     }
     let mut value = Info::derive(ty.clone(), &values);
     let mut lf = values.iter().flat_map(|value| value.list_fields.iter().cloned()).collect::<Vec<_>>();
     if test_live {
         lf.extend(list_fields(&ty));
     }
     lf.sort();
     lf.dedup();
     value.approx = values.iter().any(|value| value.approx);
     value.live_len = values.iter().any(|value| value.live_len) || lf.iter().any(Vec::is_empty);
     value.list_fields = lf;
     let node = if let Some((scrutinee, _)) = scrutinee {
         Draft::Case(scrutinee, case_arms, default)
     } else {
         Draft::Cond(conditional_arms, default)
     };
     (self.make_term(form, ty, node), value)
 }
 fn state(&mut self, cx: &Cx, form: &Syntax, args: &[SyntaxId], at: Option<CheckPath>) -> (TermId, Info) {
     if args.len() != 2 {
         return self.fail_term(form, "E_STATE", "state is (state [s init] step).");
     }
     let binding_form = form.child(args[0]);
     let SNode::Vec(binding) = binding_form.node else {
         return self.fail_term(form, "E_STATE", "state is (state [s init] step).");
     };
     let binding = binding_form.range(binding);
     if binding.len() != 2 {
         return self.fail_term(form, "E_STATE", "state is (state [s init] step).");
     }
     let pattern = &form.child(binding[0]);
     let mut seen = BTreeSet::new();
     if !matches!(&pattern.node, SNode::Sym(_))
         || !self.check_pattern(cx, pattern, &mut seen, &cx.env, "state")
     {
         return self.fail_term(form, "E_PATTERN", "state binds one name.");
     }
     let (init, mut init_value) = self.infer(cx, &form.child(binding[1]));
     self.exact_sink(cx, &form.child(binding[1]), "state init", &init_value);
     self.no_fn(&form.child(binding[1]), &init_value.ty, "A state seed");
     if init_value.live {
         self.error(&form.child(binding[1]), "E_STATE_INIT", "A state seed is static; read the frame in the step.");
     }
     if shape_ty(&init_value.ty) {
         self.error(&form.child(binding[1]), "E_STATE_TYPE", "State stores data, not deferred nodes or layouts.");
     }
     let zone = match at { Some(p) => p, None => self.inline_path(cx, "~state") };
     let seed_list_fields = list_fields(&init_value.ty);
     init_value.live = true;
     init_value.state_dep = true;
     init_value.list_fields = seed_list_fields.clone();
     init_value.live_len = seed_list_fields.iter().any(Vec::is_empty);
     let mut env = cx.env.push();
     self.bind(cx, pattern, init_value.clone(), ":", env.top_mut());
     let mut inner = cx.with_path(zone);
     inner.env = env.snapshot();
     let (step, step_value) = self.body(&inner, &form.child(args[1]));
     self.no_fn(form, &step_value.ty, "A state step");
     if !ty::fits(&step_value.ty, &init_value.ty) || shape_ty(&step_value.ty) {
         self.error(form, "E_STATE_TYPE", "A state step returns the seed's data type.");
     }
     let ty = ty::unify(&init_value.ty, &step_value.ty).unwrap_or_else(|| init_value.ty.clone());
     let mut value = Info::derive(ty.clone(), &[init_value, step_value.clone()]);
     value.live = true;
     value.state_dep = true;
     value.approx = false;
     value.approx_sources.clear();
     value.packed.clear();
     value.list_fields.extend(seed_list_fields);
     value.list_fields.sort();
     value.list_fields.dedup();
     value.live_len |= value.list_fields.iter().any(Vec::is_empty);
     (
         self.make_term(form, ty, Draft::State {
             binder: Self::pattern(pattern),
             init: init,
             step: step,
             zone: self.path_names(zone).to_vec(),
         }),
         value,
     )
 }
 fn zone(&mut self, cx: &Cx, form: &Syntax, name: &str, args: &[SyntaxId], at: Option<CheckPath>) -> (TermId, Info) {
     let kind = match name { "for" => LoopKind::For, "fold" => LoopKind::Fold, "scan" => LoopKind::Scan, _ => LoopKind::Sum };
     let is_acc = matches!(kind, LoopKind::Fold | LoopKind::Scan);
     let vector = |id: SyntaxId| match form.arena().row(id).node {
         SNode::Vec(range) => Some(form.range(range)), _ => None,
     };
     let shape: Option<(&[SyntaxId], &[SyntaxId], SyntaxId, Vec<Vec<usize>>)> = match args {
         [a, i, b] if is_acc => match (vector(*a), vector(*i)) {
             (Some(a), Some(i)) if a.len() % 2 == 0 && i.len() % 2 == 0 => Some((a, i, *b, Vec::new())), _ => None,
         },
         [i, b] if !is_acc => vector(*i).filter(|i| i.len() % 2 == 0).map(|i| (&[][..], i, *b, Vec::new())),
         [i, k, sk, b] if name == "for" && matches!(form.arena().row(*k).node, SNode::Kw(k) if form.name(k) == "skip") => {
             vector(*i).filter(|i| i.len() % 2 == 0).map(|i| {
                 let sk = form.child(*sk);
                 let skip = match skip_tuples(&sk) {
                     Some(t) => t,
                     None => {
                         self.error(&sk, "E_SKIP", ":skip is a list of iteration tuples of non-negative integers, for example :skip [[0 1] [2 0]].");
                         Vec::new()
                     }
                 };
                 (&[][..], i, *b, skip)
             })
         }
         _ => None,
     };
     let Some((accs, iters, body_form, skip)) = shape else {
         let msg = if is_acc { format!("{name} is ({name} [acc init] [i collection] body).") } else { format!("{name} is ({name} [i collection …] body).") };
         return self.fail_term(form, "E_ZONE", &msg);
     };
     let body_form = form.child(body_form);
     let mut seen = BTreeSet::new();
     let mut ok = true;
     for pair in accs.chunks_exact(2).chain(iters.chunks_exact(2)) {
         ok = self.check_pattern(cx, &form.child(pair[0]), &mut seen, &cx.env, name) && ok;
     }
     if ok && iters.is_empty() {
         self.error(form, "E_ZONE", &format!("{name} needs at least one [name collection] clause."));
         ok = false;
     }
     if ok && is_acc && accs.len() != 2 {
         self.error(form, "E_ZONE", &format!("{name} carries exactly one accumulator: [acc init]. Carry several values in a record: [{{:keys [a b]}} {{:a 0 :b 1}}]."));
         ok = false;
     }
     if !ok { return self.fail_term_no_diag(form); }

     let id = match at { Some(p) => p, None => self.inline_path(cx, &format!("~{name}")) };
     self.zone_id += 1;
     let zid = self.zone_id;
     let init: Option<(Syntax, TermId, Info)> = if is_acc {
         let (t, v) = self.infer(cx, &form.child(accs[1]));
         self.no_fn(&form.child(accs[1]), &v.ty, &format!("The {name} accumulator"));
         Some((form.child(accs[0]), t, v))
     } else { None };
     let last = self.path_names(id).last().copied().unwrap_or_else(|| symbol::intern(name));
     let zcx = cx.with_path(id);
     let acc_live0 = init.as_ref().is_some_and(|(_, _, v)| v.live);
     let mut run_live = acc_live0;
     let (clauses, lens, outer, bt, bv) = loop {
         let mut env = cx.env.push();
         if let Some((p, _, iv)) = &init {
             let mut seed = iv.clone();
             seed.live = run_live;
             seed.vary.insert(zid);
             seed.live_len = false;
             self.bind(&zcx, p, seed, ":", env.top_mut());
         }
         let mut clauses = Vec::new();
         let mut lens = Vec::new();
         let mut outer = Vec::new();
         for pair in iters.chunks_exact(2) {
             let (p, e) = (&form.child(pair[0]), &form.child(pair[1]));
             let mut ecx = cx.clone();
             ecx.env = env.snapshot();
             let (ct, cv) = self.infer(&ecx, e);
             let et = match ty::elem(&cv.ty) {
                 Some(t) => t,
                 None => {
                     if cv.ty != Ty::Any {
                         self.error(e, "E_TYPE", &format!("{} in {name} iterates a list (range, linspace, point_list …); got {}.", Self::pattern_key(p), show(&cv.ty)));
                     }
                     Ty::Any
                 }
             };
             let seed_shape = init.as_ref().is_some_and(|(_, _, i)| !shape_ty(&i.ty));
             if cv.live_len && kind != LoopKind::Sum && !(is_acc && seed_shape) && !matches!(cv.ty, Ty::Array(_)) {
                 self.error(e, "E_TIME_COUNT", &format!("The number of iterations of {last} depends on t. Loop counts are fixed while playing; animate parameters instead, for example scale a piece to 0."));
             }
             let mut part = cv.clone();
             part.ty = et;
             part.live_len = false;
             part.len = None;
             part.callable = None;
             part.vary.insert(zid);
             self.bind(&zcx, p, part, ":", env.top_mut());
             clauses.push((Self::pattern(p), ct));
             lens.push(cv.len);
             outer.push(cv);
         }
         let mut inner = cx.with_path(id);
         inner.env = env.snapshot();
         inner.scope = format!("{} › {}", cx.scope, last);
         inner.zone = Some(zid);
         inner.zbody = true;
         let (bt, bv) = self.body(&inner, &body_form);
         if init.is_some() && bv.live && !acc_live0 && !run_live { run_live = true; continue; }
         break (clauses, lens, outer, bt, bv);
     };
     // ponytail: literal counts only; a driven count is bounded when Eval runs the zone.
     if !clauses.iter().any(|(_, t)| matches!(self.terms.ty(*t), Ty::Array(_))) && lens.iter().all(|l| l.is_some_and(|n| n <= MAX_ITERATIONS)) {
         let total = lens.iter().fold(1usize, |a, l| (MAX_ITERATIONS + 1).min(a * l.unwrap_or(0)));
         if total > MAX_ITERATIONS { self.error(form, "E_ITER_BOUND", &format!("{last} runs more than 4,096 iterations.")); }
     }
     let acc_ty = init.as_ref().map(|(_, _, iv)| iv.ty.clone());
     if let Some(at) = &acc_ty {
         if !ty::fits(self.terms.ty(bt), at) && !matches!((at, self.terms.ty(bt)), (Ty::Int, Ty::Float)) {
             self.error(form, "E_ACC_TYPE", &format!("{name} body must return the accumulator type {}; it returns {}.", show(at), show(self.terms.ty(bt))));
         }
     }
     let acc_ty = acc_ty.map(|at| ty::join(&at, self.terms.ty(bt)).unwrap_or(at));
     let packed = clauses.iter().any(|(_, t)| matches!(self.terms.ty(*t), Ty::Array(_)));
     let result_ty = match (kind, &acc_ty) { (LoopKind::Fold, Some(t)) => t.clone(), _ => self.terms.ty(bt).clone() };
     let cooked_fold = kind == LoopKind::Fold && cx.ctx == context::sop() && geometry_accumulator(&result_ty) && cx.stack.iter().any(|n| self.node_defs_get(n).is_some());
     if packed && shape_ty(&result_ty) && !cooked_fold {
         self.error(form, "E_ARRAY_TYPE", "Packed-array loops produce data; their length cannot create graph nodes.");
     }
     let array_elem = |this: &mut Self, ty: &Ty| match ty {
         Ty::Int | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text => ty.clone(),
         Ty::Float | Ty::Bool | Ty::Any => Ty::Float,
         _ => { this.error(form, "E_ARRAY_TYPE", "Arrays hold ints, floats, vectors or text values."); Ty::Float }
     };
     if kind == LoopKind::Sum && !matches!(self.terms.ty(bt), Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Any) {
         self.error(form, "E_TYPE", &format!("sum adds numbers or matching-width vectors; the body returns {}.", show(self.terms.ty(bt))));
     }
     let body_ty = self.terms.ty(bt).clone();
     let ty = match (kind, &acc_ty) {
         (LoopKind::Scan, Some(at)) => if packed { Ty::Array(Box::new(array_elem(self, at))) } else { Ty::List(Box::new(at.clone())) },
         (LoopKind::For | LoopKind::Scan, _) => if packed { Ty::Array(Box::new(array_elem(self, &body_ty))) } else { Ty::List(Box::new(self.terms.ty(bt).clone())) },
         (LoopKind::Sum, _) => match self.terms.ty(bt) { Ty::Int | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => self.terms.ty(bt).clone(), _ => Ty::Float },
         (LoopKind::Fold, Some(at)) => at.clone(),
         (LoopKind::Fold, None) => Ty::Any,
     };
     let mut ins = outer;
     if let Some((_, _, iv)) = &init { ins.push(iv.clone()); }
     let mut all = vec![bv.clone()];
     all.extend(ins.iter().cloned());
     let mut value = Info::derive(ty.clone(), &all);
     value.approx = kind == LoopKind::For && skip.is_empty() && packed_array(&ty) && clauses.len() == 1
         && clauses.iter().all(|(p, _)| matches!(p, Pattern::Name(_)))
         && clauses.iter().all(|(_, t)| packed_array(self.terms.ty(*t)))
         && packed_body(&self.ops, &self.terms, bt);
     value.vary = bv.vary.iter().filter(|v| **v != zid).copied().collect();
     for i in &ins { value.vary.extend(i.vary.iter().copied()); }
     let mut reasons: Vec<Diagnostic> = Vec::new();
     if kind != LoopKind::For || clauses.len() != 1 {
         reasons.push(err("E_GPU_FORM", "GPU kernels require collecting one-source loops.".into(), Some(form.span.clone())));
     } else {
         for (pattern, source) in &clauses {
             match pattern {
                 Pattern::Name(_) => match self.terms.ty(*source) {
                     Ty::Any => (),
                     Ty::Array(e) if **e == Ty::Any => (),
                     t if packed_array(t) => (),
                     t => reasons.push(err("E_PACKED_TYPE", format!("Packed source requires Array Float/Vec2/Vec3/Vec4, got {}.", show(t)), Some(self.term_form(*source).span.clone()))),
                 },
                 _ => reasons.push(err("E_PACKED_FORM", "Packed parameters require name patterns.".into(), Some(form.span.clone()))),
             }
         }
     }
     if reasons.is_empty() { self.add_root(form, id, None); } else { for r in reasons { self.add_root(form, id, Some(r)); } }
     self.link_body(bt, &id);
     value.packed = [id].into_iter().collect();
     let accs_ir = init.as_ref().map(|(p, t, _)| vec![(Self::pattern(p), *t)]).unwrap_or_default();
     (self.make_term(form, ty, Draft::Loop { kind, accs: accs_ir, clauses, skip, body: bt, zone: self.path_names(id).to_vec() }), value)
 }
 fn link_body(&mut self, term: TermId, producer: &CheckPath) {
     let node = self.terms.node(term);
     match node {
         PackedNode::Hof(_) | PackedNode::Loop(_) | PackedNode::Fn(_) => return,
         PackedNode::Op(id)
             if op::find_checked(&self.ops, self.terms.name(self.terms.op(id).op), &context::value())
                 .and_then(|(_, metadata)| metadata.packed_kind)
                 == Some(op::PackedKind::Exact) => return,
         _ => (),
     }
     if let Some(path) = self.terms.row(term).path {
         let path = self.terms.path_names(path).iter().map(|&name| symbol::intern(self.terms.name(name))).collect::<Path>();
         let path = self.path(path);
         let next = CheckPackedForm::Candidate(Arc::new([*producer].into_iter().collect()));
         self.packed_update(path, next);
     }
     if matches!(node, PackedNode::Vec(_) | PackedNode::Get(_) | PackedNode::Bypass(_) | PackedNode::Expanded(_)
         | PackedNode::Op(_) | PackedNode::Let(_) | PackedNode::WithError(_) | PackedNode::ErrorRows(_)
         | PackedNode::If(_) | PackedNode::Cond(_) | PackedNode::Case(_) | PackedNode::CallFn(_)) {
         for index in 0..self.terms.children(term).len() {
             let child = self.terms.children(term)[index];
             self.link_body(child, producer);
         }
     }
 }
 fn make_fn(&mut self, cx: &Cx, f: &Syntax, args: &[SyntaxId], name: &str, at: Option<CheckPath>) -> (TermId, Info) {
     if args.len() != 2 { return self.fail_term(f, "E_FN", "fn is (fn [params] body)."); }
     let SNode::Vec(ps) = f.arena().row(args[0]).node else { return self.fail_term(f, "E_FN", "fn is (fn [params] body)."); };
     let path = at.unwrap_or_else(|| self.inline_path(cx, "~fn"));
     let bcx = cx.with_path(path);
     let mut params = Vec::new();
     let mut env = cx.env.push();
     let mut seen = BTreeSet::new();
     for &id in f.range(ps) {
         let p = f.child(id);
         let (mut pat, mut annot) = (p.clone(), None);
         if let SNode::List(parts) = p.node {
             let parts = p.range(parts);
             if parts.len() == 3 && matches!(p.arena().row(parts[1]).node, SNode::Sym(s) if p.name(s) == ":") {
                 pat = p.child(parts[0]);
                 annot = ty::of_syntax(&p.child(parts[2]));
                 if annot.is_none() { self.error(&p, "E_PARAM", "Invalid fn parameter annotation."); }
             }
         }
         if !self.check_pattern(cx, &pat, &mut seen, &env, "fn") { continue; }
         params.push((pat.clone(), annot.clone()));
         self.bind(&bcx, &pat, Info::leaf(annot.unwrap_or(Ty::Any)), ":", env.top_mut());
     }
     let mut inner = cx.with_path(path);
     inner.env = env.snapshot();
     inner.scope = format!("{} › {name}", cx.scope);
     let body = f.child(args[1]);
     let (bt, bv) = self.body(&inner, &body);
     let callable = Rc::new(Callable::Local(Rc::new(Closure {
         params: params.clone(), body, env: cx.env.clone(), ctx: cx.ctx, path: path, name: name.into(),
     })));
     let ir_params = params.iter().map(|(p, t)| (Self::pattern(p), t.clone())).collect();
     let mut value = Info::leaf(Ty::Fn(None));
     value.live = bv.live;
     value.state_dep = bv.state_dep;
     value.callable = Some(callable);
     (self.make_term(f, Ty::Fn(None), Draft::Fn { params: ir_params, body: bt, zone: self.path_names(path).to_vec(), capture: None }), value)
 }
 fn fn_value(&mut self, cx: &Cx, form: &Syntax, what: &str) -> (TermId, Info) {
     if let SNode::Sym(name) = form.node {
         let name = form.name(name);
         if !cx.env.contains_key(name) && !name.contains('.') && !SPECIAL.contains(&name) {
             if self.defs_get(name).is_some() {
                 if self.call_sites {
                     self.check_def_body(name, true);
                 }
                 let mut value = Info::leaf(Ty::Fn(None));
                 value.callable = Some(Rc::new(Callable::Def(name.to_owned())));
                 return (self.make_term(form, Ty::Fn(None), Draft::FnRef(name.to_owned())), value);
             }
             let ops = self.ops.clone();
             if let Some(operator) = op::find_indexed(&ops, name, &cx.ctx) {
                 let mut value = Info::leaf(Ty::Fn(None));
                 value.callable = Some(Rc::new(Callable::Op(name.to_owned())));
                 return (self.make_term(form, Ty::Fn(None), Draft::FnRef(operator.name.clone())), value);
             }
             if let Ok(kind) = catalog::resolve_kind(&self.catalog, &cx.ctx, name) {
                 self.kind_fns.insert(name.to_owned(), (kind.qualified.clone(), kind.context, kind.slots.clone()));
                 let mut value = Info::leaf(Ty::Fn(None));
                 value.callable = Some(Rc::new(Callable::Kind(kind)));
                 return (self.make_term(form, Ty::Fn(None), Draft::FnRef(name.to_owned())), value);
             }
             if self.macros_contains(name) {
                 self.error(form, "E_MACRO_AS_VALUE", &format!("{name} is a macro; a macro is not a function value. Wrap it: (fn [a] ({name} a))."));
                 return self.fail_term_no_diag(form);
             }
         }
     }
     let (term, value) = self.infer(cx, form);
     if !ty::has_fn(&value.ty) {
         self.error(form, "E_TYPE", &format!("{what} expects a function; got {}.", show(&value.ty)));
     }
     (term, value)
 }
 fn call_closure(&mut self,cx:&Cx,f:&Syntax,c:&Closure,args:&[Arg],rc:Option<&Rc<Closure>>)->(TermId,Info){
     if args.len()!=c.params.len(){return self.fail_term(f,"E_ARITY",&format!("{} takes {} argument{}; got {}.",c.name,c.params.len(),if c.params.len()==1{""}else{"s"},args.len()))}
     self.depth_seen=Some(self.depth_seen.map_or(cx.depth,|d|d.max(cx.depth)));
     if cx.depth>64{self.events+=1;return self.fail_term(f,"E_DEPTH","Call depth exceeds 64.")}
     let mut env = c.env.push();
     let pcx=cx.with_path(c.path);
     for ((pattern,annotation),arg) in c.params.iter().zip(args){
         let mut value=arg.info.clone();
         if let Some(want)=annotation{
             if !ty::fits(&value.ty,want){self.error(&arg.form,"E_TYPE",&format!("{} {}: expected {}, got {}.",c.name,Self::pattern_key(pattern),show(want),show(&value.ty)))}
             value.ty=ty::coerce(&value.ty,want);
         }
         self.bind(&pcx,pattern,value,":",env.top_mut());
     }
     let mut inner=cx.clone();
     inner.env=env.snapshot();
     inner.ctx=c.ctx;
     inner.path=c.path;
     inner.scope=format!("{} › {}",cx.scope,c.name);
     inner.depth=cx.depth+1;
     inner.stack.extend(cx.stack.clone());inner.stack.sort();inner.stack.dedup();
     inner.zone=None;
     inner.zbody=false;
     match rc {
         // the body reads the closure, its inputs, the caller's scope text and node context
         Some(rc) => {
             let key = format!("{:p}|{}|{}", Rc::as_ptr(rc), inner.in_def, inner.scope);
             self.memo_body(key, Some(rc.clone()), args.iter().map(|a| a.info.clone()).collect(), &inner, &c.body)
         }
         None => self.body(&inner,&c.body),
     }
 }
 fn apply_callable(&mut self,cx:&Cx,f:&Syntax,value:&Info,args:&[Arg])->(TermId,Info){
     match value.callable.as_deref(){
         Some(Callable::Local(closure))=>self.call_closure(cx,f,closure,args,Some(closure)),
         Some(Callable::Def(name))=>self.apply_def(cx,f,name,args),
         Some(Callable::Op(name))=>{let ops=self.ops.clone();match op::find_checked(&ops,name,&cx.ctx){Some(operator)=>self.apply_op(cx,f,operator,args,Vec::new()),None=>self.fail_term(f,"E_UNKNOWN_KIND","Unknown operator.")}},
         Some(Callable::Kind(kind))=>self.apply_kind(cx,f,kind,args),
         None=>{
             let result=match &value.ty{
                 Ty::Fn(Some(signature))=>{
                     if signature.params.len()!=args.len(){self.error(f,"E_ARITY","Function argument count differs from its declared signature.")}
                     for (arg,want) in args.iter().zip(&signature.params){if !ty::fits(&arg.info.ty,want){self.error(&arg.form,"E_TYPE",&format!("Function input expected {}, got {}.",show(want),show(&arg.info.ty)))}}
                     signature.result.clone()
                 }
                 _=>Ty::Any,
             };
             (self.make_term(f,result.clone(),Draft::Nil),Info::derive(result,std::slice::from_ref(value)))
         }
     }
 }
 fn fn_argument(&mut self, cx: &Cx, form: &Syntax) -> (TermId, Info) {
     if let SNode::List(items) = form.node {
         let items = form.range(items);
         if items.first().is_some_and(|&head| matches!(form.arena().row(head).node, SNode::Sym(name) if form.name(name) == "list")) {
             let mut terms = Vec::new();
             let mut values = Vec::new();
             for &item in items.iter().skip(1) {
                 let (term, value) = self.fn_value(cx, &form.child(item), "function input");
                 terms.push(term);
                 values.push(value);
             }
             let ty = Ty::List(Box::new(Ty::Fn(None)));
             let mut value = Info::derive(ty.clone(), &values);
             value.len = Some(terms.len());
             return (self.make_term(form, ty, Draft::ListLit(terms)), value);
         }
     }
     self.fn_value(cx, form, "function input")
 }

 fn args_of(
     &mut self,
     cx: &Cx,
     source: &Syntax,
     forms: &[SyntaxId],
     normalize: Option<&str>,
     fn_slot: &dyn Fn(Option<usize>, Option<&str>) -> bool,
 ) -> Vec<Arg> {
     let normalized = normalize.and_then(|head| {
         let mut builder = syntax::SyntaxBuilder::new();
         let mut rows = BTreeMap::new();
         for (index, (id, text)) in syntax::attribute_args_view(head, source.arena(), forms).enumerate() {
             if let Some(text) = text {
                 let form = source.child(id);
                 let bytes = builder.bytes(text);
                 let row = builder.push(form.id, SNode::Str(bytes), form.span.clone(), form.trivia().cloned());
                 rows.insert(index, row);
             }
         }
         (!rows.is_empty()).then(|| (builder.freeze(), rows))
     });
     let get = |index: usize| normalized.as_ref()
         .and_then(|(arena, rows)| rows.get(&index).map(|&id| arena.handle(id)))
         .unwrap_or_else(|| source.child(forms[index]));
     let mut result = Vec::new();
     let mut at = 0;
     let mut positional = 0;
     while at < forms.len() {
         let form = get(at);
         if let SNode::Kw(key) = form.node {
             let key = form.name(key);
             if at + 1 == forms.len() {
                 self.error(&form, "E_MISSING_VALUE", &format!(":{key} has no value"));
                 break;
             }
             let value_form = get(at + 1);
             let (term, info) = if fn_slot(None, Some(key)) && syntax::head(&value_form) != Some("fn") {
                 self.fn_argument(cx, &value_form)
             } else {
                 self.input(cx, &value_form, &format!(":{key}"))
             };
             result.push(Arg { key: Some(key.to_owned()), form, term, info });
             at += 2;
         } else {
             let (term, info) = if fn_slot(Some(positional), None) && syntax::head(&form) != Some("fn") {
                 self.fn_argument(cx, &form)
             } else {
                 self.input(cx, &form, &positional.to_string())
             };
             result.push(Arg { key: None, form, term, info });
             positional += 1;
             at += 1;
         }
     }
     result
 }
 fn call_named(&mut self, cx: &Cx, form: &Syntax, name: &str, forms: &[SyntaxId]) -> (TermId, Info) {
     let local = cx.env.get(name).cloned();
     if let Some(local) = local.as_ref().filter(|local| ty::fits(&local.ty, &Ty::Fn(None))) {
         if forms.iter().any(|&id| matches!(form.arena().row(id).node, SNode::Kw(_))) {
             return self.fail_term(form, "E_ARGS", &format!("{name} is a local function; it takes positional arguments only."));
         }
         let args = self.args_of(cx, form, forms, None, &|_, _| false);
         let (result, rv) = self.apply_callable(cx, form, local, &args);
         let body = match (local.callable.as_deref(), self.terms.node(result)) {
             (Some(Callable::Local(_)), _) => Some(result),
             (Some(Callable::Def(_)), PackedNode::CallFn(id)) => self.terms.call_fn(id).body,
             _ => None,
         };
         return (self.make_term(form, rv.ty.clone(), Draft::CallFn { fn_name: name.to_owned(), args: args.iter().map(|a| a.term).collect(), body }), rv);
     }
     if self.macros_contains(name) {
         let definitions = self.base.iter().flat_map(|base| base.macro_forms.iter()).chain(self.macro_forms.iter()).cloned().collect::<Vec<_>>();
         return match macros::expand(&definitions, form) {
             Ok(expanded) => {
                 let (term, value) = self.infer(cx, &expanded);
                 (self.make_term(form, self.terms.ty(term).clone(), Draft::Expanded { macro_name: name.into(), body: term }), value)
             }
             Err(problem) => {
                 self.diags.push(problem);
                 self.fail_term_no_diag(form)
             }
         };
     }
     if let Some(definition) = self.defs_get(name).cloned() {
         let fn_slot = |position: Option<usize>, key: Option<&str>| {
             let parameter = if let Some(index) = position {
                 definition.params.get(index)
             } else {
                 key.and_then(|key| definition.params.iter().find(|(name, _, _)| name == key))
             };
             parameter.is_some_and(|(_, ty, _)| matches!(ty, Ty::Fn(_)))
         };
         let args = self.args_of(cx, form, forms, None, &fn_slot);
         return self.apply_sig(cx, form, &definition, &args);
     }
     let ops = self.ops.clone();
     if let Some((operator, metadata)) = op::find_checked(&ops, name, &cx.ctx) {
         if operator.name == "scene/merge" {
             let mut skip = Vec::new();
             let mut args_form = Vec::new();
             let mut at = 0;
             while at < forms.len() {
                 if matches!(form.arena().row(forms[at]).node, SNode::Kw(key) if form.name(key) == "skip") && at + 1 < forms.len() {
                     if let Some(tuples) = skip_tuples(&form.child(forms[at + 1])) {
                         skip.extend(tuples);
                     } else {
                         self.error(&form.child(forms[at + 1]), "E_SKIP", ":skip is a list of iteration tuples of non-negative integers, for example :skip [[0 1] [2 0]].");
                     }
                     at += 2;
                 } else {
                     args_form.push(forms[at]);
                     at += 1;
                 }
             }
             let args = self.args_of(cx, form, &args_form, None, &|_, _| false);
             return self.apply_op(cx, form, (operator, metadata), &args, skip);
         }
         let slots = operator.signature.pos.iter().chain(&operator.signature.opt).cloned().collect::<Vec<_>>();
         let keywords = operator.signature.kw.clone();
         let fn_slot = move |position: Option<usize>, key: Option<&str>| {
             let ty = if let Some(index) = position {
                 slots.get(index).map(|(_, ty)| ty)
             } else {
                 key.and_then(|key| keywords.iter().find(|(name, _)| name == key).map(|(_, ty)| ty))
             };
             ty.is_some_and(|ty| matches!(ty, Ty::Fn(_)))
         };
         let args = self.args_of(cx, form, forms, Some(name), &fn_slot);
         return self.apply_op(cx, form, (operator, metadata), &args, Vec::new());
     }
     match catalog::resolve_kind(&self.catalog, &cx.ctx, name) {
         Ok(kind) => {
             let fn_slot = |position: Option<usize>, key: Option<&str>| {
                 let slot = if let Some(index) = position {
                     kind.slots.get(index)
                 } else {
                     key.and_then(|key| kind.slots.iter().find(|slot| slot.name == key))
                 };
                 slot.and_then(|slot| slot.ty.as_ref()).is_some_and(|ty| matches!(ty, Ty::Fn(_)))
                     || key.is_some_and(|key| kind.parameters.iter().any(|parameter| {
                         parameter.name == key && matches!(parameter.ty, Some(port_type::PortType::Fn(_)))
                     }))
             };
             let args = self.args_of(cx, form, forms, None, &fn_slot);
             self.apply_kind(cx, form, &kind, &args)
         }
         Err((mut code, message)) => {
             let mut names = crate::registry::names(&self.catalog);
             names.extend(self.ops.iter().map(|operator| operator.name.clone()));
             let suggestions = catalog::suggestions_from(name, &names);
             if code == "E_NAMESPACE" && name.split_once('/').is_some_and(|(prefix, _)| self.ops.iter().any(|operator| operator.name.split_once('/').is_some_and(|(namespace, _)| namespace == prefix))) {
                 code = "E_UNKNOWN_KIND".into();
             }
             let index = self.diags.len();
             let result = if code == "E_UNKNOWN_KIND" {
                 let suffix = suggestions.first().map_or_else(String::new, |hint| format!(" Did you mean {hint}?"));
                 self.fail_term(form, &code, &format!("Unknown operator “{name}”.{suffix}"))
             } else {
                 self.fail_term(form, &code, &message)
             };
             if let Some(diagnostic) = self.diags.get_mut(index) {
                 diagnostic.set_signature(suggestions.first().and_then(|name| crate::registry::signature(name)
                     .or_else(|| self.ops.iter().find(|operator| &operator.name == name).map(crate::registry::op_signature))
                     .or_else(|| self.catalog.kinds.iter().find(|kind| &kind.qualified == name).map(crate::registry::kind_signature))));
                 diagnostic.set_suggest(suggestions);
             }
             result
         }
     }
 }
 fn apply_def(&mut self,cx:&Cx,f:&Syntax,name:&str,args:&[Arg])->(TermId,Info){let Some(d)=self.defs_get(name).cloned()else{return self.fail_term(f,"E_UNKNOWN_KIND","Unknown definition.")};self.apply_sig(cx,f,&d,args)}
 fn apply_sig(&mut self, cx: &Cx, f: &Syntax, d: &Sig, args: &[Arg]) -> (TermId, Info) {
     if d.ctx != cx.ctx && d.ctx != context::value() {
         return self.fail_term(f, "E_WRONG_CONTEXT", &format!("{} belongs to {}; it cannot run in {}.", d.name, context_name(&d.ctx), context_name(&cx.ctx)));
     }
     self.probes.push(d.name.clone());
     if cx.stack.contains(&d.name) {
         self.events += 1;
         let mut stack = cx.stack.clone();
         stack.push(d.name.clone());
         return self.fail_term(f, "E_RECURSION", &format!("Recursive call: {}. Use fold for repetition.", stack.join(" → ")));
     }
     let pos = args.iter().filter(|a| a.key.is_none()).collect::<Vec<_>>();
     if pos.len() > d.params.len() {
         self.error(f, "E_ARITY", &format!("{} takes {} inputs; got {} positional.", d.name, d.params.len(), pos.len()));
     }
     let mut env = BTreeMap::new();
     let mut terms = Vec::new();
     for (i, (n, want, default)) in d.params.iter().enumerate() {
         let named = args.iter().find(|a| a.key.as_deref() == Some(n));
         let bypos = pos.get(i).copied();
         if bypos.is_some() && named.is_some() {
             self.error(&named.unwrap().form, "E_DUPLICATE_PARAM", &format!("{} input {n} is given twice.", d.name));
         }
         let (term, value) = if let Some(a) = bypos.or(named) {
             self.need(a, &format!("{} :{n}", d.name), want);
             let mut value = a.info.clone();
             value.ty = ty::coerce(&value.ty, want);
             (a.term, value)
         } else if let Some(default) = default {
             let dcx = Cx {
                 env: ScopeStack::default(), ctx: d.ctx, path: self.path(vec![format!("def:{}", d.name).into(), format!(":{n}").into()]),
                 scope: format!("ƒ {}", d.name), in_def: true, stack: vec![d.name.clone()], depth: 0, zone: None, zbody: false,
             };
             let (term, value) = self.infer(&dcx, default);
             if !ty::fits(&value.ty, want) { self.error(default, "E_TYPE", "Definition default differs from its declared input type."); }
             (term, value)
         } else {
             self.error(f, "E_ARGS", &format!("{} needs :{n} ({}).", d.name, show(want)));
             (self.make_term(f, want.clone(), Draft::Nil), Info::leaf(want.clone()))
         };
         env.insert(n.clone(), value);
         terms.push(term);
     }
     for a in args {
         if let Some(key) = &a.key {
             if !d.params.iter().any(|(name, _, _)| name == key) { self.error(&a.form, "E_UNKNOWN_PARAM", &format!("{} has no input :{key}.", d.name)); }
         }
     }
     let key = d.params.iter().map(|(n, _, _)| env[n].clone()).collect();
     let mut inner = Cx {
         env: ScopeStack::from_map(env), ctx: d.ctx, path: self.path(vec![format!("def:{}", d.name).into()]), scope: format!("ƒ {}", d.name),
         in_def: true, stack: cx.stack.clone(), depth: cx.depth + 1, zone: None, zbody: false,
     };
     inner.stack.push(d.name.clone());
     let (body, value) = self.call_body(d, key, &inner);
     (self.make_term(f, value.ty.clone(), Draft::CallFn { fn_name: d.name.clone(), args: terms, body: Some(body) }), value)
 }
 /// `d`'s body under `inner`, whose environment holds `env`: a repeated check of a body that
 /// already checked identically in this builder copies its terms instead of checking it again.
 fn call_body(&mut self, d: &Sig, env: Vec<Info>, inner: &Cx) -> (TermId, Info) {
     self.memo_body(d.name.clone(), None, env, inner, &d.body)
 }
 /// `body` under `inner`, memoised as `key` with these inputs (`closure` for a local function).
 fn memo_body(&mut self, key: String, closure: Option<Rc<Closure>>, env: Vec<Info>, inner: &Cx, body: &Syntax) -> (TermId, Info) {
     let budget = self.budget();
     let steps = self.steps;
     fn find<'a, T>(memos: &'a CallMemos<T>, name: &str, env: &[Info], inner: &Cx, fits: impl Fn(usize) -> bool) -> Option<&'a CallMemo<T>> {
         memos.get(name)?.iter().find(|m| {
             m.env.len() == env.len() && m.env.iter().zip(env).all(|(a, b)| a.same(b))
                 && !m.probes.iter().any(|name| inner.stack.contains(name))
                 && m.reach.is_none_or(|reach| inner.depth + reach <= 64)
                 && fits(m.steps)
         })
     }
     let fits = |more: usize| steps + more <= budget;
     let hit = match find(&self.calls, &key, &env, inner, fits) {
         Some(m) => Some((self.terms.copy_tree(m.term), m.clone())),
         None if closure.is_some() => None,
         None => match find(&self.kept, &key, &env, inner, fits).or_else(|| find(&self.inherited, &key, &env, inner, fits)) {
             // a sealed body comes back as fresh rows; later hits in this builder copy those
             Some(m) => {
                 let term = self.terms.rewrite(&m.term, &mut |_, _, draft| draft);
                 let m = CallMemo { term, env: m.env.clone(), probes: m.probes.clone(), reach: m.reach, steps: m.steps, value: m.value.clone(), closure: None };
                 self.calls.entry(key.clone()).or_default().push(m.clone());
                 Some((term, m))
             }
             None => None,
         },
     };
     if let Some((term, m)) = hit {
         self.steps += m.steps;
         if let Some(reach) = m.reach { self.depth_seen = Some(self.depth_seen.map_or(inner.depth + reach, |d| d.max(inner.depth + reach))); }
         self.probes.extend(m.probes);
         return (term, m.value);
     }
     let events = self.events;
     let outer_seen = self.depth_seen.take();
     let outer_probes = std::mem::take(&mut self.probes);
     let (term, value) = self.body(inner, body);
     let seen = std::mem::replace(&mut self.depth_seen, outer_seen);
     if let Some(seen) = seen { self.depth_seen = Some(self.depth_seen.map_or(seen, |d| d.max(seen))); }
     let mut probes = std::mem::replace(&mut self.probes, outer_probes);
     probes.sort();
     probes.dedup();
     self.probes.extend(probes.iter().cloned());
     if self.events == events {
         let memo = CallMemo { env, probes, reach: seen.map(|seen| seen.saturating_sub(inner.depth)), steps: self.steps - steps, term, value: value.clone(), closure };
         self.calls.entry(key).or_default().push(memo);
     }
     (term, value)
 }
 /// Seal the current builder and return to `outer`, keeping its call-site bodies for later builders.
 fn finish_terms(&mut self, outer: TermBuilder, outer_calls: CallMemos<TermId>, roots: &[TermId], form: SyntaxId) -> (Arc<packed::TermArena>, Vec<TermId>, Vec<SyntaxId>) {
     let calls: Vec<(String, CallMemo<TermId>)> = std::mem::replace(&mut self.calls, outer_calls).into_iter()
         .flat_map(|(name, memos)| memos.into_iter().filter(|m| m.closure.is_none()).map(move |m| (name.clone(), m))).collect();
     let extra: Vec<TermId> = calls.iter().map(|(_, m)| m.term).collect();
     let (arena, roots, forms, kept) = std::mem::replace(&mut self.terms, outer).finish_with(roots, &[form], &extra);
     for ((name, m), term) in calls.into_iter().zip(kept) {
         let Some(term) = term else { continue };
         self.kept.entry(name).or_default().push(CallMemo { term, env: m.env, probes: m.probes, reach: m.reach, steps: m.steps, value: m.value, closure: None });
     }
     (arena, roots, forms)
 }
 fn apply_op(&mut self, cx: &Cx, form: &Syntax, (operator, metadata): (&op::Op, op::OpMetadata), arguments: &[Arg], skip: Vec<Vec<usize>>) -> (TermId, Info) {
     let diagnostic_start = self.diags.len();
     if BODY_CONTEXT_OPS.contains(&operator.name.as_str()) && !cx.in_def {
         self.error(form, "E_NODE_CONTEXT", "Context seed, frame and provenance base are available inside a defnode body.");
     }
     if operator.ctx != context::value() && operator.ctx != cx.ctx {
         return self.fail_term(form, "E_WRONG_CONTEXT", &format!("{} belongs to {}; it cannot run in {}. Pass data through a typed input or ref.", operator.name, context_name(&operator.ctx), context_name(&cx.ctx)));
     }
     let is_geometry_sink = operator.name.starts_with("sop/")
         || operator.name.starts_with("settings/")
         || operator.name.starts_with("scene/");
     if is_geometry_sink && operator.name != "scene/trace" {
         let values_slot = operator.signature.pos.len().checked_sub(1);
         let mut position = 0;
         for argument in arguments {
             let readback = operator.name == "sop/with_attr"
                 && (argument.key.as_deref() == Some("values")
                     || argument.key.is_none() && Some(position) == values_slot);
             if argument.key.is_none() {
                 position += 1;
             }
             if !readback {
                 self.exact_sink(cx, &argument.form, &operator.name, &argument.info);
             }
         }
     }

     let mut position = 0;
     let mut args = Vec::with_capacity(arguments.len());
     for argument in arguments {
         let want = if let Some(key) = &argument.key {
             operator.signature.kw.iter().find(|(name, _)| name == key).map(|(_, ty)| ty.clone())
         } else {
             let index = position;
             position += 1;
             operator.signature.pos.iter().chain(&operator.signature.opt).nth(index).map(|(_, ty)| ty.clone())
                 .or_else(|| operator.signature.rest.as_ref().map(|(_, ty)| ty.clone()))
         };
         args.push(match want {
             Some(Ty::Fn(Some(signature))) => Cow::Owned(self.instantiate_fn(cx, argument, &signature, true)),
             _ => Cow::Borrowed(argument),
         });
     }
     if operator.name == "ui/canvas" {
         for argument in &mut args {
             if argument.key.is_none() && argument.info.ty == ty::image() {
                 let argument = argument.to_mut();
                 let source_form = self.term_form(argument.term).clone();
                 argument.term = self.make_term(
                     &source_form,
                     ty::drawing(),
                     Draft::Op {
                         op: "draw/image".into(),
                         args: vec![("image".into(), argument.term)],
                         skip: Vec::new(),
                     },
                 );
                 argument.info.ty = ty::drawing();
             }
         }
     }

     let positional = args.iter().filter(|argument| argument.key.is_none()).collect::<Vec<_>>();
     let slots = operator.signature.pos.iter().chain(&operator.signature.opt).collect::<Vec<_>>();
     if positional.len() < operator.signature.pos.len()
         || operator.signature.rest.is_none() && positional.len() > slots.len()
     {
         self.error(form, "E_ARITY", &format!("{} takes {} positional inputs; got {}. Expected {}", operator.name, operator.signature.pos.len(), positional.len(), crate::registry::op_signature(operator)));
     }
     let mut named = Vec::new();
     let fixed = if operator.signature.rest.is_some() { operator.signature.pos.len() } else { slots.len() };
     for (index, argument) in positional.iter().enumerate() {
         if index < fixed {
             let (name, want) = slots[index];
             let compatible_number = operator.any_num && match argument.info.ty {
                 Ty::Int | Ty::Float | Ty::Vec3 => true,
                 Ty::Vec2 | Ty::Vec4 => operator.arithmetic.is_some(),
                 _ => false,
             };
             if !compatible_number {
                 self.need(argument, &format!("{} {}", operator.name, name), want);
             }
             named.push((*name, argument.term));
         } else if let Some((name, want)) = &operator.signature.rest {
             let element = ty::elem(&argument.info.ty).filter(|element| ty::fits(element, want));
             if let Some(_element) = element {
                 if argument.info.live_len && shape_ty(want) {
                     self.error(&argument.form, "E_TIME_COUNT", &format!("The list passed to {} changes length with t. The network keeps its shape while playing; animate parameters instead, for example scale a piece to 0.", operator.name));
                 }
             } else {
                 self.need(argument, &format!("{} {}", operator.name, name), want);
             }
             named.push((*name, argument.term));
         }
     }
     let mut seen = BTreeSet::new();
     for argument in args.iter().filter(|argument| argument.key.is_some()) {
         let key = argument.key.as_deref().unwrap_or("");
         match operator.signature.kw.iter().find(|(name, _)| name == key) {
             None => {
                 let names = operator.signature.kw.iter().map(|(name, _)| format!(":{name}")).collect::<Vec<_>>().join(" ");
                 self.error(&argument.form, "E_UNKNOWN_PARAM", &format!("{} has no parameter :{}{}. ", operator.name, key, if names.is_empty() { String::new() } else { format!(". Parameters: {names}.") }));
             }
             Some((_, want)) if !seen.insert(key.to_owned()) => {
                 self.error(&argument.form, "E_DUPLICATE_PARAM", &format!(":{key} is given twice"));
             }
             Some((_, want)) => {
                self.need(argument, &format!("{} :{key}", operator.name), want);
                 named.push((symbol::intern(key), argument.term));
             }
         }
     }
     if operator.any_num {
         let mut widths = BTreeSet::new();
         for argument in &positional {
             if matches!(argument.info.ty, Ty::Vec2 | Ty::Vec3 | Ty::Vec4) {
                 widths.insert(match argument.info.ty { Ty::Vec2 => 2, Ty::Vec3 => 3, _ => 4 });
             }
         }
         if widths.len() > 1 {
             let widths = widths.into_iter().map(|width| format!("vec{width}")).collect::<Vec<_>>().join(" and ");
             self.error(form, "E_TYPE", &format!("{} needs matching vector widths; got {widths}.", operator.name));
         }
     }
     let typed = named.iter().map(|(name, term)| {
         let have = self.terms.ty(*term);
         let want = operator.signature.pos.iter().chain(&operator.signature.opt).chain(&operator.signature.kw)
             .find(|(argument, _)| argument == name).or(operator.signature.rest.as_ref()).map(|(_, ty)| ty);
         let ty = if operator.any_num { have.clone() } else { want.map_or_else(|| have.clone(), |want| ty::coerce(have, want)) };
         ((*name).into(), ty, literal_param(&self.terms, *term))
     }).collect::<Vec<_>>();
     let result = match op::result_type(operator, &typed) {
         Ok(ty) => ty,
         Err(mut problem) => {
             problem.span = Some(form.span.clone());
             self.diags.push(problem);
             Ty::Any
         }
     };
     self.literal_checks(form, operator, &args);
     let values = args.iter().map(|argument| argument.info.clone()).collect::<Vec<_>>();
     let mut info = Info::derive(result.clone(), &values);
     info.live = operator.live || values.iter().any(|value| value.live);
     info.live_len = match operator.name.as_str() {
         "range" | "linspace" => values.iter().any(|value| value.live),
         "take" | "drop" => positional.first().is_some_and(|value| value.info.live)
             || positional.get(1).is_some_and(|value| value.info.live_len),
         "rest" | "reverse" => positional.first().is_some_and(|value| value.info.live_len),
         _ => false,
     };
     let literal_int = |index: usize| positional.get(index).and_then(|argument| match literal_param(&self.terms, argument.term) {
         Some(param::Value::Int(value)) => Some(value),
         _ => None,
     });
     info.len = match operator.name.as_str() {
         "range" if positional.len() == 1 => literal_int(0).and_then(|count| usize::try_from(count.max(0)).ok()),
         "range" if positional.len() == 2 => match (literal_int(0), literal_int(1)) {
             (Some(start), Some(finish)) => usize::try_from((finish as i128 - start as i128).max(0)).ok(),
             _ => None,
         },
         "linspace" if positional.len() == 3 => literal_int(2).and_then(|count| usize::try_from(count.max(0)).ok()),
         _ => None,
     };
     let capability = metadata.packed_kind;
     info.approx = match capability {
         Some(op::PackedKind::Binary(_) | op::PackedKind::Unary(_) | op::PackedKind::Length | op::PackedKind::Noise3 | op::PackedKind::Fma) => values.iter().any(|value| value.approx),
         _ => false,
     };
     if capability == Some(op::PackedKind::Exact) || shape_ty(&result) {
         info.packed.clear();
         info.approx_sources.clear();
     }
     if operator.live {
         info.list_fields = list_fields(&result);
     }
     if operator.name == "array/sum" {
         self.add_root(form, cx.path, Some(err("E_GPU_FORM", "Ordered reductions stay on the CPU.".into(), Some(form.span.clone()))));
         info.packed.insert(cx.path);
     } else if operator.name == "image/map" {
         self.add_root(form, cx.path, None);
         for argument in &args {
             if let PackedNode::Fn(id) = self.terms.node(argument.term) {
                 let function = *self.terms.function(id);
                 let zone = self.terms.path_names(function.zone).iter().map(|&name| symbol::intern(self.terms.name(name))).collect::<Path>();
                 let zone = self.path(zone);
                 self.link_body(function.body, &cx.path);
                 self.packed_update(zone, CheckPackedForm::Candidate(Arc::new([cx.path].into_iter().collect())));
             }
         }
         info.packed.insert(cx.path);
     }
     for diagnostic in &mut self.diags[diagnostic_start..] {
         diagnostic.set_signature(Some(crate::registry::op_signature(operator)));
     }
     (self.make_term(form, result, Draft::Op { op: operator.name.clone(), args: named, skip }), info)
 }

 fn literal_checks(&mut self, form: &Syntax, operator: &op::Op, args: &[Cow<'_, Arg>]) {
     let positional = args.iter().filter(|argument| argument.key.is_none()).collect::<Vec<_>>();
     let terms = &self.terms;
     let (graphs, defs, base) = (&self.graphs, &self.defs, self.base.as_deref());
     let mut error = |form: &Syntax, code: &str, message: &str| add_to(&mut self.diags, Severity::ErrorSeverity, form, code, message.to_owned());
     let literal = |argument: &Arg| {
         let mut term = argument.term;
         loop {
             match terms.node(term) {
                 PackedNode::Lit(id) => break Some(terms.literal(id)),
                 PackedNode::Expanded(id) => term = terms.expanded(id).body,
                 _ => break None,
             }
         }
     };
     let integer = |argument: &Arg| match literal(argument) {
         Some(packed::Literal::Int(value)) => Some(value),
         _ => None,
     };
     let number = |argument: &Arg| match literal(argument) {
         Some(packed::Literal::Int(value)) => Some(value as f64), Some(packed::Literal::Float(value)) => Some(value), _ => None,
     };
     let text = |argument: &Arg| match terms.node(argument.term) {
         PackedNode::Text(bytes) => std::str::from_utf8(terms.raw_bytes(bytes)).ok(),
         PackedNode::Lit(id) => match terms.literal(id) {
             packed::Literal::Text(bytes) => std::str::from_utf8(terms.raw_bytes(bytes)).ok(),
             packed::Literal::Choice(name) => Some(terms.name(name)),
             _ => None,
         },
         _ => None,
     };
     let mut position = 0;
     for argument in args {
         let slot = if let Some(key) = &argument.key {
             operator.signature.kw.iter().find(|(name, _)| name == key)
         } else {
             let slot = operator.signature.pos.iter().chain(&operator.signature.opt).nth(position).or(operator.signature.rest.as_ref());
             position += 1;
             slot
         };
         if let Some((name, want)) = slot {
             if let Some(value) = text(argument) {
                 if *want == Ty::Color && rays_math::color::hex(value).is_err() {
                     error(&argument.form, "E_TYPE", &format!("{} {name} expects a color (#rrggbb or #rrggbbaa); got {value:?}.", operator.name));
                 }
                 if let Some((_, choices)) = operator.choices.iter().find(|(key, _)| key == name) {
                     if !choices.iter().any(|choice| choice == value) {
                         error(&argument.form, "E_TYPE", &format!("{} {name} expects one of {}; got {value:?}.", operator.name, choices.join(", ")));
                     }
                 }
             }
         }
     }
     match operator.name.as_str() {
         "sop/context_seed" => match positional.as_slice() {
             [argument] if text(argument).is_some_and(|value| !value.trim().is_empty()) => (),
             [argument] => error(&argument.form, "E_SEED_NAMESPACE", "Context seed requires a literal nonblank namespace."),
             _ => (),
         },
         "image/map" => {
             for argument in &positional {
                 match &argument.info.ty {
                     Ty::Fn(Some(signature)) if signature.params == [Ty::Vec2] && signature.result == Ty::Vec4 => (),
                     Ty::Fn(None) | Ty::Any => (),
                     _ => error(&argument.form, "E_TYPE", "image/map needs a vec2-to-vec4 pixel function."),
                 }
             }
             for argument in args {
                 if matches!(argument.key.as_deref(), Some("width" | "height"))
                     && integer(argument).is_some_and(|value| value <= 0)
                 {
                     error(&argument.form, "E_RANGE", "Image dimensions must be positive.");
                 }
             }
         }
         "length" => {
             for argument in &positional {
                 if argument.info.ty != Ty::Vec3 && argument.info.ty != Ty::Any {
                     error(&argument.form, "E_TYPE", "length needs a vec3.");
                 }
             }
         }
         "material/standard" => for argument in args {
             match (argument.key.as_deref(), terms.node(argument.term)) {
                 (Some("color" | "emission"), PackedNode::Text(bytes)) if !hex_colour(terms.raw_bytes(bytes)) => error(&argument.form, "E_TYPE", "A material colour is #rrggbb or a vec3."),
                 (Some("roughness"), _) if number(argument).is_some_and(|value| !(0.0..=1.0).contains(&value)) => error(&argument.form, "E_RANGE", "Material roughness must be in [0,1]."),
                 _ => (),
             }
         },
         "range" => match positional.iter().map(|argument| integer(argument)).collect::<Vec<_>>().as_slice() {
             [Some(count)] if *count > MAX_ITERATIONS as i64 => error(form, "E_ITER_BOUND", &format!("range 0‥{count} exceeds 4,096 iterations.")),
             [Some(start), Some(finish)] if *finish as i128 - *start as i128 > MAX_ITERATIONS as i128 => error(form, "E_ITER_BOUND", &format!("range {start}‥{finish} exceeds 4,096 iterations.")),
             _ => (),
         },
         "linspace" if positional.len() == 3 && integer(positional[2]).is_some_and(|count| count > MAX_ITERATIONS as i64) => {
             error(form, "E_ITER_BOUND", "linspace exceeds 4,096 values.");
         }
         "ui/split" | "ui/split_at" => {
             if let Some(axis) = positional.first().and_then(|argument| text(argument)) {
                 let choices = operator.choices.iter().find(|(name, _)| name == "axis").map(|(_, values)| values);
                 if choices.is_some_and(|choices| !choices.iter().any(|choice| choice == axis)) {
                     error(form, "E_RANGE", "Split axis is horizontal or vertical.");
                 }
             }
             if operator.name == "ui/split_at" && positional.len() > 1 && number(positional[1]).is_some_and(|value| !(0.1..=0.9).contains(&value)) {
                 error(form, "E_RANGE", "Split ratio is 0.1–0.9.");
             }
             let mut fixed = Vec::new();
             for argument in args.iter().filter(|argument| matches!(argument.key.as_deref(), Some("first_size" | "second_size"))) {
                 if number(argument).is_some_and(|value| value < 1.0) {
                     error(&argument.form, "E_RANGE", "A fixed split size is 1 point or more.");
                 }
                 fixed.push(argument);
             }
             if fixed.len() > 1 {
                 error(form, "E_RANGE", "A split fixes one side: :first_size or :second_size.");
             }
         }
         "ui/graph" => if let Some(argument) = positional.first() {
             if let Some(name) = text(argument) {
                 let valid = graphs.contains_key(name) || base.is_some_and(|base|base.graphs.contains_key(name))
                     || name.strip_prefix("def:").is_some_and(|name| defs.contains_key(name) || base.is_some_and(|base|base.defs.contains_key(name)));
                 if !valid {
                     error(&argument.form, "E_UNKNOWN_GRAPH", &format!("Unknown graph: {name}."));
                 }
             }
         },
         _ => (),
     }
 }
 fn instantiate_fn(&mut self, cx: &Cx, argument: &Arg, signature: &FnSignature, coerce_params: bool) -> Arg {
    if let PackedNode::ListLit(entries) = self.terms.node(argument.term) {
        let source_form = self.term_form(argument.term).clone();
        let len = self.terms.term_ids(entries).len();
        let mut terms = Vec::with_capacity(len);
        let mut values = Vec::with_capacity(len);
        for index in 0..len {
            let entry = self.terms.term_ids(entries)[index];
            let form = self.term_form(entry).clone();
            let (term, value) = self.fn_value(cx, &form, "function port");
            let nested = Arg { key: None, form, term, info: value };
             let nested = self.instantiate_fn(cx, &nested, signature, coerce_params);
             terms.push(nested.term);
             values.push(nested.info);
         }
         let ty = Ty::List(Box::new(Ty::Fn(Some(Box::new(signature.clone())))));
         let mut value = Info::derive(ty.clone(), &values);
         value.len = Some(terms.len());
         return Arg {
             key: argument.key.clone(),
             form: argument.form.clone(),
            term: self.make_term(&source_form, ty, Draft::ListLit(terms)),
             info: value,
         };
     }
     let Some(callable) = argument.info.callable.clone() else {
         return argument.clone();
     };
     let closure = match callable.as_ref() {
         Callable::Local(closure) => Some((**closure).clone()),
         Callable::Def(name) => self.defs_get(name).cloned().map(|definition| Closure {
             params: definition.params.iter().map(|(name, ty, _)| {
                (syntax::make(syntax::Draft::Sym(name.clone())), Some(ty.clone()))
             }).collect(),
             body: definition.body.clone(),
             env: ScopeStack::default(),
             ctx: definition.ctx,
             path: self.path(vec![format!("def:{name}").into()]),
             name: name.clone(),
         }),
         _ => None,
     };
     let mut inputs = Vec::with_capacity(signature.params.len());
     for (index, ty) in signature.params.iter().enumerate() {
         let name = format!("__flow_port_{index}");
        let form = syntax::make(syntax::Draft::Sym(name.clone()));
         let value = Info::leaf(ty.clone());
         inputs.push(Arg {
             key: None,
             form: argument.form.clone(),
            term: self.make_term(&form, ty.clone(), Draft::RefBinding(name, Vec::new())),
             info: value,
         });
     }
     let (body, value) = if let Some(closure) = closure.as_ref() {
         let rc = match callable.as_ref() { Callable::Local(rc) => Some(rc), _ => None };
         self.call_closure(cx, &argument.form, closure, &inputs, rc)
     } else {
         let mut callee = argument.info.clone();
         callee.callable = Some(callable.clone());
         self.apply_callable(cx, &argument.form, &callee, &inputs)
     };
     if !ty::fits(&value.ty, &signature.result) {
         self.error(&argument.form, "E_TYPE", &format!("Function port returns {}; expected {}.", show(&value.ty), show(&signature.result)));
     }
     let (params, zone) = match closure.as_ref() {
         Some(c) if c.params.len() == signature.params.len() => {
             let params = c.params.iter().zip(&signature.params).map(|((pattern, annotation), ty)| {
                 (Self::pattern(pattern), if coerce_params { Some(match annotation { Some(a) => ty::coerce(ty, a), None => ty.clone() }) } else { annotation.clone() })
             }).collect();
             (params, c.path)
         }
         _ => (
             signature.params.iter().enumerate().map(|(index, ty)| (Pattern::Name(format!("__flow_port_{index}")), if coerce_params { Some(ty.clone()) } else { None })).collect(),
             self.inline_path(cx, "~fn"),
         ),
     };
     let specialized_signature = FnSignature { params: signature.params.clone(), result: value.ty.clone() };
     let ty = Ty::Fn(Some(Box::new(specialized_signature)));
     let mut info = argument.info.clone();
     info.ty = ty.clone();
     info.live |= value.live;
     info.state_dep |= value.state_dep;
     info.approx_sources.extend(value.approx_sources);
    let source_form = self.term_form(argument.term).clone();
     Arg {
         key: argument.key.clone(),
         form: argument.form.clone(),
        term: self.make_term(&source_form, ty, Draft::Fn {
            params,
            body,
            zone: self.path_names(zone).to_vec(),
            capture: Some(argument.term),
        }),
         info,
     }
 }
 fn slot_arg(&mut self, kind: &catalog::Kind, arg: &Arg, slot: &catalog::Slot, repeated: bool) {
     let want = slot_ty(Some(slot), kind);
     if ty::fits(&arg.info.ty, &want) {
     } else if repeated && matches!(&arg.info.ty, Ty::List(element) if ty::fits(element, &want)) {
         if arg.info.live_len {
             self.error(&arg.form, "E_TIME_COUNT", &format!("The list passed to {} changes length with t. The network keeps its shape while playing; animate parameters instead, for example scale a piece to 0.", kind.qualified));
         }
     } else {
         self.error(&arg.form, "E_TYPE", &format!("Input {} takes {}", slot.name, show(&want)));
     }
 }
fn apply_kind(&mut self, cx: &Cx, form: &Syntax, kind: &catalog::Kind, args: &[Arg]) -> (TermId, Info) {
     let diagnostic_start = self.diags.len();
     let mut position = 0;
     let mut prepared: Vec<Cow<'_, Arg>> = Vec::with_capacity(args.len());
     for arg in args {
         let slot = if let Some(name) = &arg.key {
             kind.slots.iter().find(|slot| &slot.name == name).cloned()
         } else {
             let index = position;
             position += 1;
             let mut at = 0;
             let mut found = None;
             for slot in &kind.slots {
                 if slot.rest {
                     if index >= at {
                         found = Some(slot.clone());
                     }
                     break;
                 }
                 if index == at {
                     found = Some(slot.clone());
                     break;
                 }
                 at += 1;
             }
             found
         };
         let signature = slot
             .as_ref()
             .and_then(|slot| slot.ty.as_ref())
             .and_then(|ty| match ty {
                 Ty::Fn(Some(signature)) => Some((**signature).clone()),
                 _ => None,
             })
             .or_else(|| {
                 arg.key.as_ref().and_then(|name| {
                     kind.parameters.iter().find(|parameter| &parameter.name == name).and_then(|parameter| {
                         match &parameter.ty {
                             Some(port_type::PortType::Fn(signature)) => Some(signature.clone()),
                             _ => None,
                         }
                     })
                 })
             });
         prepared.push(match signature {
             Some(signature) => Cow::Owned(self.instantiate_fn(cx, arg, &signature, true)),
             None => Cow::Borrowed(arg),
         });
     }

     if !op::is_display_kind(&kind.qualified) {
         for arg in &prepared {
             self.exact_sink(cx, &arg.form, &kind.qualified, &arg.info);
         }
     }

     let rest_index = kind.slots.iter().position(|slot| slot.rest);
     let short = catalog::short(&kind.qualified);
     let nslots = kind.slots.len();
     let plural = |n: usize| if n == 1 { "" } else { "s" };
    let mut named: Vec<(Symbol, TermId)> = Vec::new();
     let mut seen = BTreeSet::new();
     let mut npos = 0usize;
     let mut writes: Vec<String> = Vec::new();
     let groups_in = prepared.iter().flat_map(|arg| arg.info.groups.iter().cloned()).collect::<BTreeSet<_>>();
     for arg in &prepared {
         match &arg.key {
             None => {
                 let index = npos;
                 npos += 1;
                 if rest_index.is_some_and(|first| index >= first) {
                     let slot = &kind.slots[rest_index.unwrap()];
                     self.slot_arg(kind, arg, slot, true);
                     named.push((symbol::intern(&slot.name), arg.term));
                 } else if index >= nslots {
                     self.error(&arg.form, "E_EXTRA_POSITIONAL", &format!("{} takes {} geometry input{}; this one is extra", short, nslots, plural(nslots)));
                 } else {
                     let slot = &kind.slots[index];
                     if seen.contains(&slot.name) {
                         self.error(&arg.form, "E_DUPLICATE_PARAM", &format!("Input {} is given twice: by :{} and by position", slot.name, slot.name));
                     } else {
                         seen.insert(slot.name.clone());
                         self.slot_arg(kind, arg, slot, false);
                         named.push((symbol::intern(&slot.name), arg.term));
                     }
                 }
             }
             Some(name) => {
                 if seen.contains(name) {
                     self.error(&arg.form, "E_DUPLICATE_PARAM", &format!(":{} is given twice", name));
                 } else {
                     seen.insert(name.clone());
                     let slot = kind.slots.iter().find(|slot| &slot.name == name);
                     let parameter = kind.parameters.iter().find(|parameter| &parameter.name == name);
                     match (slot, parameter) {
                         (Some(slot), _) => {
                             self.slot_arg(kind, arg, slot, slot.rest);
                             named.push((symbol::intern(name), arg.term));
                         }
                         (None, Some(parameter)) => {
                             if kind.qualified == "sop/material" && name == "material" && arg.info.ty == ty::material() {
                             } else if matches!((&parameter.ty, &arg.info.ty, &arg.info.callable), (Some(port_type::PortType::Fn(_)), Ty::Fn(None), None)) {
                             } else {
                                let diags = &mut self.diags;
                                validate_parameter_id(Some(arg.info.ty.clone()), &mut |severity, code, message| add_to(diags, severity, &arg.form, code, message.to_owned()), parameter, &self.terms, arg.term);
                            }
                            if let PackedNode::Text(bytes) = self.terms.node(arg.term) {
                                let group = String::from_utf8_lossy(self.terms.raw_bytes(bytes)).into_owned();
                                 if group_reader(parameter) && !group.is_empty() && !groups_in.contains(&group) {
                                     self.warn_owned(&arg.form, "W_UNKNOWN_GROUP", format!("Group \"{group}\" is not made by any node upstream of {short}."));
                                 } else if group_writer(kind, parameter) {
                                     writes.push(group);
                                 }
                             }
                             named.push((symbol::intern(name), arg.term));
                         }
                         (None, None) => {
                             let names = kind.slots.iter().map(|slot| slot.name.clone()).chain(kind.parameters.iter().map(|parameter| parameter.name.clone())).collect::<Vec<_>>();
                             self.error(&arg.form, "E_UNKNOWN_PARAM", &format!("{} has no parameter :{}.{} Parameters: {}. See rays doc {}", short, name, catalog::suggestion(name, &names), names.iter().take(20).map(|name| format!(":{name}")).collect::<Vec<_>>().join(" "), kind.qualified));
                         }
                     }
                 }
             }
         }
     }

     for slot in &kind.slots {
         if slot.required && !slot.rest && !seen.contains(&slot.name) {
             if slot_ty(Some(slot), kind) == ty::geometry() {
                named.push((symbol::intern(&slot.name), self.make_term(form, ty::geometry(), Draft::Nil)));
             } else {
                 self.error(form, "E_MISSING_INPUT", &format!("{} needs its {} input", catalog::short(&kind.qualified), slot.name));
             }
         }
     }
     for parameter in &kind.parameters {
         if parameter.fields.is_empty()
             && matches!(parameter.ty, Some(port_type::PortType::Fn(_) | port_type::PortType::Image))
             && !seen.contains(&parameter.name)
         {
             self.error(form, "E_MISSING_INPUT", &format!("{} needs its :{} input", catalog::short(&kind.qualified), parameter.name));
         }
     }

     let out = if kind.context != context::sop() && kind.context != context::value() {
         context::result(kind.context)
     } else {
         match kind.outputs.as_slice() {
             [( _, output)] => port_ty(output),
             [] => Ty::Any,
             outputs => Ty::Record(outputs.iter().map(|(name, port)| (name.clone().into(), port_ty(port))).collect()),
         }
     };
     let values = args.iter().map(|arg| arg.info.clone()).collect::<Vec<_>>();
     let mut value = Info::derive(out.clone(), &values);
    value.groups = groups_in.iter().cloned().chain(writes.iter().cloned()).collect();
     value.packed.clear();
     value.approx_sources.clear();
    for diagnostic in &mut self.diags[diagnostic_start..] {
        diagnostic.set_signature(Some(crate::registry::kind_signature(kind)));
    }
    (self.make_term(form, out, Draft::Call { kind: kind.qualified.clone(), ctx: kind.context, args: named }), value)
 }
 fn link_path(&mut self, path: CheckPath, producer: &CheckPath) {
     let next = CheckPackedForm::Candidate(Arc::new([*producer].into_iter().collect()));
     self.packed_update(path, next);
 }
fn value_arg(&mut self, x: &Syntax, v: &Info) -> Arg {
    Arg { key: None, form: x.clone(), term: self.make_term(x, v.ty.clone(), Draft::RefBinding("_".into(), Vec::new())), info: v.clone() }
}
fn hof(&mut self, cx: &Cx, x: &Syntax, name: &str, forms: &[packed::SyntaxId]) -> (TermId, Info) {
     let (lo, hi) = match name { "map" => (2, 4), "filter" | "sort_by" => (2, 2), _ => (3, 3) };
     let n = forms.len();
     if n < lo || n > hi {
         return self.fail_term(x, "E_ARITY", match name {
             "map" => "map is (map f list …) with 1–3 lists.",
             "filter" => "filter is (filter pred list).",
             "reduce" => "reduce is (reduce f init list).",
             _ => "sort_by is (sort_by key list).",
         });
     }
    let f_form = x.child(forms[0]);
    let (ft, fv) = if syntax::head(&f_form) == Some("fn") { self.input(cx, &f_form, "0") } else { self.fn_value(cx, &f_form, name) };
    let reduce = name == "reduce";
    let init = if reduce { Some(self.input(cx, &x.child(forms[1]), "1")) } else { None };
     if let Some((_, iv)) = &init { self.no_fn(x, &iv.ty, "The reduce accumulator"); }
     let first = if reduce { 2 } else { 1 };
    let mut lists: Vec<(Syntax, TermId, Info)> = Vec::new();
    for (i, id) in forms[first..].iter().enumerate() {
        let a = x.child(*id);
        let (t, v) = self.input(cx, &a, &(i + first).to_string());
         if ty::elem(&v.ty).is_none() && v.ty != Ty::Any {
            self.error(&a, "E_TYPE", &format!("{name} iterates a list; got {}.", show(&v.ty)));
         }
         lists.push((a.clone(), t, v));
     }
     let elem = |v: &Info| {
         let mut v = v.clone();
         v.ty = ty::elem(&v.ty).unwrap_or(Ty::Any);
         v.live_len = false;
         v.len = None;
         v.callable = None;
         v
     };
     let call_args: Vec<Arg> = match (&init, lists.first()) {
         (Some((_, iv)), Some((_, _, l))) => vec![self.value_arg(x, iv), self.value_arg(x, &elem(l))],
         _ => lists.iter().map(|(_, _, l)| self.value_arg(x, &elem(l))).collect(),
     };
     let (result, rv) = self.apply_callable(cx, x, &fv, &call_args);
     let ft = match fv.callable.as_deref() {
         Some(Callable::Op(_) | Callable::Kind(_)) => ft,
         _ => {
             let signature = FnSignature { params: call_args.iter().map(|a| a.info.ty.clone()).collect(), result: rv.ty.clone() };
             self.instantiate_fn(cx, &Arg { key: None, form: f_form.clone(), term: ft, info: fv.clone() }, &signature, false).term
         }
     };
     let l0 = lists.first().map(|(_, _, v)| v.clone()).unwrap_or_else(|| Info::leaf(Ty::Any));
     let packed = lists.iter().any(|(_, _, v)| matches!(v.ty, Ty::Array(_)));
     if packed {
         for (a, _, v) in &lists {
             if !matches!(v.ty, Ty::Array(_)) { self.error(a, "E_ARRAY_TYPE", "A packed map combines packed arrays."); }
         }
         if shape_ty(&rv.ty) { self.error(x, "E_ARRAY_TYPE", "Packed-array functions produce data, not graph nodes."); }
     }
     let list_vs: Vec<Info> = lists.iter().map(|(_, _, v)| v.clone()).collect();
     let any_len = list_vs.iter().any(|v| v.live_len);
     let mut terms = vec![ft];
     if let Some((t, _)) = &init { terms.push(*t); }
     terms.extend(lists.iter().map(|(_, t, _)| *t));
     let (kind, out, live_len) = match name {
         "map" => {
             self.no_fn(x, &rv.ty, "The map result");
             let out = if packed {
                 Ty::Array(Box::new(match rv.ty {
                     Ty::Int | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text => rv.ty.clone(),
                     Ty::Float | Ty::Bool | Ty::Any => Ty::Float,
                     _ => { self.error(x, "E_ARRAY_TYPE", "Arrays hold ints, floats, vectors or text values."); Ty::Float }
                 }))
             } else { Ty::List(Box::new(rv.ty.clone())) };
             (HofKind::Map, out, any_len)
         }
         "filter" => {
             if !ty::fits(&rv.ty, &Ty::Bool) { self.error(x, "E_TYPE", &format!("filter's predicate returns bool; it returns {}.", show(&rv.ty))); }
             (HofKind::Filter, l0.ty, any_len || rv.live)
         }
         "sort_by" => {
             if !matches!(rv.ty, Ty::Int | Ty::Float | Ty::Bool | Ty::Any) { self.error(x, "E_TYPE", &format!("sort_by's key returns a number; it returns {}.", show(&rv.ty))); }
             (HofKind::SortBy, l0.ty, any_len)
         }
         _ => {
             let it = init.as_ref().map(|(_, iv)| iv.ty.clone()).unwrap_or(Ty::Any);
             self.no_fn(x, &rv.ty, "The reduce result");
             if !ty::fits(&rv.ty, &it) && !matches!((&it, &rv.ty), (Ty::Int, Ty::Float)) {
                 self.error(x, "E_TYPE", &format!("reduce's function must return the accumulator type {}; it returns {}.", show(&it), show(&rv.ty)));
             }
             let out = ty::join(&it, &rv.ty).unwrap_or(it);
             (HofKind::Reduce, out, false)
         }
     };
     let mut vals = vec![fv.clone(), rv.clone()];
     vals.extend(list_vs.iter().cloned());
     if let Some((_, iv)) = &init { vals.push(iv.clone()); }
     let mut v = Info::derive(out.clone(), &vals);
    let specialized = match self.terms.node(result) { PackedNode::CallFn(id) => self.terms.call_fn(id).body.unwrap_or(result), _ => result };
    let eligible = packed_body(&self.ops, &self.terms, specialized);
     let params: Option<Vec<(Syntax, Option<Ty>)>> = match fv.callable.as_deref() {
         Some(Callable::Local(c)) => Some(c.params.clone()),
        Some(Callable::Def(def)) => self.defs_get(def).map(|d| d.params.iter().map(|(pn, pty, _)| (syntax::make(syntax::Draft::Sym(pn.clone())), Some(pty.clone()))).collect()),
         _ => None,
     };
     let inputs_fit = params.as_ref().is_none_or(|params| {
         params.len() == lists.len() && params.iter().zip(&lists).all(|((p, annotation), (_, _, v))| matches!(p.node, SNode::Sym(_)) && (annotation.is_none() || *annotation == ty::elem(&v.ty)))
     });
     let mut reasons: Vec<Diagnostic> = Vec::new();
     if kind != HofKind::Map {
         reasons.push(err("E_GPU_FORM", "GPU kernels require collecting maps.".into(), Some(x.span.clone())));
     } else {
         for (_, t, _) in &lists {
            match self.terms.ty(*t) {
                 Ty::Any => (),
                 Ty::Array(e) if **e == Ty::Any => (),
                 ty if packed_array(ty) => (),
                ty => reasons.push(err("E_PACKED_TYPE", format!("Packed source requires Array Float/Vec2/Vec3/Vec4, got {}.", show(ty)), Some(self.term_form(*t).span.clone()))),
             }
         }
         if let Some(params) = &params {
             if params.len() != lists.len() {
                 reasons.push(err("E_PACKED_FORM", "Packed parameter count differs from its sources.".into(), Some(x.span.clone())));
             } else {
                 for ((p, annotation), (_, _, v)) in params.iter().zip(&lists) {
                     let have = ty::elem(&v.ty);
                     match (&p.node, annotation, &have) {
                         (SNode::Sym(_), _, None) if v.ty == Ty::Any => (),
                         (SNode::Sym(_), _, Some(Ty::Any)) | (SNode::Sym(_), None, _) => (),
                         (SNode::Sym(_), Some(want), Some(have)) if want == have => (),
                         (SNode::Sym(_), _, _) => reasons.push(err("E_PACKED_TYPE", "Packed parameter annotation differs from its source.".into(), Some(x.span.clone()))),
                         _ => reasons.push(err("E_PACKED_FORM", "Packed parameters require name patterns.".into(), Some(x.span.clone()))),
                     }
                 }
             }
         }
     }
     if reasons.is_empty() { self.add_root(x, cx.path, None); } else { for r in reasons { self.add_root(x, cx.path, Some(r)); } }
    self.link_body(specialized, &cx.path);
     match fv.callable.as_deref() {
         Some(Callable::Local(c)) => { let cid = c.path; self.link_path(cid, &cx.path); }
         Some(Callable::Def(def)) => {let path=self.path(vec![format!("def:{def}").into()]);self.link_path(path, &cx.path)},
         _ => (),
     }
     v.live_len = live_len;
     v.live = v.live || live_len;
     v.packed = [cx.path].into_iter().collect();
     v.approx = kind == HofKind::Map && packed_array(&out) && inputs_fit && lists.iter().all(|(_, _, l)| packed_array(&l.ty)) && eligible;
    (self.make_term(x, out, Draft::Hof(kind, terms)), v)
 }
fn iterate(&mut self, cx: &Cx, f: &Syntax, forms: &[packed::SyntaxId]) -> (TermId, Info) {
    let until_form = match forms {
        [_, _, _] => None,
        [_, _, _, k, g] if matches!(f.arena().row(*k).node, SNode::Kw(k) if f.name(k) == "until") => Some(f.child(*g)),
         _ => return self.fail_term(f, "E_ARITY", "iterate is (iterate n init (fn [x] body)) with an optional :until (fn [prev next] bool)."),
     };
    let (n_form, init_form, f_form) = (f.child(forms[0]), f.child(forms[1]), f.child(forms[2]));
    let (nt, nv) = self.input(cx, &n_form, "0");
     if !ty::fits(&nv.ty, &Ty::Int) && nv.ty != Ty::Any {
        self.error(&n_form, "E_TYPE", &format!("iterate's count is an int; got {}.", show(&nv.ty)));
     }
    let (it, iv) = self.input(cx, &init_form, "1");
     self.no_fn(f, &iv.ty, "The iterate value");
     let fn_term = |s: &mut Self, key: &str, form: &Syntax| if syntax::head(form) == Some("fn") { s.input(cx, form, key) } else { s.fn_value(cx, form, "iterate") };
    let (ft, fv) = fn_term(self, "2", &f_form);
     let plain = |v: &Info| { let mut v = v.clone(); v.live_len = false; v.len = None; v.callable = None; v };
     let arg_iv = self.value_arg(f, &plain(&iv));
     let (result, rv) = self.apply_callable(cx, f, &fv, std::slice::from_ref(&arg_iv));
     self.no_fn(f, &rv.ty, "The iterate result");
     if !ty::fits(&rv.ty, &iv.ty) && !matches!((&iv.ty, &rv.ty), (Ty::Int, Ty::Float)) {
         self.error(f, "E_TYPE", &format!("iterate's function must return the value type {}; it returns {}.", show(&iv.ty), show(&rv.ty)));
     }
     let ty = ty::join(&iv.ty, &rv.ty).unwrap_or(iv.ty.clone());
     if shape_ty(&ty) { self.error(f, "E_ARRAY_TYPE", "iterate carries data; it cannot create graph nodes."); }
     let ft = match fv.callable.as_deref() {
         Some(Callable::Op(_) | Callable::Kind(_)) => ft,
         _ => self.instantiate_fn(cx, &Arg { key: None, form: f_form, term: ft, info: fv.clone() }, &FnSignature { params: vec![iv.ty.clone()], result: rv.ty.clone() }, false).term,
     };
     let until = until_form.map(|g| {
        let (gt, gv) = fn_term(self, ":until", &g);
         let arg_rv = self.value_arg(f, &plain(&rv));
         let (_, uv) = self.apply_callable(cx, f, &gv, &[arg_iv.clone(), arg_rv]);
        if !ty::fits(&uv.ty, &Ty::Bool) { self.error(&g, "E_TYPE", &format!("iterate's :until returns bool; it returns {}.", show(&uv.ty))); }
         let gt = match gv.callable.as_deref() {
             Some(Callable::Op(_) | Callable::Kind(_)) => gt,
             _ => self.instantiate_fn(cx, &Arg { key: None, form: g, term: gt, info: gv.clone() }, &FnSignature { params: vec![iv.ty.clone(), rv.ty.clone()], result: Ty::Bool }, false).term,
         };
         (gt, gv, uv)
     });
    let specialized = match self.terms.node(result) { PackedNode::CallFn(id) => self.terms.call_fn(id).body.unwrap_or(result), _ => result };
     let mut vals = vec![fv.clone(), rv.clone(), iv.clone(), nv];
     if let Some((_, gv, uv)) = &until { vals.push(gv.clone()); vals.push(uv.clone()); }
     let mut v = Info::derive(ty.clone(), &vals);
     self.add_root(f, cx.path, Some(err("E_GPU_FORM", "GPU kernels require collecting maps.".into(), Some(f.span.clone()))));
    self.link_body(specialized, &cx.path);
     match fv.callable.as_deref() {
         Some(Callable::Local(c)) => { let cid = c.path; self.link_path(cid, &cx.path); }
         Some(Callable::Def(def)) => {let path=self.path(vec![format!("def:{def}").into()]);self.link_path(path, &cx.path)},
         _ => (),
     }
     let mut terms = vec![ft, it, nt];
     if let Some((gt, _, _)) = &until { terms.push(*gt); }
     v.live_len = false;
     v.packed = [cx.path].into_iter().collect();
    (self.make_term(f, ty, Draft::Hof(HofKind::Iterate, terms)), v)
 }
 fn need(&mut self, a: &Arg, what: &str, want: &Ty) -> bool {
     if ty::fits(&a.info.ty, want) || (a.info.ty == Ty::Fn(None) && a.info.callable.is_none() && matches!(want, Ty::Fn(_))) { return true; }
     self.error(&a.form, "E_TYPE", &format!("{what}: expected {}, got {}.", show(want), show(&a.info.ty)));
     false
 }
fn ref_form(&mut self, cx: &Cx, x: &Syntax, forms: &[packed::SyntaxId]) -> (TermId, Info) {
    self.events += 1;
    let Some(first) = forms.first() else { return self.fail_term(x, "E_ARGS", "ref takes a graph name, then :input value pairs.") };
    let first = x.child(*first);
    let SNode::Sym(g) = first.node else { return self.fail_term(x, "E_ARGS", "ref takes a graph name, then :input value pairs.") };
    let g = first.name(g);
    let Some(sg) = self.graphs_get(g).cloned() else { return self.fail_term(&first, "E_UNKNOWN_GRAPH", &format!("Unknown graph reference: {g}.")) };
     if cx.in_def { return self.fail_term(x, "E_REF_IN_DEF", "Reusable functions cannot capture project graphs; add an explicit parameter."); }
     let gv = self.graph_info(x, g);
    let mut overrides: Vec<(Symbol, TermId)> = Vec::new();
     let mut vs = vec![gv.clone()];
     let mut rest = &forms[1..];
     loop {
         match rest {
            [key, value, more @ ..] if matches!(x.arena().row(*key).node, SNode::Kw(_)) => {
                let kf = x.child(*key);
                let SNode::Kw(k) = kf.node else { unreachable!() };
                let k = kf.name(k);
                let value = x.child(*value);
                match sg.params.iter().find(|(n, _, _)| n == k) {
                     None => {
                         let inputs = if sg.params.is_empty() { "none".to_owned() } else { sg.params.iter().map(|(n, _, _)| n.clone()).collect::<Vec<_>>().join(", ") };
                        self.error(&kf, "E_UNKNOWN_PARAM", &format!("Graph {g} has no input :{k}. Inputs: {inputs}."));
                     }
                     Some((_, want, _)) => {
                        let (t, v) = self.infer(cx, &value);
                        self.exact_sink(cx, &value, &format!("ref {g} :{k}"), &v);
                        self.no_fn(&value, &v.ty, &format!("Input :{k} of {g}"));
                        let arg = self.value_arg(&value, &v);
                         self.need(&arg, &format!(":{k} of {g}"), want);
                        overrides.push((symbol::intern(k), t));
                         vs.push(v);
                     }
                 }
                 rest = more;
             }
             [] => break,
            [y, ..] => { self.error(&x.child(*y), "E_ARGS", "ref takes a graph name, then :input value pairs."); break; }
         }
     }
     let out = context::result(sg.ctx);
     let mut info = Info::derive(out.clone(), &vs);
     info.groups.clear();
     info.approx = gv.approx;
    (self.make_term(x, out, Draft::GraphRef { graph: g.to_owned(), inputs: overrides }), info)
}
fn with_error(&mut self,cx:&Cx,f:&Syntax,forms:&[packed::SyntaxId])->(TermId,Info){
    if forms.len()!=2{return self.fail_term(f,"E_ARITY","with_error takes a protected value and a pure error mapper.")}
    let protected_form = f.child(forms[0]);
    let mapper_form = f.child(forms[1]);
    let(protected,protected_info)=self.input(cx,&protected_form,"0");
    let(mapper_term,mapper_info)=self.fn_value(cx,&mapper_form,"with_error");
     let payload=Ty::Record(vec![("code".into(),Ty::Text),("message".into(),Ty::Text),("cause".into(),Ty::List(Box::new(Ty::Text)))]);
    let argument=self.instantiate_fn(cx,&Arg{key:None,form:mapper_form.clone(),term:mapper_term,info:mapper_info},&FnSignature{params:vec![payload.clone()],result:payload.clone()},false);
     let(mapper,mapper_info)=(argument.term,argument.info);
    match &mapper_info.ty{Ty::Fn(Some(s)) if s.params==[payload.clone()]&&ty::fits(&s.result,&payload)=>(),_=>self.error(&mapper_form,"E_DIAGNOSTIC_MAP","An error mapper takes and returns a diagnostic record.")}
    self.no_fn(&protected_form,&protected_info.ty,"A protected value");
     fn cooked_container(t:&Ty)->bool{cooked(t)||match t{Ty::List(e)=>cooked_container(e),Ty::Record(fs)=>fs.iter().any(|(_,t)|cooked_container(t)),_=>false}}
    if cooked_container(&protected_info.ty)&&protected_info.ty!=ty::geometry()&&protected_info.ty!=ty::image(){self.error(&protected_form,"E_ERROR_SCOPE","An error boundary completes a direct cooked result, not a cooked container.")}
    self.pure_boundary(cx,false,"E_ERROR_SCOPE","An error boundary has no state, graph, drawing or UI effects.",protected);
    if mapper_info.live||mapper_info.state_dep{self.error(&mapper_form,"E_DIAGNOSTIC_MAP","An error mapper is pure and independent of frame/state.")}
    self.pure_boundary(cx,true,"E_DIAGNOSTIC_MAP","An error mapper is pure: no frame, state, context or cooked values.",mapper);
    let result=Info::derive(protected_info.ty.clone(),&[protected_info,mapper_info]);
    (self.make_term(f,result.ty.clone(),Draft::WithError(protected,mapper)),result)
}
fn error_rows(&mut self,cx:&Cx,f:&Syntax,forms:&[packed::SyntaxId])->(TermId,Info){
    let is_policy=|id:packed::SyntaxId|matches!(f.arena().row(id).node,SNode::Kw(k) if f.name(k)=="policy");
    let parse=|this:&mut Self,rows:packed::SyntaxId,mapper:packed::SyntaxId,policy:packed::SyntaxId|->Option<(Syntax,Syntax,DiagnosticPolicy)>{
        let policy=f.child(policy);
        match policy.node{
            SNode::Str(s) if policy.bytes(s)==b"first"=>Some((f.child(rows),f.child(mapper),DiagnosticPolicy::First)),
            SNode::Str(s) if policy.bytes(s)==b"range-last"=>Some((f.child(rows),f.child(mapper),DiagnosticPolicy::RangeLast)),
            _=>{this.error(&policy,"E_DIAGNOSTIC_POLICY","Diagnostic policy is literal first or range-last.");None}
        }
    };
    let parsed=if forms.len()==2{Some((f.child(forms[0]),f.child(forms[1]),DiagnosticPolicy::First))}else if forms.len()==4&&is_policy(forms[1]){parse(self,forms[0],forms[3],forms[2])}else if forms.len()==4&&is_policy(forms[2]){parse(self,forms[0],forms[1],forms[3])}else{None};
     let Some((rows_form,mapper_form,policy))=parsed else{return self.fail_term(f,"E_ARGS","error_rows takes rows, a pure diagnostic mapper, and optional :policy text.")};
     let(rows,rv)=self.input(cx,&rows_form,"0");
    if rv.ty!=Ty::Array(Box::new(Ty::Int)){self.error(&rows_form,"E_DIAGNOSTIC_ROWS","Diagnostic rows require an integer 0/1 array.")}
     let(mapper_term,mapper_info)=self.fn_value(cx,&mapper_form,"error_rows");
     let payload=Ty::Record(vec![("code".into(),Ty::Text),("message".into(),Ty::Text),("cause".into(),Ty::List(Box::new(Ty::Text)))]);
     let argument=self.instantiate_fn(cx,&Arg{key:None,form:mapper_form.clone(),term:mapper_term,info:mapper_info},&FnSignature{params:vec![Ty::Int],result:payload.clone()},false);
     let(mapper,mv)=(argument.term,argument.info);
    match &mv.ty{Ty::Fn(Some(s)) if s.params==[Ty::Int]&&ty::fits(&s.result,&payload)=>(),_=>self.error(&mapper_form,"E_DIAGNOSTIC_MAP","A row mapper takes an int and returns a diagnostic record.")}
    if mv.live||mv.state_dep{self.error(&mapper_form,"E_DIAGNOSTIC_MAP","A row mapper is pure and independent of frame/state.")}
    self.pure_boundary(cx,true,"E_DIAGNOSTIC_MAP","A row mapper is pure: no frame, state, context or cooked values.",mapper);
     let out=ty::geometry();
    (self.make_term(f,out.clone(),Draft::ErrorRows{rows,policy,mapper}),Info::derive(out,&[rv,mv]))
 }
 fn graph_info(&mut self, x: &Syntax, name: &str) -> Info {
     if let Some(v) = self.ginfo_get(name) { return v.clone(); }
     let Some(sg) = self.graphs_get(name).cloned() else { return Info::leaf(Ty::Any) };
     if self.graph_stack.iter().any(|entry| entry == name) {
         let mut cycle = self.graph_stack.clone();
         cycle.push(name.to_owned());
         self.error(x, "E_GRAPH_CYCLE", &format!("Graph cycle: {}.", cycle.join(" → ")));
         return Info::leaf(context::result(sg.ctx));
     }
     self.events += 1;
     let outer_terms = std::mem::take(&mut self.terms);
     let outer_calls = std::mem::take(&mut self.calls);
     let graph_form = self.terms.import_form(&sg.form);
     self.graph_stack.push(name.to_owned());
     let cx0 = Cx { env: ScopeStack::default(), ctx: sg.ctx, path: self.path(vec![symbol::intern(name)]), scope: name.to_owned(), in_def: false, stack: Vec::new(), depth: 0, zone: None, zbody: false };
     let mut env = ScopeStack::default().push();
     let mut inputs = Vec::new();
     for (n, t, df) in &sg.params {
         let mut cx = cx0.clone();
         cx.env = env.snapshot();
         let (dt, dv) = match df {
             Some(d) => {
                 let (term, v) = self.infer(&cx, d);
                 if !ty::fits(&v.ty, t) {
                     self.error(d, "E_TYPE", &format!("The default of {name} input {n} is {}; it is declared {}.", show(&v.ty), show(t)));
                 }
                 (Some(term), v)
             }
             None => (None, Info::leaf(t.clone())),
         };
         let mut v = Info::leaf(t.clone());
         v.live = dv.live;
         v.state_dep = dv.state_dep;
         v.packed = dv.packed.clone();
         v.approx_sources = dv.approx_sources.clone();
         let input_path=self.path(vec![symbol::intern(name), format!(":{n}").into()]);
         self.mark(input_path, &v);
         env.top_mut().insert(n.clone(), v);
         inputs.push((n.clone(), t.clone(), dt));
     }
     let mut cx = cx0;
     cx.env = env.snapshot();
     let (bt, bv) = self.body(&cx, &sg.body);
     let want = context::result(sg.ctx);
     if !ty::fits(&bv.ty, &want) {
         self.error(&sg.body, "E_TYPE", &format!("{name} must return {}, but its result is {}.", show(&want), show(&bv.ty)));
     }
     self.graph_stack.pop();
     let mut roots = inputs.iter().filter_map(|(_, _, default)| *default).collect::<Vec<_>>();
     roots.push(bt);
     let (arena, roots, forms) = self.finish_terms(outer_terms, outer_calls, &roots, graph_form);
     let mut roots = roots.into_iter();
     let inputs = inputs.into_iter().map(|(name, ty, default)| {
         (name, ty, default.map(|_| arena.handle(roots.next().unwrap())))
     }).collect();
     let body = arena.handle(roots.next().unwrap());
     let form = arena.syntax().handle(forms[0]);
     self.graph_terms.insert(name.to_owned(), Graph { name: name.to_owned(), context: sg.ctx, inputs, body, form });
     let mut v = Info::leaf(want);
     v.live = bv.live;
     v.approx = bv.approx;
     v.approx_sources = bv.approx_sources.clone();
     v.packed = bv.packed.clone();
     v.state_dep = bv.state_dep;
     self.ginfo.insert(name.to_owned(), v.clone());
     v
 }
}

impl Checker {
    fn reserved(&self, name: &str) -> bool {
        SPECIAL.contains(&name)
            || ["t", "pi", "true", "false", "nil"].contains(&name)
    }

    fn known_macro_name(&self, head: bool, name: &str) -> bool {
        self.reserved(name)
            || ty::names().iter().any(|(n, _)| n == name)
            || name == "fn"
            || self.declared_names_contains(name)
            || (head || !macros::valid_name(name))
                && (context::all().iter().any(|c| op::find_indexed(&self.ops, name, c).is_some())
                    || match catalog::resolve_kind(&self.catalog, &context::sop(), name) {
                        Ok(_) => true,
                        Err((code, _)) => code == "E_WRONG_CONTEXT" || code == "E_AMBIGUOUS",
                    })
    }

    fn metadata(&mut self, form: &Syntax, allowed: &[&str]) {
        for metadata in form.meta() {
            if !allowed.contains(&metadata.as_str()) {
                self.warn(form, "W_UNKNOWN_META", &format!("Unknown metadata ^:{metadata} here; it is ignored."));
            }
        }
    }

    fn infer_context(&self, form: &Syntax, declarations: &[(String, String, Syntax)], depth: usize) -> Option<Context> {
        let depth = depth.checked_sub(1)?;
        if let Some(head) = syntax::head(form) {
            let referenced = if head == "ref" {
                form.children().get(1).and_then(|&id| match form.child(id).node { SNode::Sym(name) => Some(form.name(name)), _ => None })
            } else { Some(head) };
            if let Some((_, _, declaration)) = declarations.iter().find(|(kind, name, _)|
                referenced == Some(name.as_str()) && if head == "ref" { kind == "graph" } else { kind == "defn" }) {
                let items = declaration.children();
                if items.get(2).is_some_and(|&id| matches!(declaration.child(id).node, SNode::Kw(key) if declaration.name(key) == "context")) {
                    return items.get(3).and_then(|&id| match declaration.child(id).node {
                        SNode::Sym(name) => context::of_string(declaration.name(name)).ok(), _ => None,
                    });
                }
                return items.last().and_then(|&id| self.infer_context(&declaration.child(id), declarations, depth));
            }
            if let Some(definition) = self.defs_get(head) { return Some(definition.ctx); }
            if let Some(operator) = op::find_ref(&self.ops, head, &context::value()) { return Some(operator.ctx); }
            let candidates: BTreeSet<_> = self.catalog.kinds.iter()
                .filter(|kind| kind.qualified == head || catalog::short(&kind.qualified) == head)
                .map(|kind| context::name(kind.context)).collect();
            if candidates.len() == 1 { return context::of_string(candidates.first().copied()?).ok(); }
            if matches!(head, "let" | "let*") {
                let children = form.children();
                if let Some(&body) = children.last() {
                    if let Some(ctx) = self.infer_context(&form.child(body), declarations, depth) { return Some(ctx); }
                }
            }
        }
        let mut contexts = BTreeSet::new();
        for &child in form.children() {
            if let Some(ctx) = self.infer_context(&form.child(child), declarations, depth) {
                if ctx != context::value() { contexts.insert(context::name(ctx)); }
            }
        }
        if contexts.len() == 1 { return context::of_string(contexts.first().copied()?).ok(); }
        if !contexts.is_empty() { return None; }
        match form.node {
            SNode::Num(_) | SNode::Str(_) | SNode::Vec(_) | SNode::Map(_) => Some(context::value()),
            _ => None,
        }
    }

    fn parse_signature(&mut self, form: &Syntax, head: &str, name: &str, declarations: &[(String, String, Syntax)]) -> Option<Sig> {
        let SNode::List(items) = form.node else {
            self.error(form, "E_SHAPE", "Expected a graph or defn declaration.");
            return None;
        };
        let items = form.range(items);
        let explicit = items.get(2).is_some_and(|&id| matches!(form.arena().row(id).node, SNode::Kw(key) if form.name(key) == "context"));
        let start = if explicit { 4 } else { 2 };
        let tail = &items[start.min(items.len())..];
        let (parameters, body): (&[packed::SyntaxId], packed::SyntaxId) = match tail {
            [body] if head == "graph" => (&[], *body),
            [parameters, body] => match form.arena().row(*parameters).node {
                SNode::Vec(range) => (form.range(range), *body),
                _ => { self.error(form, "E_SHAPE", "Graph/function inputs must be a parameter vector."); return None; }
            },
            _ => { self.error(form, "E_SHAPE", &format!("Invalid shape: expected ({head} {name} [:context ctx] [params] body).")); return None; }
        };
        let ctx = if explicit {
            let SNode::Sym(context_name) = form.arena().row(items[3]).node else {
                self.error(form, "E_CONTEXT_UNKNOWN", "Unknown context."); return None;
            };
            match context::of_string(form.name(context_name)) {
                Ok(ctx) => ctx,
                Err(problem) => { self.diags.push(problem); return None; }
            }
        } else {
            match self.infer_context(&form.child(body), declarations, 64) {
                Some(ctx) => ctx,
                None => { self.error(form, "E_CONTEXT_INFER", "The body does not identify exactly one context; write :context explicitly."); return None; }
            }
        };
        let mut params = Vec::new();
        let mut seen = BTreeSet::new();
        for id in parameters {
            let parameter = form.child(*id);
            let SNode::List(parts) = parameter.node else {
                self.error(&parameter, "E_PARAM", &format!("Invalid parameter in {name}. Each is (name : type default?)."));
                continue;
            };
            let parts = parameter.range(parts);
            if !(3..=4).contains(&parts.len())
                || !matches!(parameter.arena().row(parts[0]).node, SNode::Sym(_))
                || !matches!(parameter.arena().row(parts[1]).node, SNode::Sym(colon) if parameter.name(colon) == ":")
            {
                self.error(&parameter, "E_PARAM", &format!("Invalid parameter in {name}. Each is (name : type default?)."));
                continue;
            }
            let SNode::Sym(param_name) = parameter.arena().row(parts[0]).node else { continue };
            let param_name = parameter.name(param_name);
            let Some(param_ty) = ty::of_syntax(&parameter.child(parts[2])) else {
                self.error(&parameter, "E_PARAM", &format!("Invalid parameter in {name}. Each is (name : type default?)."));
                continue;
            };
            let default = parts.get(3).map(|id| parameter.child(*id));
            if !macros::valid_name(param_name) || self.reserved(param_name) || !seen.insert(param_name.to_owned()) {
                self.error(&parameter, "E_PARAM", &format!("Invalid or repeated parameter in {name}."));
                continue;
            }
            if ty::has_fn(&param_ty) && (param_ty != Ty::Fn(None) || head == "graph") {
                self.error(&parameter, "E_FN_ESCAPES", &format!("{name} input {param_name}: a function value cannot be stored or returned (E_FN_ESCAPES)."));
                continue;
            }
            if param_ty == Ty::Fn(None) && default.is_some() {
                self.error(&parameter, "E_PARAM", &format!("{name} input {param_name}: a fn input has no default; callers pass a function."));
                continue;
            }
            if head == "graph" && default.is_none() {
                self.error(&parameter, "E_INPUT_DEFAULT", &format!("Graph input {param_name} of {name} needs a default, so the graph runs on its own."));
            }
            params.push((param_name.to_owned(), param_ty, default));
        }
        Some(Sig { name: name.to_owned(), ctx, params, body: form.child(body), form: form.clone() })
    }

    fn check_def_body(&mut self, name: &str, own: bool) {
        if own && (self.checked_defs_contains(name) || !self.checked_defs.insert(name.to_owned())) {
            return;
        }
        let Some(definition) = self.defs_get(name).cloned() else { return };
        self.events += 1;
        let outer_terms = std::mem::take(&mut self.terms);
        let outer_calls = std::mem::take(&mut self.calls);
        let graph_form = self.terms.import_form(&definition.form);
        let mut env = ScopeStack::default().push();
        let mut inputs = Vec::new();
        for (input, want, default) in &definition.params {
            let value = if let Some(default) = default {
                let cx = Cx {
                    env: ScopeStack::default(),
                    ctx: definition.ctx,
                    path: self.path(vec![format!("def:{name}").into(), format!(":{input}").into()]),
                    scope: format!("ƒ {name}"),
                    in_def: true,
                    stack: vec![name.to_owned()],
                    depth: 0,
                    zone: None,
                    zbody: false,
                };
                let (term, value) = self.infer(&cx, default);
                if !ty::fits(&value.ty, want) {
                    self.error(default, "E_TYPE", &format!("The default of {name} input {input} is {}; it is declared {}.", show(&value.ty), show(want)));
                }
                Some(term)
            } else {
                None
            };
            env.top_mut().insert(input.clone(), Info::leaf(want.clone()));
            inputs.push((input.clone(), want.clone(), value));
        }
        let cx = Cx {
            env: env.snapshot(),
            ctx: definition.ctx,
            path: self.path(vec![format!("def:{name}").into()]),
            scope: format!("ƒ {name}"),
            in_def: true,
            stack: vec![name.to_owned()],
            depth: 0,
            zone: None,
            zbody: false,
        };
        let body = if own {
            self.body(&cx, &definition.body).0
        } else {
            self.make_term(&definition.body, Ty::Any, Draft::Nil)
        };
        let mut roots = inputs.iter().filter_map(|(_, _, default)| *default).collect::<Vec<_>>();
        roots.push(body);
        let (arena, roots, forms) = self.finish_terms(outer_terms, outer_calls, &roots, graph_form);
        let mut roots = roots.into_iter();
        let inputs = inputs.into_iter().map(|(name, ty, default)| {
            (name, ty, default.map(|_| arena.handle(roots.next().unwrap())))
        }).collect();
        let body = arena.handle(roots.next().unwrap());
        self.def_terms.insert(name.to_owned(), Graph {
            name: name.to_owned(),
            context: definition.ctx,
            inputs,
            body,
            form: arena.syntax().handle(forms[0]),
        });
    }

    fn contains_body_context_op(&self, root: TermId) -> bool {
        let mut todo = vec![root];
        while let Some(term) = todo.pop() {
            if let PackedNode::Op(id) = self.terms.node(term) {
                if BODY_CONTEXT_OPS.contains(&self.terms.name(self.terms.op(id).op)) {
                    return true;
                }
            }
            todo.extend_from_slice(self.terms.children(term));
        }
        false
    }

    fn pure_boundary(&mut self, cx: &Cx, mapper: bool, code: &str, message: &str, root: TermId) {
        let mut visited = BTreeSet::new();
        let mut todo = vec![root];
        while let Some(term) = todo.pop() {
            if mapper && cooked(self.terms.ty(term)) {
                add_to(&mut self.diags, Severity::ErrorSeverity, self.terms.source_form(term).expect("checked term source form"), code, message.to_owned());
            }
            let invalid = match self.terms.node(term) {
                PackedNode::State(_) | PackedNode::GraphRef(_) => true,
                PackedNode::Time if mapper => true,
                PackedNode::Call(id) => {
                    let call_context = self.terms.call(id).ctx;
                    mapper || (call_context != context::sop() && call_context != context::image())
                }
                PackedNode::Op(id) => {
                    let name = self.terms.name(self.terms.op(id).op);
                    let operator = op::find_indexed(&self.ops, name, &cx.ctx);
                    !operator.as_ref().is_some_and(|operator| {
                        if mapper {
                            operator.ctx == context::value() && !operator.live && !BODY_CONTEXT_OPS.contains(&operator.name.as_str())
                        } else {
                            operator.ctx == context::value() || operator.ctx == context::sop() || operator.ctx == context::image()
                        }
                    })
                }
                PackedNode::RefBinding(id) if mapper => {
                    let name = self.terms.name(self.terms.ref_binding(id).name);
                    cx.env.get(name).is_some_and(|value| value.live || value.state_dep || cooked(&value.ty))
                }
                PackedNode::FnRef(name) => {
                    self.pure_helper(cx, mapper, code, message, term, name, &mut visited, &mut todo);
                    false
                }
                PackedNode::CallFn(id) if self.terms.call_fn(id).body.is_none() => {
                    let name = self.terms.call_fn(id).name;
                    self.pure_helper(cx, mapper, code, message, term, name, &mut visited, &mut todo);
                    false
                }
                PackedNode::ErrorRows(_) if mapper => true,
                _ => false,
            };
            if invalid {
                add_to(&mut self.diags, Severity::ErrorSeverity, self.terms.source_form(term).expect("checked term source form"), code, message.to_owned());
            }
            todo.extend_from_slice(self.terms.children(term));
        }
    }

    fn pure_helper(
        &mut self,
        cx: &Cx,
        mapper: bool,
        code: &str,
        message: &str,
        form: TermId,
        name: packed::NameId,
        visited: &mut BTreeSet<String>,
        todo: &mut Vec<TermId>,
    ) {
        let name = self.terms.name(name);
        if !visited.insert(name.to_owned()) {
            return;
        }
        if let Some(body) = self.def_terms_get(name).map(|definition|definition.body.clone()) {
            todo.push(self.terms.import(&body));
            return;
        }
        let operator = op::find_indexed(&self.ops, name, &cx.ctx);
        if operator.is_some_and(|operator| {
            operator.ctx == context::value()
                && (!mapper || !operator.live)
                && !BODY_CONTEXT_OPS.contains(&operator.name.as_str())
        }) {
            return;
        }
        add_to(&mut self.diags, Severity::ErrorSeverity, self.terms.source_form(form).expect("checked term source form"), code, message.to_owned());
    }

    fn check_node_cycles(&mut self) {
        let definitions = self.base.iter().flat_map(|base|base.nodes.iter()).chain(self.nodes.iter()).map(|node| (node.kind.qualified.clone(), node.clone())).collect::<BTreeMap<_, _>>();
        fn visit(
            checker: &mut Checker,
            name: &str,
            stack: &mut Vec<String>,
            visited: &mut BTreeSet<String>,
            definitions: &BTreeMap<String, Arc<NodeDefinition>>,
        ) {
            if let Some(position) = stack.iter().position(|entry| entry == name) {
                if let Some(node) = definitions.get(name) {
                    let mut cycle = stack[position..].to_vec();
                    cycle.push(name.to_owned());
                    let form = node.body.form.clone();
                    checker.error(&form, "E_RECURSION", &format!("Recursive node call: {}.", cycle.join(" → ")));
                }
                return;
            }
            if !visited.insert(name.to_owned()) {
                return;
            }
            let Some(node) = definitions.get(name) else { return };
            stack.push(name.to_owned());
            for called in kind_calls(&node.body.body) {
                if definitions.contains_key(&called) {
                    visit(checker, &called, stack, visited, definitions);
                }
            }
            stack.pop();
        }
        let mut visited = BTreeSet::new();
        for name in self.base.iter().flat_map(|base|base.node_order.iter()).chain(self.node_order.iter()).cloned().collect::<Vec<_>>() {
            visit(self, &name, &mut Vec::new(), &mut visited, &definitions);
        }
    }

    fn phase(&mut self, forms: impl IntoIterator<Item = Syntax>) -> (Vec<String>, Vec<String>) {
        let registration_phase = flow_syntax::phase_timer::measure("check.register");
        let mut declarations = Vec::new();
        let mut seen = BTreeSet::new();
        let mut graph_names = Vec::new();
        let mut def_names = Vec::new();
        let mut node_names = Vec::new();
        for form in forms {
            let form = &form;
            self.metadata(form, &[]);
            let SNode::List(items) = &form.node else {
                self.error(form, "E_FORM", "Workspace children are graph, defn, defnode and defmacro forms.");
                continue;
            };
            let items = form.range(*items);
            let Some((SNode::Sym(head), SNode::Sym(name))) = items.first().zip(items.get(1)).map(|(head, name)| (form.arena().row(*head).node, form.arena().row(*name).node)) else {
                self.error(form, "E_FORM", "Workspace children are graph, defn, defnode and defmacro forms.");
                continue;
            };
            let head = form.name(head);
            let name = form.name(name);
            if !["graph", "defn", "defnode", "defmacro"].contains(&head) {
                self.error(form, "E_FORM", "Workspace children are graph, defn, defnode and defmacro forms.");
                continue;
            }
            let valid = if head == "defnode" { defnode::valid_name(name) } else { macros::valid_name(name) };
            let duplicate_kind = head == "defnode" && self.catalog.kinds.iter().any(|kind| kind.qualified == name);
            let operator_name = op::all().iter().chain(self.ops.iter()).any(|operator| operator.ctx == context::value() && operator.name == name);
            let already_declared = self.declared_names_contains(name);
            if !valid || self.reserved(name) || !seen.insert(name.to_owned()) || already_declared || duplicate_kind || operator_name {
                self.error(form, "E_NAME", &format!("Invalid, reserved or duplicate name: {name}."));
                continue;
            }
            self.declared_names.insert(name.to_owned());
            declarations.push((head.to_owned(), name.to_owned(), form.clone()));
        }

        for (head, name, form) in &declarations {
            if head != "defnode" {
                continue;
            }
            match defnode::parse(form) {
                Ok(definition) => {
                    if !self.catalog.kinds.iter().any(|kind| kind.qualified == definition.kind.qualified) {
                        Rc::make_mut(&mut self.catalog).kinds.push(definition.kind.clone());
                    }
                    self.node_defs.insert(name.clone(), definition);
                    self.node_order.push(name.clone());
                    node_names.push(name.clone());
                }
                Err(problem) => self.diags.push(problem),
            }
        }

        drop(registration_phase);
        let macro_phase = flow_syntax::phase_timer::measure("check.macros");
        for (head, name, form) in &declarations {
            if head != "defmacro" {
                continue;
            }
            self.diags.extend(macros::check(form, &|is_head, symbol| self.known_macro_name(is_head, symbol)));
            if macros::params(form).is_ok() {
                self.macros.insert(name.clone(), form.clone());
                self.macro_forms.push(form.clone());
            }
        }

        drop(macro_phase);
        let signature_phase = flow_syntax::phase_timer::measure("check.signatures");
        for (head, name, form) in &declarations {
            if head != "graph" && head != "defn" {
                continue;
            }
            let Some(signature) = self.parse_signature(form, head, name, &declarations) else { continue };
            if head == "graph" {
                self.graphs.insert(name.clone(), signature);
                self.graph_order.push(name.clone());
                graph_names.push(name.clone());
            } else {
                self.defs.insert(name.clone(), signature);
                self.def_order.push(name.clone());
                def_names.push(name.clone());
            }
        }

        drop(signature_phase);
        let function_phase = flow_syntax::phase_timer::measure("check.functions");
        for name in &def_names {
            self.check_def_body(name, !self.call_sites);
        }
        drop(function_phase);

        let node_phase = flow_syntax::phase_timer::measure("check.nodes");
        for name in node_names {
            let Some(definition) = self.node_defs_get(&name).cloned() else { continue };
            self.events += 1;
            let outer_terms = std::mem::take(&mut self.terms);
            let outer_calls = std::mem::take(&mut self.calls);
            let graph_form = self.terms.import_form(&definition.form);
                   let bindings = definition.inputs.iter().map(|(input, ty, _)| (input.clone(), Info::leaf(ty.clone()))).collect::<BTreeMap<_, _>>();
            let base = Cx {
                env: ScopeStack::from_map(bindings),
                ctx: context::sop(),
                path: self.path(vec![format!("node:{name}").into()]),
                scope: name.clone(),
                in_def: true,
                stack: vec![name.clone()],
                depth: 0,
                zone: None,
                zbody: false,
            };
            let mut inputs = Vec::new();
            for (input, input_ty, default) in &definition.inputs {
                let term = default.as_ref().map(|form| {
                    let mut cx = base.clone();
                    cx.env = ScopeStack::default();
                    cx.path=self.child_path(cx.path,format!(":{input}").into());
                    let (term, value) = self.infer(&cx, form);
                    if !ty::fits(&value.ty, input_ty) {
                        self.error(form, "E_TYPE", &format!("Invalid default for {input}."));
                    }
                    term
                });
                inputs.push((input.clone(), input_ty.clone(), term));
            }
            let (body, body_value) = self.body(&base, &definition.body);
            if !ty::fits(&body_value.ty, &ty::geometry()) {
                self.error(&definition.body, "E_TYPE", &format!("{name} must return geometry."));
            }
            let mut validations = Vec::new();
            for validation in &definition.validations {
                let (term, value) = self.infer(&base, &validation.predicate);
                if self.contains_body_context_op(term) {
                    self.error(&validation.predicate, "E_NODE_CONTEXT", "A validation checks parameters; context seed is available inside the body.");
                }
                if value.ty != Ty::Bool {
                    self.error(&validation.predicate, "E_TYPE", "A validation predicate returns bool.");
                }
                validations.push((term, validation.code.clone(), validation.message.clone()));
            }
            let mut normalizers = Vec::new();
            for (input, form) in &definition.normalizers {
                let mut cx = base.clone();
                cx.ctx = context::value();
                cx.env = ScopeStack::default();
                let (term, value) = self.fn_value(&cx, form, "normalizer");
                let signature = FnSignature { params: vec![Ty::Text], result: Ty::Text };
                let argument = Arg { key: None, form: form.clone(), term, info: value };
                let normalized = self.instantiate_fn(&cx, &argument, &signature, false);
                if normalized.info.ty != Ty::Fn(Some(Box::new(signature))) {
                    self.error(form, "E_TYPE", "An encoded normalizer takes one text and returns text.");
                }
                self.pure_boundary(&cx, true, "E_PARAM_NORMALIZE", "An encoded normalizer is pure: no frame, state, context or cooked values.", normalized.term);
                normalizers.push((input.clone(), normalized.term));
            }
            let mut roots = inputs.iter().filter_map(|(_, _, default)| *default).collect::<Vec<_>>();
            roots.push(body);
            roots.extend(validations.iter().map(|(term, _, _)| *term));
            roots.extend(normalizers.iter().map(|(_, term)| *term));
            let (arena, roots, forms) = self.finish_terms(outer_terms, outer_calls, &roots, graph_form);
            let mut roots = roots.into_iter();
            let inputs = inputs.into_iter().map(|(name, ty, default)| {
                (name, ty, default.map(|_| arena.handle(roots.next().unwrap())))
            }).collect();
            let body = arena.handle(roots.next().unwrap());
            let validations = validations.into_iter().map(|(_, code, message)| {
                (arena.handle(roots.next().unwrap()), code, message)
            }).collect();
            let normalizers = normalizers.into_iter().map(|(input, _)| {
                (input, arena.handle(roots.next().unwrap()))
            }).collect();
            self.nodes.push(Arc::new(NodeDefinition {
                kind: definition.kind,
                slots: definition.slots,
                keyword_inputs: definition.keyword_inputs,
                fields: definition.fields,
                label: definition.label,
                category: definition.category,
                description: definition.description,
                operation: definition.operation,
                version: definition.version,
                body: Graph { name: name.clone(), context: context::sop(), inputs, body, form: arena.syntax().handle(forms[0]) },
                validations,
                normalizers,
            }));
        }
        drop(node_phase);
        self.check_node_cycles();
        for name in &graph_names {
            if let Some(signature) = self.graphs_get(name) {
                let form = signature.form.clone();
                self.graph_info(&form, name);
            }
        }
        (graph_names, self.base.iter().flat_map(|base| base.def_order.iter()).chain(self.def_order.iter()).cloned().collect())
    }
}

fn literal_param(terms: &TermBuilder, mut term: TermId) -> Option<param::Value> {
    loop {
        return match terms.node(term) {
            PackedNode::Lit(id) => Some(match terms.literal(id) {
                packed::Literal::Bool(value) => param::Value::Bool(value), packed::Literal::Int(value) => param::Value::Int(value),
                packed::Literal::Float(value) => param::Value::Float(value), packed::Literal::Text(id) => param::Value::Text(terms.shared_raw_bytes(id)),
                packed::Literal::Choice(name) => param::Value::Choice(terms.name(name).to_owned()),
            }),
            PackedNode::Text(id) => Some(param::Value::Text(terms.shared_raw_bytes(id))),
            PackedNode::Expanded(id) => { term = terms.expanded(id).body; continue; }
            _ => None,
        };
    }
}
fn catalog_term(terms: &TermBuilder, term: TermId) -> catalog::Term {
    let ty = ty_port(terms.ty(term));
    let node = match terms.node(term) {
        PackedNode::Lit(_) | PackedNode::Text(_) => catalog::TermNode::Literal(literal_param(terms, term).unwrap()),
        PackedNode::Nil => catalog::TermNode::Nil,
        PackedNode::Vec(range) => catalog::TermNode::Vector(terms.term_ids(range).iter().map(|&component| {
            let mut component = catalog_term(terms, component);
            if component.ty.is_none() { component.ty = Some(port_type::PortType::Float); }
            component
        }).collect()),
        _ => catalog::TermNode::Reference("_".into(), "_".into()),
    };
    catalog::Term { node, ty }
}
fn catalog_term_of(term: &packed::Term) -> catalog::Term {
    let arena = term.arena();
    let ty = ty_port(term.ty());
    let node = match term.node {
        PackedNode::Lit(id) => catalog::TermNode::Literal(match arena.literal(id) {
            packed::Literal::Bool(value) => param::Value::Bool(value), packed::Literal::Int(value) => param::Value::Int(value),
            packed::Literal::Float(value) => param::Value::Float(value), packed::Literal::Text(id) => param::Value::Text(arena.shared_bytes(id)),
            packed::Literal::Choice(name) => param::Value::Choice(term.name(name).to_owned()),
        }),
        PackedNode::Text(id) => catalog::TermNode::Literal(param::Value::Text(arena.shared_bytes(id))),
        PackedNode::Nil => catalog::TermNode::Nil,
        PackedNode::Vec(range) => catalog::TermNode::Vector(arena.term_ids(range).iter().map(|&id| {
            let mut component = catalog_term_of(&term.child(id));
            if component.ty.is_none() { component.ty = Some(port_type::PortType::Float); }
            component
        }).collect()),
        _ => catalog::TermNode::Reference("_".into(), "_".into()),
    };
    catalog::Term { node, ty }
}
fn validate_parameter_id(ty_override: Option<Ty>, report: &mut dyn FnMut(Severity, &str, &str),
    parameter: &catalog::Parameter, terms: &TermBuilder, term: TermId) {
    let text = match terms.node(term) { PackedNode::Text(id) => Some(terms.raw_bytes(id)), _ => None };
    validate_parameter_value(ty_override.as_ref().unwrap_or_else(|| terms.ty(term)), report, parameter, text, &|| catalog_term(terms, term));
}
pub fn validate_parameter(ty_override: Option<Ty>, report: &mut dyn FnMut(Severity, &str, &str),
    parameter: &catalog::Parameter, term: &packed::Term) {
    let text = match term.node { PackedNode::Text(id) => Some(term.arena().bytes(id)), _ => None };
    validate_parameter_value(ty_override.as_ref().unwrap_or_else(|| term.ty()), report, parameter, text, &|| catalog_term_of(term));
}
fn validate_parameter_value(have: &Ty, report: &mut dyn FnMut(Severity, &str, &str),
    parameter: &catalog::Parameter, text: Option<&[u8]>, data: &dyn Fn() -> catalog::Term) {
    let want = parameter_ty(parameter);
    if *have == Ty::Any { return; }
    if !ty::fits(have, &want) {
        report(Severity::ErrorSeverity, "E_TYPE", &format!(":{} takes {}, but this is {}", parameter.name, show(&want), show(have)));
        return;
    }
    match (&want, text) {
        (Ty::Color, Some(bytes)) => {
            if !hex_colour(bytes) {
                report(Severity::ErrorSeverity, "E_TYPE", &format!(":{} is a colour: write \"#rrggbb\" or a vec3, not {}", parameter.name, lisp::quote_string(bytes)));
            }
        }
        (Ty::Color, None) => {
            if *have == Ty::Vec3 {
                let mut typed = parameter.clone();
                typed.ty = Some(port_type::PortType::Vec3);
                catalog::validate_parameter(report, &typed, &data());
            }
        }
        (Ty::Vec3, _) if *have != Ty::Vec3 => (),
        (Ty::Text, Some(_)) => catalog::validate_parameter(report, parameter, &data()),
        (Ty::Text, None) => (),
        _ => catalog::validate_parameter(report, parameter, &data()),
    }
}

pub fn prelude(
    ops: &[op::Op],
    library: bool,
    call_sites: bool,
    catalog: &catalog::Catalog,
    forms: &[Syntax],
) -> Prelude {
    let mut checker = Checker::new(Rc::new(catalog.clone()), Arc::new(ops.to_vec()), library, call_sites);
    checker.children = forms.len();
    checker.phase(forms.iter().cloned());
    checker.count += forms.len();
    checker.inherited = Rc::new(std::mem::take(&mut checker.kept));
    Prelude { checker: Rc::new(checker) }
}

pub fn check(
    ops: &[op::Op],
    library: bool,
    call_sites: bool,
    prelude: Option<&Prelude>,
    catalog: &catalog::Catalog,
    forms: &[Syntax],
) -> (Option<Workspace>, Vec<Diagnostic>) {
    let _phase = flow_syntax::phase_timer::measure("check");
    if let Some(problem) = op::validate(ops) {
        return (None, vec![problem]);
    }
    let mut checker = if let Some(prelude) = prelude {
        let mut checker = Checker::fork(prelude.checker.clone());
        checker.ops = if op::same_declarations(&checker.ops, ops) { checker.ops.clone() } else { Arc::new(ops.to_vec()) };
        checker.library = library;
        checker.call_sites = call_sites;
        checker
    } else {
        Checker::new(Rc::new(catalog.clone()), Arc::new(ops.to_vec()), library, call_sites)
    };
    let workspace_form = match forms {
        [] => return (None, vec![err("E_NO_WORKSPACE", "Expected (workspace name …).".into(), None)]),
        [form] => form,
        [_, form, ..] => return (None, vec![err("E_ONE_WORKSPACE", "A file holds one workspace form.".into(), Some(form.span.clone()))]),
    };
    let SNode::List(items) = &workspace_form.node else {
        return (None, vec![err("E_WORKSPACE", "Expected (workspace name …).".into(), Some(workspace_form.span.clone()))]);
    };
    let items = workspace_form.range(*items);
    checker.children = items.len().saturating_sub(2);
    if items.first().and_then(|id| match workspace_form.arena().row(*id).node { SNode::Sym(name) => Some(workspace_form.name(name)), _ => None }) != Some("workspace") {
        checker.error(workspace_form, "E_WORKSPACE", "Expected (workspace name …).");
        return (None, checker.diags);
    }
    let Some(SNode::Sym(name)) = items.get(1).map(|id| workspace_form.arena().row(*id).node) else {
        checker.error(workspace_form, "E_WORKSPACE", "Expected (workspace name …).");
        return (None, checker.diags);
    };
    let name = workspace_form.name(name);
    if !macros::valid_name(name) {
        checker.error(workspace_form, "E_WORKSPACE", "Expected (workspace name …).");
        return (None, checker.diags);
    }
    checker.metadata(workspace_form, &["allow-warnings"]);
    let children = &items[2..];
    let (graph_names, def_names) = checker.phase(children.iter().map(|id| workspace_form.child(*id)));
    if !library && graph_names.is_empty() {
        checker.error(workspace_form, "E_NO_GRAPH", "A workspace needs at least one graph.");
    }
    let mut diagnostics = Vec::new();
    for problem in std::mem::take(&mut checker.diags) {
        if !diagnostics.iter().any(|old: &Diagnostic| {
            old.code == problem.code && old.span == problem.span && old.message == problem.message
        }) {
            diagnostics.push(problem);
        }
    }
    if diagnostics.iter().any(|problem| problem.severity == Severity::ErrorSeverity) {
        return (None, diagnostics);
    }
    let base = checker.base.as_deref();
    let packed = base.into_iter().flat_map(|base| base.packed.iter()).chain(checker.packed.iter()).map(|(path, form)| {
        let form = match form {
            CheckPackedForm::Candidate(paths) => PackedForm::Candidate(Arc::new(paths.iter().map(|path|checker.path_names(*path).to_vec()).collect())),
            CheckPackedForm::Refused(diagnostics) => PackedForm::Refused(diagnostics.clone()),
        };
        (checker.path_names(*path).to_vec(), form)
    }).collect::<BTreeMap<_,_>>().into_iter().collect::<Vec<_>>();
    let live = base.into_iter().flat_map(|base|base.live.iter()).chain(checker.live.iter()).map(|path|checker.path_names(*path).to_vec()).collect();
    let invariant = base.into_iter().flat_map(|base|base.invariant.iter()).chain(checker.invariant.iter()).map(|path|checker.path_names(*path).to_vec()).collect();
    let mut packed_roots = base.into_iter().flat_map(|base|base.roots.iter()).chain(checker.roots.iter()).map(|(form,path)|(form.clone(),checker.path_names(*path).to_vec())).collect::<Vec<_>>();
    let nodes=base.into_iter().flat_map(|base|base.nodes.iter()).chain(checker.nodes.iter()).cloned().collect::<Vec<_>>().into();
    let macros=base.into_iter().flat_map(|base|base.macro_forms.iter()).chain(checker.macro_forms.iter()).cloned().collect::<Vec<_>>().into();
    let kind_fns=base.into_iter().flat_map(|base|base.kind_fns.iter()).chain(checker.kind_fns.iter()).map(|(name,kind)|(name.clone(),kind.clone())).collect::<BTreeMap<_,_>>().into_iter().collect();
    let approx_reasons: Vec<_> = packed.iter().map(|(path, form)| {
        let reasons = match form {
            PackedForm::Refused(reasons) => reasons.clone(),
            PackedForm::Candidate(_) => vec![err("E_PACKED_PENDING", "Actual producer captures have not been qualified.".into(), None)],
        };
        (path.clone(), reasons)
    }).collect();
    packed_roots.sort_by(|(a, p),(b, q)| p.cmp(q).then(a.id.cmp(&b.id)));
    let workspace = Workspace {
        name: name.to_owned(),
        graphs: graph_names.iter().filter_map(|name| checker.graph_terms.remove(name).or_else(||checker.base.as_ref()?.graph_terms.get(name).cloned())).collect::<Vec<_>>().into(),
        defs: def_names.iter().filter_map(|name| checker.def_terms.remove(name).or_else(||checker.base.as_ref()?.def_terms.get(name).cloned())).collect::<Vec<_>>().into(),
        nodes,
        macros,
        source: forms.to_vec().into(),
        live,
        invariant,
        approx: Paths::new(),
        packed: packed.into(),
        packed_roots,
        approx_reasons: approx_reasons.into(),
        kind_fns,
        ops: checker.ops.into(),
    };
    (Some(workspace), diagnostics)
}
#[cfg(test)]
mod memory_tests {
    use super::*;

    #[test]
    fn checker_paths_keep_their_owner_when_term_builders_change() {
        let mut base = Checker::new(Rc::new(catalog::Catalog { version: 0, kinds: Vec::new() }), Arc::new(Vec::new()), false, false);
        let inherited = base.path(vec!["base".into()]);
        let base = Rc::new(base);
        let mut left = Checker::fork(base.clone());
        let mut right = Checker::fork(base.clone());
        let l = left.path(vec!["left".into()]);
        let r = right.path(vec!["right".into()]);
        assert_eq!(l, r, "local indices can coincide in independent owners");
        left.terms = TermBuilder::new();
        left.terms.path(&["unrelated builder path".into()]);
        assert_eq!(&**left.path_names(l), &["left"]);
        assert_eq!(&**right.path_names(r), &["right"]);
        assert_eq!(left.path(vec!["base".into()]), inherited);
        assert_eq!(base.paths.len(), 1, "a fork cannot grow the cached prelude");
        assert_eq!(left.paths.len(), 1, "inherited paths are shared, not copied");
    }

    #[test]
    fn workspace_and_prelude_resolve_ids_in_their_own_syntax_arenas() {
        let catalog = catalog::Catalog { version: 0, kinds: Vec::new() };
        let forms = syntax::parse("(defn identity :context value [(x : int)] x)").unwrap();
        let prelude = prelude(&[], false, false, &catalog, &forms);
        drop(forms);
        let forms = syntax::parse("(workspace example (graph result :context value (identity 7)))").unwrap();
        let (workspace, diagnostics) = check(&[], false, false, Some(&prelude), &catalog, &forms);
        let workspace = workspace.unwrap_or_else(|| panic!("{diagnostics:?}"));
        assert_eq!(workspace.name, "example");
        assert_eq!(workspace.graphs[0].name, "result");
        assert_eq!(workspace.defs[0].name, "identity");
        for source in ["()", "(workspace)", "(workspace 7)", "(workspace :bad)", "(other w)"] {
            let forms = syntax::parse(source).unwrap();
            let (workspace, diagnostics) = check(&[], false, false, None, &catalog, &forms);
            assert!(workspace.is_none(), "{source}");
            assert!(diagnostics.iter().any(|d| d.code == "E_WORKSPACE"), "{source}: {diagnostics:?}");
        }
    }

    #[test]
    fn repeated_call_sites_check_to_fresh_equal_bodies() {
        let source = "(workspace w
          (defn sq :context value [(x : float)] (* x x))
          (defn bump :context value [(x : float)] (+ 1.0 (let* [y (* x 2.0)] y)))
          (graph g :context value (+ (+ (sq 1.0) (sq 2.0)) (+ (bump 1.0) (bump 2.0)))))";
        let catalog = catalog::Catalog { version: 0, kinds: Vec::new() };
        let (workspace, diagnostics) = check(&[], false, false, None, &catalog, &syntax::parse(source).unwrap());
        let workspace = workspace.unwrap_or_else(|| panic!("{diagnostics:?}"));
        let mut bodies: BTreeMap<String, Vec<Term>> = BTreeMap::new();
        let mut todo = vec![workspace.graphs[0].body.clone()];
        while let Some(term) = todo.pop() {
            if let PackedNode::CallFn(id) = term.node {
                let call = term.arena().call_fn(id);
                bodies.entry(term.name(call.name).to_owned()).or_default().push(term.child(call.body.unwrap()));
            }
            todo.extend(term.children().iter().map(|&child| term.child(child)));
        }
        let (sq, bump) = (&bodies["sq"], &bodies["bump"]);
        // a memoised call-site body comes back as a copy: equal terms, never a shared row
        assert!(sq.len() == 2 && sq[0] == sq[1] && sq[0].identity() != sq[1].identity());
        // a body that numbers its inline scopes is checked again, so each call keeps its own path
        let shown = |term: &Term| format!("{term:?}");
        assert!(bump.len() == 2 && shown(&bump[0]).contains("~let") && shown(&bump[0]) != shown(&bump[1]), "{}", shown(&bump[1]));
    }

    #[test]
    fn workspace_clones_share_packed_metadata_and_edits_keep_the_original() {
        let forms = syntax::parse("(workspace w (graph g :context host (array/count (map (fn [(x : float)] (+ x 1.0)) (array/float 8 2.0)))))").unwrap();
        let catalog = catalog::Catalog { version: 0, kinds: Vec::new() };
        let (workspace, diagnostics) = check(&[], false, false, None, &catalog, &forms);
        let workspace = workspace.unwrap_or_else(|| panic!("{diagnostics:?}"));
        assert!(!workspace.packed.is_empty(), "the fixture has packed candidates");
        let mut edited = workspace.clone();
        assert!(Arc::ptr_eq(&workspace.packed, &edited.packed), "snapshot cloning does not copy candidate paths");
        assert!(!workspace.approx_reasons.is_empty());
        assert!(Arc::ptr_eq(&workspace.approx_reasons, &edited.approx_reasons), "snapshot cloning does not copy refusal diagnostics");
        let before = workspace.packed.as_ref().clone();
        for ((_, original), (_, copy)) in workspace.packed.iter().zip(&before) {
            if let (PackedForm::Candidate(original), PackedForm::Candidate(copy)) = (original, copy) {
                assert!(Arc::ptr_eq(original, copy), "cloning prelude entries shares producer paths");
            }
        }
        assert!(before.iter().any(|(_, form)| matches!(form, PackedForm::Candidate(_))), "the fixture has public producer paths");
        let producers = &Arc::new([0usize].into_iter().collect::<BTreeSet<CheckPath>>());
        let mut candidate = CheckPackedForm::Candidate(producers.clone());
        candidate = Checker::merge_packed(candidate, CheckPackedForm::Candidate(producers.clone()));
        let CheckPackedForm::Candidate(shared) = &candidate else { unreachable!() };
        assert!(Arc::ptr_eq(producers, shared));
        let original = producers.as_ref().clone();
        candidate = Checker::merge_packed(candidate, CheckPackedForm::Candidate(Arc::new([1usize].into_iter().collect())));
        let CheckPackedForm::Candidate(merged) = candidate else { unreachable!() };
        assert!(merged.contains(&1usize));
        assert!(original.is_subset(&merged));
        assert_eq!(producers.as_ref(), &original, "merging leaves cached producer sets intact");
        Arc::make_mut(&mut edited.packed).clear();
        assert_eq!(workspace.packed.as_ref(), &before, "changing a snapshot leaves the original metadata intact");
        let reasons = workspace.approx_reasons.as_ref().clone();
        Arc::make_mut(&mut edited.approx_reasons).clear();
        assert_eq!(workspace.approx_reasons.as_ref(), &reasons, "changing a snapshot leaves the original refusal diagnostics intact");
    }

    #[test]
    fn retained_memory_counts_capacities_and_shared_workspace_roots_once() {
        let forms = syntax::parse("(workspace w (graph g :context host (let* [x {:field [1 2]}] (get x :field))))").unwrap();
        let catalog = catalog::Catalog { version: 0, kinds: Vec::new() };
        let (workspace, diagnostics) = check(&[], false, false, None, &catalog, &forms);
        let workspace = Arc::new(workspace.unwrap_or_else(|| panic!("{diagnostics:?}")));
        let mut memory = Memory::default();
        memory.workspace(&workspace);
        let before = (memory.syntax.bytes(), memory.checked_bytes());
        assert!(before.0 > std::mem::size_of::<Syntax>());
        assert!(before.1 > std::mem::size_of::<Workspace>() + std::mem::size_of::<Graph>());
        memory.workspace(&workspace);
        assert_eq!((memory.syntax.bytes(), memory.checked_bytes()), before);
        let cloned = Arc::new((*workspace).clone());
        memory.workspace(&cloned);
        assert_eq!(memory.syntax.bytes(), before.0, "workspace clones share syntax allocations");
        assert!(memory.checked_bytes() > before.1, "the separate workspace value is retained too");

        let mut text = Vec::with_capacity(128);
        text.push(b'x');
        let text: Arc<[u8]> = text.into();
        let allocation = (text.as_ptr() as usize, text.len() + 2 * std::mem::size_of::<usize>());
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&syntax::make(syntax::Draft::Sym("x".into())));
        let root = builder.push(None, &Ty::Text, form, Draft::Text(text));
        let arena = builder.freeze();
        let mut memory = Memory::default();
        memory.term(&arena.handle(root));
        let before = (memory.syntax.bytes(), memory.checked_bytes());
        memory.term(&arena.handle(root));
        assert_eq!((memory.syntax.bytes(), memory.checked_bytes()), before, "separate handles share one arena allocation");
        let (_, checked) = memory.into_allocations();
        assert!(checked.contains(&allocation), "shared immutable text payload and Arc header are retained");
    }

    #[test]
    fn packed_skip_tuples_keep_integer_and_nonempty_tuple_rules() {
        for (source, expected) in [
            ("[]", Some(vec![])),
            ("[0 [1 2] 3]", Some(vec![vec![0], vec![1, 2], vec![3]])),
            ("[[]]", None), ("[-1]", None), ("[1.0]", None),
            ("[9223372036854775808]", None), ("[name]", None),
        ] {
            let form = syntax::parse(source).unwrap().remove(0);
            assert_eq!(skip_tuples(&form), expected, "{source}");
        }
    }

    #[test]
    fn packed_bypass_selects_authored_positional_argument_not_body() {
        let source = syntax::parse("(f :option 9 2)").unwrap().remove(0);
        let forms = source.children();
        let mut builder = TermBuilder::new();
        let option_form = builder.import_form(&source.child(forms[2]));
        let option = builder.push(None, &Ty::Int, option_form, Draft::Lit(param::Value::Int(9)));
        let input_form = builder.import_form(&source.child(forms[3]));
        let input = builder.push(None, &Ty::Int, input_form, Draft::Lit(param::Value::Int(2)));
        let form = builder.import_form(&source);
        let call = builder.push(None, &Ty::Int, form, Draft::Op {
            op: "f".into(), args: vec![("option".into(), option), ("value".into(), input)], skip: Vec::new(),
        });
        let body_only = builder.push(None, &Ty::Int, form, Draft::CallFn { fn_name: "f".into(), args: Vec::new(), body: Some(input) });
        let arena = builder.freeze();
        assert_eq!(bypass_input(&arena.handle(call)).unwrap().row_id(), input);
        assert!(bypass_input(&arena.handle(body_only)).is_none());
    }
}
