use crate::{catalog, defnode, op, port_type};
use flow_syntax::{context, lisp, macros, syntax::{self, Node as SNode, Syntax}, ty::{self, FnSignature, Ty}, value::Value};
use param::{self, FieldView, KindView};
use rays_diagnostic::{self as diagnostic, Diagnostic, Severity, Span};
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

trait SyntaxArguments {
    fn node_as_args(&self) -> &[Syntax];
}

impl SyntaxArguments for Syntax {
    fn node_as_args(&self) -> &[Syntax] {
        match &self.node {
            SNode::List(forms) if !forms.is_empty() => &forms[1..],
            _ => &[],
        }
    }
}

pub type Path = Vec<String>;
pub type Paths = BTreeSet<Path>;
pub type Context = context::Context;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern { Name(String), Seq(Vec<Pattern>), Keys(Vec<String>) }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticPolicy { First, RangeLast }
#[derive(Clone, Debug, PartialEq)]
pub struct Term { pub path: Option<Path>, pub ty: Ty, pub node: Node, pub form: Syntax }
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Lit(param::Value), Text(Vec<u8>), Nil, Time, Vec(Vec<Term>), RefBinding(String, Vec<String>),
    Call { kind: String, ctx: Context, args: Vec<(String, Term)> },
    Op { op: String, args: Vec<(String, Term)>, skip: Vec<Vec<usize>> },
    CallFn { fn_name: String, args: Vec<Term>, body: Option<Box<Term>> }, FnRef(String),
    GraphRef { graph: String, inputs: Vec<(String, Term)> }, Let(Vec<(Pattern, Term)>, Box<Term>),
    State { binder: Pattern, init: Box<Term>, step: Box<Term>, zone: Path },
    Loop { kind: LoopKind, accs: Vec<(Pattern, Term)>, clauses: Vec<(Pattern, Term)>, skip: Vec<Vec<usize>>, body: Box<Term>, zone: Path },
    WithError(Box<Term>, Box<Term>), ErrorRows { rows: Box<Term>, policy: DiagnosticPolicy, mapper: Box<Term> },
    If(Box<Term>, Box<Term>, Box<Term>), Cond(Vec<(Term, Term)>, Box<Term>),
    Case(Box<Term>, Vec<(Syntax, Term)>, Box<Term>),
    Fn { params: Vec<(Pattern, Option<Ty>)>, body: Box<Term>, zone: Path, capture: Option<Box<Term>> },
    Hof(HofKind, Vec<Term>), ListLit(Vec<Term>), Record(Vec<(String, Term)>), Get(Box<Term>, String),
    Assoc(Box<Term>, Vec<(String, Term)>), Str(Vec<Term>), ListOp(String, Vec<Term>), Bypass(Box<Term>),
    Expanded { macro_name: String, body: Box<Term> },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopKind { For, Fold, Scan, Sum }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HofKind { Map, Filter, Reduce, SortBy, Iterate }

fn fields(xs: &[(String, Term)], out: &mut Vec<Term>) { out.extend(xs.iter().map(|(_, t)| t.clone())); }
pub fn children(term: &Term) -> Vec<Term> {
    let mut out = Vec::new();
    match &term.node {
        Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::RefBinding(_, _) | Node::FnRef(_) => (),
        Node::Vec(ts) | Node::Hof(_, ts) | Node::ListLit(ts) | Node::Str(ts) | Node::ListOp(_, ts) => out.extend(ts.iter().cloned()),
        Node::Call { args, .. } | Node::Op { args, .. } | Node::GraphRef { inputs: args, .. } | Node::Record(args) => fields(args, &mut out),
        Node::CallFn { args, body, .. } => { out.extend(args.iter().cloned()); if let Some(t) = body { out.push((**t).clone()); } },
        Node::Let(bs, b) => { for (_, t) in bs { out.push(t.clone()); } out.push((**b).clone()); },
        Node::State { init, step, .. } => { out.push((**init).clone()); out.push((**step).clone()); },
        Node::Loop { accs, clauses, body, .. } => { for (_, t) in accs.iter().chain(clauses) { out.push(t.clone()); } out.push((**body).clone()); },
        Node::WithError(a,b) => { out.push((**a).clone()); out.push((**b).clone()); },
        Node::ErrorRows { rows, mapper, .. } => { out.push((**rows).clone()); out.push((**mapper).clone()); },
        Node::If(a,b,c) => { out.push((**a).clone()); out.push((**b).clone()); out.push((**c).clone()); },
        Node::Cond(arms,d) => { for (a,b) in arms { out.push(a.clone()); out.push(b.clone()); } out.push((**d).clone()); },
        Node::Case(v,arms,d) => { out.push((**v).clone()); out.extend(arms.iter().map(|(_,t)| t.clone())); out.push((**d).clone()); },
        Node::Fn { body, capture, .. } => { out.push((**body).clone()); if let Some(t) = capture { out.push((**t).clone()); } },
        Node::Get(t,_) | Node::Bypass(t) | Node::Expanded { body:t, .. } => out.push((**t).clone()),
        Node::Assoc(t,fs) => { out.push((**t).clone()); fields(fs,&mut out); },
    }
    out
}
pub fn map_children(f: &dyn Fn(&Term) -> Term, term: &Term) -> Term {
    fn mapped(fs: &[(String, Term)], f: &dyn Fn(&Term)->Term) -> Vec<(String,Term)> { fs.iter().map(|(n,t)|(n.clone(),f(t))).collect() }
    let node = match &term.node {
        Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::RefBinding(_,_) | Node::FnRef(_) => term.node.clone(),
        Node::Vec(ts) => Node::Vec(ts.iter().map(f).collect()), Node::Hof(k,ts) => Node::Hof(*k,ts.iter().map(f).collect()),
        Node::ListLit(ts) => Node::ListLit(ts.iter().map(f).collect()), Node::Str(ts) => Node::Str(ts.iter().map(f).collect()),
        Node::ListOp(n,ts) => Node::ListOp(n.clone(),ts.iter().map(f).collect()), Node::Call { kind,ctx,args } => Node::Call{kind:kind.clone(),ctx:ctx.clone(),args:mapped(args,f)},
        Node::Op{op,args,skip} => Node::Op{op:op.clone(),args:mapped(args,f),skip:skip.clone()},
        Node::CallFn{fn_name,args,body} => Node::CallFn{fn_name:fn_name.clone(),args:args.iter().map(f).collect(),body:body.as_ref().map(|t|Box::new(f(t)))},
        Node::GraphRef{graph,inputs} => Node::GraphRef{graph:graph.clone(),inputs:mapped(inputs,f)},
        Node::Let(bs,b) => Node::Let(bs.iter().map(|(p,t)|(p.clone(),f(t))).collect(),Box::new(f(b))),
        Node::State{binder,init,step,zone} => Node::State{binder:binder.clone(),init:Box::new(f(init)),step:Box::new(f(step)),zone:zone.clone()},
        Node::Loop{kind,accs,clauses,skip,body,zone} => Node::Loop{kind:*kind,accs:accs.iter().map(|(p,t)|(p.clone(),f(t))).collect(),clauses:clauses.iter().map(|(p,t)|(p.clone(),f(t))).collect(),skip:skip.clone(),body:Box::new(f(body)),zone:zone.clone()},
        Node::WithError(a,b)=>Node::WithError(Box::new(f(a)),Box::new(f(b))),
        Node::ErrorRows{rows,policy,mapper}=>Node::ErrorRows{rows:Box::new(f(rows)),policy:*policy,mapper:Box::new(f(mapper))},
        Node::If(a,b,c)=>Node::If(Box::new(f(a)),Box::new(f(b)),Box::new(f(c))),
        Node::Cond(arms,d)=>Node::Cond(arms.iter().map(|(a,b)|(f(a),f(b))).collect(),Box::new(f(d))),
        Node::Case(v,arms,d)=>Node::Case(Box::new(f(v)),arms.iter().map(|(s,t)|(s.clone(),f(t))).collect(),Box::new(f(d))),
        Node::Fn{params,body,zone,capture}=>Node::Fn{params:params.clone(),body:Box::new(f(body)),zone:zone.clone(),capture:capture.as_ref().map(|t|Box::new(f(t)))},
        Node::Record(fs)=>Node::Record(mapped(fs,f)),Node::Get(t,n)=>Node::Get(Box::new(f(t)),n.clone()),
        Node::Assoc(t,fs)=>Node::Assoc(Box::new(f(t)),mapped(fs,f)),Node::Bypass(t)=>Node::Bypass(Box::new(f(t))),
        Node::Expanded{macro_name,body}=>Node::Expanded{macro_name:macro_name.clone(),body:Box::new(f(body))},
    };
    Term{path:term.path.clone(),ty:term.ty.clone(),node,form:term.form.clone()}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Graph { pub name:String, pub context:Context, pub inputs:Vec<(String,Ty,Option<Term>)>, pub body:Term, pub form:Syntax }
#[derive(Clone, Debug, PartialEq)]
pub struct NodeDefinition { pub kind:catalog::Kind, pub slots:Vec<catalog::Slot>, pub keyword_inputs:Vec<String>, pub fields:Vec<FieldView>, pub label:String, pub category:Vec<String>, pub description:Option<String>, pub operation:String, pub version:i64, pub body:Graph, pub validations:Vec<(Term,String,String)>, pub normalizers:Vec<(String,Term)> }
#[derive(Clone, Debug, PartialEq)]
pub enum PackedForm { Candidate(Paths), Refused(Vec<Diagnostic>) }
#[derive(Clone)]
pub struct Workspace {
    pub name:String, pub graphs:Vec<Graph>, pub defs:Vec<Graph>, pub nodes:Vec<NodeDefinition>, pub macros:Vec<Syntax>, pub source:Vec<Syntax>,
    pub live:Paths, pub invariant:Paths, pub approx:Paths, pub packed:Vec<(Path,PackedForm)>, pub packed_roots:Vec<(Syntax,Path)>,
    pub approx_reasons:Vec<(Path,Vec<Diagnostic>)>, pub kind_fns:Vec<(String,(String,Context,Vec<catalog::Slot>))>, pub ops:Vec<op::Op>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpSignature { pub pos:Vec<(String,Ty)>,pub opt:Vec<(String,Ty)>,pub rest:Option<(String,Ty)>,pub kw:Vec<(String,Ty)> }
pub type op_signature = OpSignature;
#[derive(Clone)]
pub struct Prelude { checker: Checker }
pub const MAX_ITERATIONS:usize=4096;
pub const BODY_CONTEXT_OPS:&[&str]=&["sop/context_seed","sop/provenance_base","sop/context_frame","sop/context_time"];
const SPECIAL:&[&str]=&["workspace","graph","defn","defnode","defmacro","let*","ref","for","fold","scan","sum","state","if","with_error","error_rows","values","fn","cond","case","list","concat","str","get","assoc","map","filter","reduce","sort-by","iterate","quote","quasiquote","unquote","unquote-splicing"];
pub fn special_forms()-> &'static [&'static str] { SPECIAL }
pub fn context_name(ctx:&Context)->String { context::name(*ctx).to_owned() }
pub fn max_iterations()->usize { MAX_ITERATIONS }
pub fn value_ops()->Vec<String> { op::of_context(&[],&context::value()).into_iter().map(|o|o.name.clone()).collect() }
pub fn op_signature(ops:&[op::Op],ctx:&Context,name:&str)->Option<OpSignature>{op::find(ops,name,ctx).map(|o|OpSignature{pos:o.signature.pos.clone(),opt:o.signature.opt.clone(),rest:o.signature.rest.clone(),kw:o.signature.kw.clone()})}
pub fn op_choices(ops:&[op::Op],name:&str,argument:&str)->Vec<String>{op::find(ops,name,&context::value()).and_then(|o|o.choices.iter().find(|(n,_)|n==argument).map(|(_,v)|v.clone())).unwrap_or_default()}
pub fn name_taken(name:&str)->bool { SPECIAL.contains(&name) || flow_syntax::symbol::reserved(name) || ty::names().iter().any(|(n,_)|n==name) || op::find(&[],name,&context::value()).is_some() }

pub fn skip_tuples(form:&Syntax)->Option<Vec<Vec<usize>>>{
    fn number(x:&Syntax)->Option<usize>{ if let SNode::Num(s)=&x.node { let n=s.parse::<i64>().ok()?; if n>=0 {usize::try_from(n).ok()}else{None} }else{None} }
    fn tuple(x:&Syntax)->Option<Vec<usize>>{match &x.node{SNode::Vec(xs) if !xs.is_empty()=>xs.iter().map(number).collect(),_=>number(x).map(|n|vec![n])}}
    match &form.node { SNode::Vec(xs)=>xs.iter().map(tuple).collect(),_=>None }
}

pub fn kind_calls(term:&Term)->Vec<String>{let mut out=BTreeSet::new();let mut stack=vec![term.clone()];while let Some(t)=stack.pop(){if let Node::Call{kind,..}=&t.node{out.insert(kind.clone());}stack.extend(children(&t));}out.into_iter().collect()}
pub fn bypass_input_ref(term:&Term)->Option<&Term>{
    let entries:Vec<&Term>=match &term.node{Node::Call{args,..}|Node::Op{args,..}=>args.iter().map(|(_,t)|t).collect(),Node::CallFn{args,..}=>args.iter().collect(),_=>Vec::new()};
    match &term.form.node{
        SNode::List(xs) if !xs.is_empty()=>{
            let mut rest=&xs[1..];
            let first=loop{match rest{[Syntax{node:SNode::Kw(_),..},_,tail@..]=>rest=tail,[input,..]=>break input,[]=>return None}};
            entries.into_iter().find(|t|t.form.id==first.id&&t.form.span==first.span)
        }
        _=>entries.into_iter().next(),
    }
}
pub fn bypass_input(term:&Term)->Option<Term>{ bypass_input_ref(term).cloned() }

pub fn slot_ty(slot:Option<&catalog::Slot>,kind:&catalog::Kind)->Ty { if let Some(t)=slot.and_then(|s|s.ty.as_ref()){return t.clone()} match kind.qualified.as_str(){"scene/world"=>ty::world(),"scene/root"=>ty::scene(),_ if kind.context==context::scene()=>ty::geometry(),_=>context::result(kind.context)} }
pub fn group_reader(p:&catalog::Parameter)->bool { p.name=="group" }
pub fn group_writer(k:&catalog::Kind,p:&catalog::Parameter)->bool { p.name=="name"&&k.qualified.starts_with("sop/group_") }

fn err(code:&str,message:String,span:Option<Span>)->Diagnostic { diagnostic::error(code.to_owned(),message,span,None,None,Vec::new(),Vec::new()) }
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
        Ty::Named(name) => ty::shape(ty),
        Ty::List(element) => shape_ty(element),
        Ty::Record(fields) => fields.iter().any(|(_, ty)| shape_ty(ty)),
        _ => false,
    }
}

fn cooked(ty: &Ty) -> bool {
    ty::is_cooked(ty)
}

fn make_term(form: &Syntax, ty: Ty, node: Node) -> Term {
    Term { path: None, ty, node, form: form.clone() }
}

fn set_path(mut term: Term, path: Path) -> Term {
    term.path = Some(path);
    term
}

fn path_text(path: &[String]) -> String {
    path.join("/")
}

fn marker(path: &[String], name: &str) -> Path {
    let mut result = path.to_vec();
    result.push(name.to_owned());
    result
}

fn num_param(source: &str) -> Option<param::Value> {
    let number = syntax::number_value(source)?;
    if !number.is_finite() {
        return None;
    }
    if !source.contains('.') && !source.contains('e') && !source.contains('E') {
        if let Ok(value) = source.parse::<i64>() {
            const MIN: i64 = -(1_i64 << 62);
            const MAX: i64 = (1_i64 << 62) - 1;
            if (MIN..=MAX).contains(&value) {
                return Some(param::Value::Int(value));
            }
        }
    }
    Some(param::Value::Float(number))
}

fn opaque_head(name: &str) -> bool {
    [
        "fn", "map", "filter", "reduce", "sort-by", "iterate", "if", "cond", "case",
        "with_error", "error_rows", "exact",
    ]
    .contains(&name)
}

fn list_fields(ty: &Ty) -> Vec<Vec<String>> {
    match ty {
        Ty::List(_) => vec![Vec::new()],
        Ty::Record(fields) => fields
            .iter()
            .flat_map(|(name, ty)| {
                list_fields(ty).into_iter().map(|mut path| {
                    path.insert(0, name.clone());
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
fn packed_source(ty: &Ty) -> bool {
    matches!(ty, Ty::Any)
        || matches!(ty, Ty::Array(element) if **element == Ty::Any)
        || packed_array(ty)
}

fn packed_body(ops: &[op::Op], term: &Term) -> bool {
    let packed = |name: &str| op::find(ops, name, &context::value()).and_then(|operator| op::packed_kind(&operator));
    let body = |term: &Term| packed_body(ops, term);
    match &term.node {
        Node::Lit(param::Value::Int(_) | param::Value::Float(_) | param::Value::Bool(_)) | Node::Time => true,
        Node::Text(_) => true,
        Node::RefBinding(_, _) => term.ty == Ty::Text || flow_syntax::packed_ops::scalar_width(&term.ty).is_some(),
        Node::Vec(items) => matches!(items.len(), 2 | 3 | 4) && items.iter().all(body),
        Node::Get(value, field) => {
            let scalar_field = match &value.ty {
                Ty::Vec2 => ["x", "y"].contains(&field.as_str()),
                Ty::Vec3 => ["x", "y", "z"].contains(&field.as_str()),
                Ty::Vec4 => ["x", "y", "z", "w"].contains(&field.as_str()),
                Ty::Record(fields) => fields.iter().any(|(name, _)| name == field),
                _ => false,
            };
            scalar_field && body(value)
        }
        Node::Record(fields) => fields.iter().all(|(_, value)| body(value)),
        Node::CallFn { args, body: Some(specialized), .. } => args.iter().all(body) && body(specialized),
        Node::Op { op: name, args, .. } => {
            let Some(capability) = packed(name) else { return false };
            match capability {
                op::PackedKind::Frame => args.is_empty() && matches!(&term.ty, Ty::Float | Ty::Int | Ty::Bool),
                op::PackedKind::Noise3 => args.iter().all(|(key, value)| match (key.as_str(), &value.node) {
                    ("position", _) => body(value),
                    ("seed", Node::Lit(param::Value::Int(_))) => true,
                    ("octaves", Node::Lit(param::Value::Int(value))) => flow_syntax::packed_ops::supported_noise_octaves(*value),
                    ("lacunarity" | "gain", Node::Lit(param::Value::Float(_))) => true,
                    _ => false,
                }),
                op::PackedKind::Length | op::PackedKind::Normalize => {
                    matches!(args.as_slice(), [(_, value)] if value.ty == Ty::Vec3 && body(value))
                }
                op::PackedKind::Dot | op::PackedKind::Cross => {
                    args.len() == 2 && args.iter().all(|(_, value)| value.ty == Ty::Vec3 && body(value))
                }
                op::PackedKind::Exact => matches!(args.as_slice(), [(_, value)] if body(value)),
                op::PackedKind::Fma | op::PackedKind::CheckedFma => args.len() == 3 && args.iter().all(|(_, value)| body(value)),
                op::PackedKind::Trace => args.len() == 6 && args.iter().all(|(_, value)| body(value)),
                op::PackedKind::Stage => args.iter().all(|(_, value)| body(value)),
                op::PackedKind::CheckedMul => args.len() == 2 && args.iter().all(|(_, value)| body(value)),
                op::PackedKind::Gather => matches!(args.as_slice(), [(_, array), (_, index)] if (array.ty == Ty::Array(Box::new(Ty::Text)) || packed_array(&array.ty)) && body(index)),
                op::PackedKind::Binary(_) | op::PackedKind::Integer(_) | op::PackedKind::IntegerCompare(_) | op::PackedKind::RandAt => {
                    args.len() == 2 && args.iter().all(|(_, value)| body(value))
                }
                op::PackedKind::Unary(_) | op::PackedKind::Convert(_) => args.len() == 1 && args.iter().all(|(_, value)| body(value)),
                op::PackedKind::ConstantOnly => false,
            }
        }
        Node::Let(bindings, result) => bindings.iter().all(|(pattern, value)| matches!(pattern, Pattern::Name(_)) && body(value)) && body(result),
        Node::If(test, yes, no) => body(test) && body(yes) && body(no),
        Node::Cond(arms, default) => body(default) && arms.iter().all(|(test, value)| body(test) && body(value)),
        Node::Case(scrutinee, arms, default) => {
            body(scrutinee) && body(default) && arms.iter().all(|(literal, value)| {
                let literal = matches!(&literal.node, SNode::Num(_) | SNode::Str(_))
                    || matches!(&literal.node, SNode::Sym(name) if name == "true" || name == "false");
                literal && body(value)
            })
        }
        Node::Expanded { body: expanded, .. } => body(expanded),
        Node::Bypass(call) => bypass_input(call).is_some_and(|input| {
            (input.ty == term.ty
                || (term.ty == Ty::Float && matches!(&input.ty, Ty::Int | Ty::Bool))
                || (matches!(&term.ty, Ty::Vec2 | Ty::Vec3 | Ty::Vec4) && matches!(&input.ty, Ty::Int | Ty::Float)))
                && body(&input)
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
    packed: Paths,
    approx: bool,
    approx_sources: Paths,
    state_dep: bool,
    list_fields: Vec<Vec<String>>,
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
            packed: Paths::new(),
            approx: false,
            approx_sources: Paths::new(),
            state_dep: false,
            list_fields: Vec::new(),
        }
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

#[derive(Clone)]
enum Callable{Local(Rc<Closure>),Def(String),Op(String),Kind(catalog::Kind)}
#[derive(Clone)]
struct Closure{params:Vec<(Syntax,Option<Ty>)>,body:Syntax,env:BTreeMap<String,Info>,ctx:Context,path:Path,name:String}
#[derive(Clone)]
struct Sig{name:String,ctx:Context,params:Vec<(String,Ty,Option<Syntax>)>,body:Syntax,form:Syntax}
#[derive(Clone)]
struct Cx{env:BTreeMap<String,Info>,ctx:Context,path:Path,scope:String,in_def:bool,stack:Vec<String>,depth:usize,zone:Option<usize>,zbody:bool}
#[derive(Clone)]
struct Arg{key:Option<String>,form:Syntax,term:Term,info:Info}

#[derive(Clone)]
struct Checker {
    catalog: catalog::Catalog,
    ops: Vec<op::Op>,
    library: bool,
    call_sites: bool,
    diags: Vec<Diagnostic>,
    live: Paths,
    invariant: Paths,
    packed: BTreeMap<Path, PackedForm>,
    roots: Vec<(Syntax, Path)>,
    defs: BTreeMap<String, Sig>,
    graphs: BTreeMap<String, Sig>,
    macros: BTreeMap<String, Syntax>,
    macro_forms: Vec<Syntax>,
    node_defs: BTreeMap<String, defnode::Defnode>,
    nodes: Vec<NodeDefinition>,
    graph_terms: BTreeMap<String, Graph>,
    ginfo: BTreeMap<String, Info>,
    def_terms: BTreeMap<String, Graph>,
    kind_fns: BTreeMap<String, (String, Context, Vec<catalog::Slot>)>,
    inline: BTreeMap<Path, usize>,
    input_paths: BTreeMap<(Path, String, i64), Path>,
    checked_defs: BTreeSet<String>,
    graph_order: Vec<String>,
    def_order: Vec<String>,
    node_order: Vec<String>,
    declared_names: BTreeSet<String>,
    steps: usize,
    zone_id: usize,
    graph_stack: Vec<String>,
}

impl Checker {
    fn new(catalog: catalog::Catalog, ops: &[op::Op], library: bool, call_sites: bool) -> Self {
        Self {
            catalog,
            ops: ops.to_vec(),
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
            zone_id: 0,
            graph_stack: Vec::new(),
        }
    }
 fn add(&mut self,sev:Severity,form:&Syntax,code:&str,msg:String){let span=if form.span.start==0&&form.span.finish==0{None}else{Some(form.span.clone())};let d=match sev{Severity::ErrorSeverity=>err(code,msg,span),Severity::Warning=>diagnostic::warning(span,code.to_owned(),msg)};self.diags.push(d)}
 fn error(&mut self,f:&Syntax,c:&str,m:&str){self.add(Severity::ErrorSeverity,f,c,m.to_owned())}
 fn warn(&mut self,f:&Syntax,c:&str,m:&str){self.add(Severity::Warning,f,c,m.to_owned())}
 fn warn_owned(&mut self,f:&Syntax,c:&str,m:String){self.add(Severity::Warning,f,c,m)}
 fn inline_path(&mut self,cx:&Cx,seg:&str)->Path{let mut key=cx.path.clone();key.push(seg.to_owned());let n=*self.inline.get(&key).unwrap_or(&0);self.inline.insert(key.clone(),n+1);if n==0{key}else{let mut p=cx.path.clone();p.push(format!("{seg}~{n}"));p}}
 fn merge_packed(old: PackedForm, next: PackedForm) -> PackedForm {
     match (old, next) {
         (PackedForm::Candidate(mut left), PackedForm::Candidate(right)) => {
             left.extend(right);
             PackedForm::Candidate(left)
         }
         (PackedForm::Refused(mut left), PackedForm::Refused(right)) => {
             for diagnostic in right {
                 if !left.iter().any(|old| old.code == diagnostic.code && old.span == diagnostic.span && old.message == diagnostic.message) {
                     left.push(diagnostic);
                 }
             }
             PackedForm::Refused(left)
         }
         (refusal @ PackedForm::Refused(_), _) => refusal,
         (candidate @ PackedForm::Candidate(_), PackedForm::Refused(_)) => candidate,
     }
 }

 fn mark(&mut self, path: Path, value: &Info) {
     if value.live {
         self.live.insert(path.clone());
     }
     if !value.packed.is_empty() {
         let next = PackedForm::Candidate(value.packed.clone());
         self.packed.entry(path).and_modify(|old| *old = Self::merge_packed(old.clone(), next.clone())).or_insert(next);
     }
 }

 fn add_root(&mut self, form: &Syntax, path: Path, refusal: Option<Diagnostic>) {
     if !self.roots.iter().any(|(old, owner)| old.id == form.id && owner == &path) {
         self.roots.push((form.clone(), path.clone()));
     }
     let next = match refusal {
         Some(reason) => PackedForm::Refused(vec![reason]),
         None => PackedForm::Candidate([path.clone()].into_iter().collect()),
     };
     self.packed.entry(path.clone()).and_modify(|old| *old = Self::merge_packed(old.clone(), next.clone())).or_insert(next);
     let producer = PackedForm::Candidate([path.clone()].into_iter().collect());
     self.packed.entry(path).and_modify(|old| *old = Self::merge_packed(old.clone(), producer.clone())).or_insert(producer);
 }

 fn fail_term(&mut self, form: &Syntax, code: &str, message: &str) -> (Term, Info) {
     self.error(form, code, message);
     (make_term(form, Ty::Any, Node::Nil), Info::leaf(Ty::Any))
 }

 fn exact_sink(&mut self, cx: &Cx, form: &Syntax, sink: &str, value: &Info) {
     let mut sources = value.approx_sources.clone();
     if value.approx && sources.is_empty() {
         sources.insert(cx.path.clone());
     }
     if !sources.is_empty() {
         let producers = sources.iter().map(|path| path_text(path)).collect::<Vec<_>>().join(", ");
         self.error(form, "E_APPROX_SINK", &format!("Approximate values require (exact x) before catalog calls, exports, state or cache keys. Producer: {producers}; sink: {} ({sink}).", path_text(&cx.path)));
     }
 }

 fn infer(&mut self, cx: &Cx, form: &Syntax) -> (Term, Info) {
     self.steps += 1;
     if self.steps > 400_000 {
         return self.fail_term(form, "E_BUDGET", "The workspace is too large to check: call-site typing exceeded its budget. Reduce nested function calls.");
     }
     let mut result = if !form.meta.is_empty() {
         if let Some(metadata) = form.meta.iter().find(|name| name.as_str() != "bypass") {
             self.fail_term(form, "E_META", &format!("Unknown metadata ^:{metadata}. The only metadata is ^:bypass."))
         } else {
             self.bypass(cx, form)
         }
     } else {
         self.infer_plain(cx, form)
     };
     if result.1.approx && result.1.approx_sources.is_empty() {
         result.1.approx_sources.insert(cx.path.clone());
     }
     result
 }

 fn infer_plain(&mut self, cx: &Cx, form: &Syntax) -> (Term, Info) {
     match &form.node {
         SNode::Num(raw) => match num_param(raw) {
             Some(param::Value::Int(value)) => (make_term(form, Ty::Int, Node::Lit(param::Value::Int(value))), Info::leaf(Ty::Int)),
             Some(param::Value::Float(value)) => (make_term(form, Ty::Float, Node::Lit(param::Value::Float(value))), Info::leaf(Ty::Float)),
             _ => self.fail_term(form, "E_TYPE", "Nonfinite number."),
         },
         SNode::Str(bytes) => (make_term(form, Ty::Text, Node::Text(bytes.clone())), Info::leaf(Ty::Text)),
         SNode::Kw(key) => self.fail_term(form, "E_KEYWORD", &format!("Keyword :{key} has no call to belong to.")),
         SNode::Sym(name) => self.symbol(cx, form, name),
         SNode::Vec(items) => self.vector(cx, form, items),
         SNode::Map(items) => self.record(cx, form, items, "A record"),
         SNode::Quote(_, _) => self.fail_term(form, "E_QUOTE", "Quotes belong in a defmacro template (`…)."),
         SNode::List(items) if items.is_empty() => self.fail_term(form, "E_EXPR", "Expected an expression."),
         SNode::List(items) => match &items[0].node {
             SNode::Sym(head) => self.call(cx, form, head, &items[1..]),
             _ => self.fail_term(form, "E_CALL", "A call head is a name."),
         },
     }
 }

 fn bypass(&mut self, cx: &Cx, form: &Syntax) -> (Term, Info) {
     let SNode::List(items) = &form.node else {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {}: only operator and function calls pass an input through.", lisp::flat(form)));
     };
     let Some(SNode::Sym(head)) = items.first().map(|item| &item.node) else {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {}: only operator and function calls pass an input through.", lisp::flat(form)));
     };
     if head == "if" && items.len() == 4 {
         let mut plain = form.clone();
         plain.meta.clear();
         let (_, checked) = self.infer_plain(cx, &plain);
         let (mut branch, mut value) = self.branch(cx, "then", &items[2]);
         branch.ty = checked.ty.clone();
         branch.form = form.clone();
         value.ty = checked.ty;
         return (branch, value);
     }
     if SPECIAL.contains(&head.as_str()) || self.macros.contains_key(head) {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {head}: only operator and function calls pass an input through."));
     }
     let args = &items[1..];
     let first_positional = { let mut i = 0; loop { match args.get(i) { None => break false, Some(Syntax { node: SNode::Kw(_), .. }) if i + 1 < args.len() => i += 2, Some(_) => break true } } };
    if !first_positional {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {head}: it has no positional input to pass through."));
     }
     let mut plain = form.clone();
     plain.meta.clear();
     let (call, value) = self.infer_plain(cx, &plain);
     let Some(input) = bypass_input(&call) else {
         return self.fail_term(form, "E_BYPASS", &format!("can't bypass {head}: only operator and function calls pass an input through."));
     };
     if !ty::fits(&input.ty, &value.ty) || ty::has_fn(&input.ty) {
         self.error(form, "E_BYPASS", &format!("can't bypass {head}: its input is {} but it returns {}.", show(&input.ty), show(&value.ty)));
     }
     (make_term(form, value.ty.clone(), Node::Bypass(Box::new(call))), value)
 }
 fn symbol(&mut self,cx:&Cx,f:&Syntax,s:&str)->(Term,Info){match s{"t"=>{let mut v=Info::leaf(Ty::Float);v.live=true;(make_term(f,Ty::Float,Node::Time),v)},"pi"=>(make_term(f,Ty::Float,Node::Lit(param::Value::Float(std::f64::consts::PI))),Info::leaf(Ty::Float)),"true"|"false"=>(make_term(f,Ty::Bool,Node::Lit(param::Value::Bool(s=="true"))),Info::leaf(Ty::Bool)),"nil"=>(make_term(f,ty::geometry(),Node::Nil),Info::leaf(ty::geometry())),_=>{
     let mut parts=s.split('.');let name=parts.next().unwrap_or(s);let fs=parts.map(str::to_owned).collect::<Vec<_>>();
     if let Some(mut v)=cx.env.get(name).cloned(){let mut t=make_term(f,v.ty.clone(),Node::RefBinding(name.into(),fs.clone()));for field in &fs{v=self.field(f,&v,field,name);}t.ty=v.ty.clone();(t,v)}else if !fs.is_empty()&&["t","pi","true","false","nil"].contains(&name){self.fail_term(f,"E_FIELD",&format!("{name} is a constant; it has no .{}.",fs.join(".")))}else{self.error(f,"E_UNBOUND",&format!("{name} is not bound in {}.{}",cx.scope,if cx.env.is_empty(){String::new()}else{format!(" Bound here: {}.",cx.env.keys().cloned().collect::<Vec<_>>().join(", "))}));self.fail_term_no_diag(f)}
   }}}
 fn fail_term_no_diag(&self,f:&Syntax)->(Term,Info){(make_term(f,Ty::Any,Node::Nil),Info::leaf(Ty::Any))}
 fn field(&mut self, form: &Syntax, value: &Info, name: &str, base: &str) -> Info {
     let ty = match &value.ty {
         Ty::Any => Ty::Any,
         Ty::Vec2 if ["x", "y"].contains(&name) => Ty::Float,
         Ty::Vec3 if ["x", "y", "z"].contains(&name) => Ty::Float,
         Ty::Vec4 if ["x", "y", "z", "w"].contains(&name) => Ty::Float,
         Ty::Record(fields) => match fields.iter().find(|(field, _)| field == name) {
             Some((_, ty)) => ty.clone(),
             None => {
                 self.error(form, "E_FIELD", &format!("{base} has no field {name}. Fields: {}.", if fields.is_empty() { "none".into() } else { fields.iter().map(|(field, _)| field.clone()).collect::<Vec<_>>().join(", ") }));
                 Ty::Any
             }
         },
         _ => {
             self.error(form, "E_FIELD", "Value has no such output. A vec3 has .x .y .z; a record has its fields.");
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
 fn vector(&mut self,cx:&Cx,f:&Syntax,xs:&[Syntax])->(Term,Info){let ty=match xs.len(){2=>Ty::Vec2,3=>Ty::Vec3,4=>Ty::Vec4,_=>{return self.fail_term(f,"E_VECTOR",&format!("A vector has 2, 3 or 4 components; this one has {}.",xs.len()))}};let mut ts=Vec::new();let mut vs=Vec::new();for x in xs{let(t,v)=self.infer(cx,x);if !matches!(v.ty,Ty::Int|Ty::Float|Ty::Any){self.error(x,"E_TYPE",&format!("Vector components are numbers; got {}.",show(&v.ty)));}ts.push(t);vs.push(v);}let v=Info::derive(ty.clone(),&vs);(make_term(f,ty,Node::Vec(ts)),v)}
 fn record(&mut self, cx: &Cx, form: &Syntax, items: &[Syntax], what: &str) -> (Term, Info) {
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
         let key = &pair[0];
         let SNode::Kw(name) = &key.node else {
             self.error(key, "E_RECORD", &format!("{what} keys are keywords like :size; got {}.", lisp::flat(key)));
             valid = false;
             continue;
         };
         if !seen.insert(name.clone()) {
             self.error(key, "E_RECORD", &format!("{what} gives :{name} twice."));
             valid = false;
             continue;
         }
         let (term, value) = self.infer(cx, &pair[1]);
         self.no_fn(&pair[1], &value.ty, &format!("Record field :{name}"));
         fields.push((name.clone(), term.ty.clone()));
         terms.push((name.clone(), term));
         if value.live_len {
             list_fields.push(vec![name.clone()]);
         }
         list_fields.extend(value.list_fields.iter().map(|path| {
             let mut path = path.clone();
             path.insert(0, name.clone());
             path
         }));
         values.push(value);
     }
     if !valid {
         return (make_term(form, Ty::Any, Node::Nil), Info::leaf(Ty::Any));
     }
     list_fields.sort();
     list_fields.dedup();
     let ty = Ty::Record(fields);
     let mut value = Info::derive(ty.clone(), &values);
     value.list_fields = list_fields;
     value.live_len = value.list_fields.iter().any(Vec::is_empty);
     (make_term(form, ty, Node::Record(terms)), value)
 }
 fn no_fn(&mut self,f:&Syntax,t:&Ty,what:&str){if ty::has_fn(t){self.error(f,"E_FN_ESCAPES",&format!("{what} is a function; a function value cannot be stored or returned (E_FN_ESCAPES). Call it where it is bound."))}}
 fn input(&mut self, cx: &Cx, form: &Syntax, key: &str) -> (Term, Info) {
     let head = syntax::head(form).unwrap_or("");
     if !head.contains('/') && !self.macros.contains_key(head) && !opaque_head(head) {
         return self.infer(cx, form);
     }
     let memo = (cx.path.clone(), key.to_owned(), form.id);
     let path = if let Some(path) = self.input_paths.get(&memo) {
         path.clone()
     } else {
         let mut stem = cx.path.clone();
         let leaf = stem.pop().unwrap_or_else(|| "@result".into());
         stem.push(format!("{leaf}#{key}"));
         let count = self.inline.entry(stem.clone()).or_insert(0);
         let path = if *count == 0 {
             stem
         } else {
             let mut unique = stem;
             if let Some(leaf) = unique.last_mut() {
                 *leaf = format!("{leaf}~{}", *count);
             }
             unique
         };
         *count += 1;
         self.input_paths.insert(memo, path.clone());
         path
     };
     if opaque_head(head) {
         return self.binding(cx, form, path);
     }
     let mut inner = cx.clone();
     inner.path = path.clone();
     let (term, value) = self.infer(&inner, form);
     self.mark(path.clone(), &value);
     let term = if term.ty == ty::drawing() || term.ty == ty::image() {
         set_path(term, path)
     } else {
         term
     };
     (term, value)
 }
 fn bind(&mut self, cx: &Cx, pattern: &Syntax, value: Info, marker_text: &str, env: &mut BTreeMap<String, Info>) {
     if !matches!(&pattern.node, SNode::Sym(_)) {
         let mut path = cx.path.clone();
         path.push(format!("{marker_text}{}", Self::pattern_key(pattern)));
         self.mark(path, &value);
     }
     match &pattern.node {
         SNode::Sym(name) => {
             let mut path = cx.path.clone();
             path.push(format!("{marker_text}{name}"));
             self.mark(path, &value);
             env.insert(name.clone(), value);
         }
         SNode::Vec(patterns) => {
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
             for pattern in patterns {
                 self.bind(cx, pattern, part.clone(), marker_text, env);
             }
         }
         SNode::Map(items) if items.len() == 2 && matches!(&items[0].node, SNode::Kw(key) if key == "keys") => {
             if !matches!(&value.ty, Ty::Record(_) | Ty::Any) {
                 self.error(pattern, "E_PATTERN", &format!("{} destructures a record; got {}.", Self::pattern_key(pattern), show(&value.ty)));
             }
             if let SNode::Vec(keys) = &items[1].node {
                 for key in keys {
                     if let SNode::Sym(name) = &key.node {
                         let field = self.field(key, &value, name, "pattern");
                         self.bind(cx, key, field, marker_text, env);
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

 fn check_pattern(&mut self, cx: &Cx, pattern: &Syntax, seen: &mut BTreeSet<String>, _env: &BTreeMap<String, Info>, what: &str) -> bool {
     let zone = matches!(what, "for" | "fold" | "scan" | "sum" | "state");
     let shape = |p: &Syntax| if zone {
         format!("{what} binds names; {} is not a name or pattern.", Self::pattern_key(p))
     } else {
         format!("Invalid binding name {}. Bind a name, [a b] or {{:keys [a b]}}.", Self::pattern_key(p))
     };
     match &pattern.node {
         SNode::Sym(name) => self.check_name(cx, pattern, name, seen, what, zone),
         SNode::Vec(items) if !items.is_empty() => {
             let mut ok = true;
             for item in items { ok &= self.check_pattern(cx, item, seen, _env, what); }
             ok
         }
         SNode::Map(items) if items.len() == 2 && matches!(&items[0].node, SNode::Kw(k) if k == "keys") && matches!(&items[1].node, SNode::Vec(ks) if !ks.is_empty()) => {
             let SNode::Vec(keys) = &items[1].node else { unreachable!() };
             let mut ok = true;
             for key in keys {
                 match &key.node {
                     SNode::Sym(name) => ok &= self.check_name(cx, key, name, seen, what, zone),
                     _ => { ok = false; let m = shape(key); self.error(key, "E_PATTERN", &m); }
                 }
             }
             ok
         }
         _ => { let m = shape(pattern); self.error(pattern, "E_PATTERN", &m); false }
     }
 }
 fn pattern(p:&Syntax)->Pattern{match &p.node{SNode::Sym(n)=>Pattern::Name(n.clone()),SNode::Vec(xs)=>Pattern::Seq(xs.iter().map(Self::pattern).collect()),SNode::Map(xs) if xs.len()==2=>if let SNode::Vec(ks)=&xs[1].node{Pattern::Keys(ks.iter().filter_map(|x|if let SNode::Sym(n)=&x.node{Some(n.clone())}else{None}).collect())}else{Pattern::Name("_".into())},_=>Pattern::Name("_".into())}}
 fn path_term(&mut self,cx:&Cx,f:&Syntax,term:Term,v:Info)->(Term,Info){let mut t=term;t.path=Some(cx.path.clone());self.mark(cx.path.clone(),&v);(t,v)}
 fn binding(&mut self, cx: &Cx, x: &Syntax, id: Path) -> (Term, Info) {
     let mut inner = cx.clone();
     inner.path = id.clone();
     let head = match &x.node { SNode::List(items) if x.meta.is_empty() => match items.first().map(|h| &h.node) { Some(SNode::Sym(h)) => Some((h.as_str(), &items[1..])), _ => None }, _ => None };
     let (mut term, mut value) = match head {
         Some((h @ ("for" | "fold" | "scan" | "sum"), args)) => self.zone(&inner, x, h, args, Some(id.clone())),
         Some(("let*", args)) => self.let_scope(&inner, x, args, id.clone()),
         Some(("state", args)) => self.state(&inner, x, args, Some(id.clone())),
         Some(("fn", args)) => self.make_fn(&inner, x, args, "fn", Some(id.clone())),
         _ => self.infer(&inner, x),
     };
     if value.approx && value.approx_sources.is_empty() { value.approx_sources.insert(id.clone()); }
     self.mark(id.clone(), &value);
     term.path = Some(id);
     (term, value)
 }
 fn result_in(&mut self, cx: &Cx, form: &Syntax, base: &Path) -> (Term, Info) {
     let path = marker(base, "@result");
     let (term, value) = self.binding(cx, form, path);
     self.no_fn(form, &value.ty, &format!("The result of {}", cx.scope));
     (term, value)
 }
 fn body(&mut self, cx: &Cx, form: &Syntax) -> (Term, Info) {
     if let SNode::List(items) = &form.node {
         if items.first().is_some_and(|head| matches!(&head.node, SNode::Sym(name) if name == "let*"))
             && form.meta.is_empty()
         {
             return self.let_scope(cx, form, &items[1..], cx.path.clone());
         }
     }
     let path = marker(&cx.path, "@result");
     let (term, value) = self.binding(cx, form, path);
     self.no_fn(form, &value.ty, &format!("The result of {}", cx.scope));
     (term, value)
 }

 fn branch_test(&mut self, cx: &Cx, key: &str, form: &Syntax) -> (Term, Info) {
     let mut path = cx.path.clone();
     if let Some(last) = path.pop() {
         path.push(format!("{last}#{key}"));
     } else {
         path.push(format!("@result#{key}"));
     }
     self.binding(cx, form, path)
 }

 fn branch(&mut self, cx: &Cx, key: &str, form: &Syntax) -> (Term, Info) {
     let mut path = cx.path.clone();
     if let Some(last) = path.pop() {
         path.push(format!("{last}#{key}"));
     } else {
         path.push(format!("@result#{key}"));
     }
     let mut inner = cx.clone();
     inner.path = path.clone();
     let (mut term, value) = self.body(&inner, form);
     if syntax::head(form) != Some("let*") {
         term = make_term(form, value.ty.clone(), Node::Let(Vec::new(), Box::new(term)));
     }
     term.path = Some(path.clone());
     self.mark(path, &value);
     (term, value)
 }
 fn call(&mut self,cx:&Cx,f:&Syntax,name:&str,args:&[Syntax])->(Term,Info){match name{
   "let*"=>{let base=self.inline_path(cx,"~let");self.let_scope(cx,f,args,base)},"state"=>self.state(cx,f,args,None),"for"|"fold"|"scan"|"sum"=>self.zone(cx,f,name,args,None),"fn"=>self.make_fn(cx,f,args,"fn",None),
   "if"=>self.if_form(cx,f,args),"cond"=>self.cond_form(cx,f,name,args),"case"=>self.cond_form(cx,f,name,args),"values"=>self.record(cx,f,args,"values"),"list"=>self.list(cx,f,args),"concat"=>self.concat(cx,f,args),"str"=>self.str_form(cx,f,args),"get"=>self.get(cx,f,args),"assoc"=>self.assoc(cx,f,args),"map"|"filter"|"reduce"|"sort-by"=>self.hof(cx,f,name,args),"iterate"=>self.iterate(cx,f,args),"ref"=>self.ref_form(cx,f,args),"with_error"=>self.with_error(cx,f,args),"error_rows"=>self.error_rows(cx,f,args),
   "quote"|"quasiquote"|"unquote"|"unquote-splicing"=>self.fail_term(f,"E_QUOTE",&format!("{name} belongs in a defmacro template (`…).")),"workspace"|"graph"|"defn"|"defnode"|"defmacro"=>self.fail_term(f,"E_FORM",&format!("{name} is a top-level form of a workspace.")),
   _=>self.call_named(cx,f,name,args)
 }}
 fn let_scope(&mut self,cx:&Cx,f:&Syntax,args:&[Syntax],base:Path)->(Term,Info){if args.len()!=2{return self.fail_term(f,"E_LET","let* needs [name expression …] and one result.")}let SNode::Vec(bs)=&args[0].node else{return self.fail_term(f,"E_LET","let* needs [name expression …] and one result.")};if bs.len()%2!=0{return self.fail_term(f,"E_LET","let* needs [name expression …] and one result.")}let mut env=cx.env.clone();let mut out=Vec::new();let mut seen=BTreeSet::new();for pair in bs.chunks_exact(2){let p=&pair[0];if !self.check_pattern(cx,p,&mut seen,&env,"let*" ){continue}let mut path=base.clone();path.push(Self::pattern_key(p));let mut inner=cx.clone();inner.env=env.clone();inner.path=path.clone();inner.zbody=false;let(t,v)=if pair[1].meta.is_empty()&&matches!((&p.node,&pair[1].node),(SNode::Sym(_),SNode::List(xs)) if xs.first().is_some_and(|h|matches!(&h.node,SNode::Sym(s) if s=="fn"))){self.make_fn(&inner,&pair[1],&pair[1].node_as_args(),Self::pattern_key(p).as_str(),Some(path.clone()))}else{self.binding(&inner,&pair[1],path.clone())};let mut t=t;t.path=Some(path.clone());self.mark(path.clone(),&v);if cx.zbody&&!cx.zone.is_some_and(|z|v.vary.contains(&z)){self.invariant.insert(path.clone());}{let mut bcx=cx.clone();bcx.path=base.clone();self.bind(&bcx,p,v.clone(),"",&mut env);}out.push((Self::pattern(p),t));}let mut inner=cx.clone();inner.env=env;inner.zbody=false;let(t,v)=self.result_in(&inner,&args[1],&base);(make_term(f,v.ty.clone(),Node::Let(out,Box::new(t))),v)}
 fn pattern_key(p:&Syntax)->String{if let SNode::Sym(s)=&p.node{s.clone()}else{lisp::flat(p)}}
 fn list(&mut self,cx:&Cx,f:&Syntax,args:&[Syntax])->(Term,Info){let mut ts=Vec::new();let mut vs=Vec::new();let mut elem=Ty::Any;for a in args{let(t,v)=self.infer(cx,a);self.no_fn(a,&v.ty,"A list element");elem=ty::join(&elem,&v.ty).unwrap_or_else(||{self.error(a,"E_TYPE",&format!("List elements share one type; got {} and {}.",show(&elem),show(&v.ty)));elem.clone()});ts.push(t);vs.push(v)}let mut v=Info::derive(Ty::List(Box::new(elem.clone())),&vs);v.len=Some(args.len());(make_term(f,Ty::List(Box::new(elem)),Node::ListLit(ts)),v)}
 fn concat(&mut self,cx:&Cx,f:&Syntax,args:&[Syntax])->(Term,Info){let mut ts=Vec::new();let mut vs=Vec::new();let mut el=Ty::Any;let mut len=Some(0usize);for a in args{let(t,v)=self.infer(cx,a);if let Some(e)=ty::elem(&v.ty){el=ty::join(&el,&e).unwrap_or_else(||{self.error(a,"E_TYPE","concat joins lists of one type.");el.clone()});}else{self.error(a,"E_TYPE",&format!("concat joins lists; got {}.",show(&v.ty)));}len=match(len,v.len){(Some(x),Some(y))=>x.checked_add(y),_=>None};ts.push(t);vs.push(v)}let ty=Ty::List(Box::new(el));let mut v=Info::derive(ty.clone(),&vs);v.len=len;(make_term(f,ty,Node::ListOp("concat".into(),ts)),v)}
 fn str_form(&mut self,cx:&Cx,f:&Syntax,args:&[Syntax])->(Term,Info){let mut ts=Vec::new();let mut vs=Vec::new();for a in args{let(t,v)=self.infer(cx,a);ts.push(t);vs.push(v)}(make_term(f,Ty::Text,Node::Str(ts)),Info::derive(Ty::Text,&vs))}
 fn get(&mut self,cx:&Cx,f:&Syntax,args:&[Syntax])->(Term,Info){if args.len()!=2{return self.fail_term(f,"E_ARGS","get is (get record :field).")};let(t,v)=self.infer(cx,&args[0]);let SNode::Kw(field)=&args[1].node else{return self.fail_term(f,"E_ARGS","get is (get record :field).")};if !matches!(v.ty,Ty::Record(_)|Ty::Any){self.error(&args[0],"E_TYPE",&format!("get reads a record field; got {}.",show(&v.ty)));}let out=self.field(f,&v,field,&Self::pattern_key(&args[0]));(make_term(f,out.ty.clone(),Node::Get(Box::new(t),field.clone())),out)}
 fn assoc(&mut self, cx: &Cx, f: &Syntax, args: &[Syntax]) -> (Term, Info) {
     if args.len() < 3 || args.len() % 2 == 0 { return self.fail_term(f, "E_ARGS", "assoc is (assoc record :field value …)."); }
     let (rt, rv) = self.infer(cx, &args[0]);
     match rv.ty.clone() {
         Ty::Any => {
             let mut poison = Info::leaf(Ty::Any);
             poison.live = rv.live;
             (make_term(f, Ty::Any, Node::Assoc(Box::new(rt), Vec::new())), poison)
         }
         Ty::Record(mut fs) => {
             let mut lengths = rv.list_fields.clone();
             let mut vs = vec![rv];
             let mut updates: Vec<(String, Term)> = Vec::new();
             for pair in args[1..].chunks_exact(2) {
                 let (k, value) = (&pair[0], &pair[1]);
                 match &k.node {
                     SNode::Kw(n) => {
                         let (t, v) = self.infer(cx, value);
                         self.no_fn(value, &v.ty, &format!("Record field :{n}"));
                         match fs.iter().find(|(name, _)| name == n) {
                             Some((_, want)) => {
                                 if !ty::fits(&v.ty, want) {
                                     let m = format!("assoc :{n} is {}; got {}.", show(want), show(&v.ty));
                                     self.error(value, "E_TYPE", &m);
                                 }
                             }
                             None => fs.push((n.clone(), v.ty.clone())),
                         }
                         lengths.retain(|p| p.first() != Some(n));
                         let fields = if v.live_len { let mut x = vec![Vec::new()]; x.extend(v.list_fields.iter().cloned()); x } else { v.list_fields.clone() };
                         lengths.extend(fields.into_iter().map(|p| { let mut q = vec![n.clone()]; q.extend(p); q }));
                         updates.push((n.clone(), t));
                         vs.push(v);
                     }
                     _ => self.error(k, "E_ARGS", &format!("assoc keys are keywords; got {}.", Self::pattern_key(k))),
                 }
             }
             let ty = Ty::Record(fs);
             let mut info = Info::derive(ty.clone(), &vs);
             info.list_fields = lengths;
             (make_term(f, ty, Node::Assoc(Box::new(rt), updates)), info)
         }
         t => self.fail_term(f, "E_TYPE", &format!("assoc updates a record; got {}.", show(&t))),
     }
 }
 fn if_form(&mut self, cx: &Cx, form: &Syntax, args: &[Syntax]) -> (Term, Info) {
     if args.len() != 3 {
         return self.fail_term(form, "E_NO_ELSE", "if takes a condition, a then and an else.");
     }
     let (condition, condition_value) = self.branch_test(cx, "test", &args[0]);
     if !ty::fits(&condition_value.ty, &Ty::Bool) {
         self.error(&args[0], "E_TYPE", &format!("if needs a bool condition, got {}.", show(&condition_value.ty)));
     }
     let (then_term, then_value) = self.branch(cx, "then", &args[1]);
     let (else_term, else_value) = self.branch(cx, "else", &args[2]);
     if !ty::fits(&then_value.ty, &else_value.ty) && !ty::fits(&else_value.ty, &then_value.ty) {
         self.error(form, "E_TYPE", &format!("Both branches of if must have one type: {} and {}.", show(&then_value.ty), show(&else_value.ty)));
     }
     self.no_fn(&args[1], &then_value.ty, "An if branch");
     self.no_fn(&args[2], &else_value.ty, "An if branch");
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
     (make_term(form, ty, Node::If(Box::new(condition), Box::new(then_term), Box::new(else_term))), value)
 }

 fn cond_form(&mut self, cx: &Cx, form: &Syntax, name: &str, args: &[Syntax]) -> (Term, Info) {
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
     if !matches!(&pairs[pairs.len() - 1][0].node, SNode::Kw(key) if key == "else") {
         return self.fail_term(form, "E_NO_ELSE", &format!("{name} needs a final :else arm, so it always has a value."));
     }
     let scrutinee = scrutinee.map(|form| self.branch_test(cx, "of", form));
     let mut test_live = scrutinee.as_ref().is_some_and(|(_, value)| value.live);
     let mut values = scrutinee.iter().map(|(_, value)| value.clone()).collect::<Vec<_>>();
     let mut conditional_arms = Vec::new();
     let mut case_arms = Vec::new();
     let mut default = None;
     let mut result_ty: Option<Ty> = None;
     for (index, pair) in pairs.iter().enumerate() {
         let test = &pair[0];
         let result_form = &pair[1];
         let is_else = matches!(&test.node, SNode::Kw(key) if key == "else");
         if is_else && index + 1 != pairs.len() {
             self.error(test, "E_COND", &format!(":else is the last arm of {name}."));
         }
         let test_term = if is_else {
             None
         } else if let Some((_, scrutinee_value)) = &scrutinee {
             let literal = matches!(&test.node, SNode::Num(_) | SNode::Str(_))
                 || matches!(&test.node, SNode::Sym(name) if name == "true" || name == "false");
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
         let (mut result, value) = self.branch(cx, &key, result_form);
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
                 case_arms.push((test.clone(), result));
             } else {
                 conditional_arms.push((test_term, result));
             }
         }
     }
     let ty = result_ty.unwrap_or(Ty::Any);
     let default = default.unwrap_or_else(|| make_term(form, Ty::Any, Node::Nil));
     if test_live && shape_ty(&ty) {
         self.error(form, "E_TIME_BRANCH", &format!("A branch in {} picks between shapes by t. The network keeps its shape while playing; pick a value instead.", path_text(&cx.path)));
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
         Node::Case(Box::new(scrutinee), case_arms, Box::new(default))
     } else {
         Node::Cond(conditional_arms, Box::new(default))
     };
     (make_term(form, ty, node), value)
 }
 fn state(&mut self, cx: &Cx, form: &Syntax, args: &[Syntax], at: Option<Path>) -> (Term, Info) {
     if args.len() != 2 {
         return self.fail_term(form, "E_STATE", "state is (state [s init] step).");
     }
     let SNode::Vec(binding) = &args[0].node else {
         return self.fail_term(form, "E_STATE", "state is (state [s init] step).");
     };
     if binding.len() != 2 {
         return self.fail_term(form, "E_STATE", "state is (state [s init] step).");
     }
     let pattern = &binding[0];
     let mut seen = BTreeSet::new();
     if !matches!(&pattern.node, SNode::Sym(_))
         || !self.check_pattern(cx, pattern, &mut seen, &cx.env, "state")
     {
         return self.fail_term(form, "E_PATTERN", "state binds one name.");
     }
     let (init, mut init_value) = self.infer(cx, &binding[1]);
     self.exact_sink(cx, &binding[1], "state init", &init_value);
     self.no_fn(&binding[1], &init_value.ty, "A state seed");
     if init_value.live {
         self.error(&binding[1], "E_STATE_INIT", "A state seed is static; read the frame in the step.");
     }
     if shape_ty(&init_value.ty) {
         self.error(&binding[1], "E_STATE_TYPE", "State stores data, not deferred nodes or layouts.");
     }
     let zone = match at { Some(p) => p, None => self.inline_path(cx, "~state") };
     let seed_list_fields = list_fields(&init_value.ty);
     init_value.live = true;
     init_value.state_dep = true;
     init_value.list_fields = seed_list_fields.clone();
     init_value.live_len = seed_list_fields.iter().any(Vec::is_empty);
     let mut env = cx.env.clone();
     self.bind(cx, pattern, init_value.clone(), ":", &mut env);
     let mut inner = cx.clone();
     inner.env = env;
     inner.path = zone.clone();
     let (step, step_value) = self.body(&inner, &args[1]);
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
         make_term(form, ty, Node::State {
             binder: Self::pattern(pattern),
             init: Box::new(init),
             step: Box::new(step),
             zone,
         }),
         value,
     )
 }
 fn zone(&mut self, cx: &Cx, form: &Syntax, name: &str, args: &[Syntax], at: Option<Path>) -> (Term, Info) {
     let kind = match name { "for" => LoopKind::For, "fold" => LoopKind::Fold, "scan" => LoopKind::Scan, _ => LoopKind::Sum };
     let is_acc = matches!(kind, LoopKind::Fold | LoopKind::Scan);
     let even = |xs: &Vec<Syntax>| xs.len() % 2 == 0;
     let shape: Option<(Vec<Syntax>, Vec<Syntax>, &Syntax, Vec<Vec<usize>>)> = match args {
         [Syntax { node: SNode::Vec(a), .. }, Syntax { node: SNode::Vec(i), .. }, b] if is_acc && even(a) && even(i) => Some((a.clone(), i.clone(), b, Vec::new())),
         [Syntax { node: SNode::Vec(i), .. }, b] if !is_acc && even(i) => Some((Vec::new(), i.clone(), b, Vec::new())),
         [Syntax { node: SNode::Vec(i), .. }, Syntax { node: SNode::Kw(k), .. }, sk, b] if k == "skip" && name == "for" && even(i) => {
             let skip = match skip_tuples(sk) {
                 Some(t) => t,
                 None => {
                     self.error(sk, "E_SKIP", ":skip is a list of iteration tuples of non-negative integers, for example :skip [[0 1] [2 0]].");
                     Vec::new()
                 }
             };
             Some((Vec::new(), i.clone(), b, skip))
         }
         _ => None,
     };
     let Some((accs, iters, body_form, skip)) = shape else {
         let msg = if is_acc { format!("{name} is ({name} [acc init] [i collection] body).") } else { format!("{name} is ({name} [i collection …] body).") };
         return self.fail_term(form, "E_ZONE", &msg);
     };
     let mut seen = BTreeSet::new();
     let mut ok = true;
     for pair in accs.chunks_exact(2).chain(iters.chunks_exact(2)) {
         ok = self.check_pattern(cx, &pair[0], &mut seen, &cx.env, name) && ok;
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
     let init: Option<(Syntax, Term, Info)> = if is_acc {
         let (t, v) = self.infer(cx, &accs[1]);
         self.no_fn(&accs[1], &v.ty, &format!("The {name} accumulator"));
         Some((accs[0].clone(), t, v))
     } else { None };
     let last = id.last().cloned().unwrap_or_else(|| name.to_owned());
     let mut zcx = cx.clone();
     zcx.path = id.clone();
     let acc_live0 = init.as_ref().is_some_and(|(_, _, v)| v.live);
     let mut run_live = acc_live0;
     let (clauses, lens, outer, bt, bv) = loop {
         let mut env = cx.env.clone();
         if let Some((p, _, iv)) = &init {
             let mut seed = iv.clone();
             seed.live = run_live;
             seed.vary.insert(zid);
             seed.live_len = false;
             self.bind(&zcx, p, seed, ":", &mut env);
         }
         let mut clauses = Vec::new();
         let mut lens = Vec::new();
         let mut outer = Vec::new();
         for pair in iters.chunks_exact(2) {
             let (p, e) = (&pair[0], &pair[1]);
             let mut ecx = cx.clone();
             ecx.env = env.clone();
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
             self.bind(&zcx, p, part, ":", &mut env);
             clauses.push((Self::pattern(p), ct));
             lens.push(cv.len);
             outer.push(cv);
         }
         let mut inner = cx.clone();
         inner.env = env;
         inner.path = id.clone();
         inner.scope = format!("{} › {}", cx.scope, last);
         inner.zone = Some(zid);
         inner.zbody = true;
         let (bt, bv) = self.body(&inner, body_form);
         if init.is_some() && bv.live && !acc_live0 && !run_live { run_live = true; continue; }
         break (clauses, lens, outer, bt, bv);
     };
     // ponytail: literal counts only; a driven count is bounded when Eval runs the zone.
     if !clauses.iter().any(|(_, t)| matches!(t.ty, Ty::Array(_))) && lens.iter().all(|l| l.is_some_and(|n| n <= MAX_ITERATIONS)) {
         let total = lens.iter().fold(1usize, |a, l| (MAX_ITERATIONS + 1).min(a * l.unwrap_or(0)));
         if total > MAX_ITERATIONS { self.error(form, "E_ITER_BOUND", &format!("{last} runs more than 4,096 iterations.")); }
     }
     let acc_ty = init.as_ref().map(|(_, _, iv)| iv.ty.clone());
     if let Some(at) = &acc_ty {
         if !ty::fits(&bt.ty, at) {
             self.error(form, "E_ACC_TYPE", &format!("{name} body must return the accumulator type {}; it returns {}.", show(at), show(&bt.ty)));
         }
     }
     let acc_ty = acc_ty.map(|at| ty::join(&at, &bt.ty).unwrap_or(at));
     let packed = clauses.iter().any(|(_, t)| matches!(t.ty, Ty::Array(_)));
     let result_ty = match (kind, &acc_ty) { (LoopKind::Fold, Some(t)) => t.clone(), _ => bt.ty.clone() };
     let cooked_fold = kind == LoopKind::Fold && cx.ctx == context::sop() && geometry_accumulator(&result_ty) && cx.stack.iter().any(|n| self.node_defs.contains_key(n));
     if packed && shape_ty(&result_ty) && !cooked_fold {
         self.error(form, "E_ARRAY_TYPE", "Packed-array loops produce data; their length cannot create graph nodes.");
     }
     let array_elem = |this: &mut Self, ty: &Ty| match ty {
         Ty::Int | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text => ty.clone(),
         Ty::Float | Ty::Bool | Ty::Any => Ty::Float,
         _ => { this.error(form, "E_ARRAY_TYPE", "Arrays hold ints, floats, vectors or text values."); Ty::Float }
     };
     if kind == LoopKind::Sum && !matches!(bt.ty, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Any) {
         self.error(form, "E_TYPE", &format!("sum adds numbers or matching-width vectors; the body returns {}.", show(&bt.ty)));
     }
     let ty = match (kind, &acc_ty) {
         (LoopKind::Scan, Some(at)) => if packed { Ty::Array(Box::new(array_elem(self, at))) } else { Ty::List(Box::new(at.clone())) },
         (LoopKind::For | LoopKind::Scan, _) => if packed { Ty::Array(Box::new(array_elem(self, &bt.ty))) } else { Ty::List(Box::new(bt.ty.clone())) },
         (LoopKind::Sum, _) => match bt.ty { Ty::Int | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => bt.ty.clone(), _ => Ty::Float },
         (LoopKind::Fold, Some(at)) => at.clone(),
         (LoopKind::Fold, None) => Ty::Any,
     };
     let mut ins = outer.clone();
     if let Some((_, _, iv)) = &init { ins.push(iv.clone()); }
     let mut all = vec![bv.clone()];
     all.extend(ins.iter().cloned());
     let mut value = Info::derive(ty.clone(), &all);
     value.approx = kind == LoopKind::For && skip.is_empty() && packed_array(&ty) && clauses.len() == 1
         && clauses.iter().all(|(p, _)| matches!(p, Pattern::Name(_)))
         && clauses.iter().all(|(_, t)| packed_array(&t.ty))
         && packed_body(&self.ops, &bt);
     value.vary = bv.vary.iter().filter(|v| **v != zid).copied().collect();
     for i in &ins { value.vary.extend(i.vary.iter().copied()); }
     let mut reasons: Vec<Diagnostic> = Vec::new();
     if kind != LoopKind::For || clauses.len() != 1 {
         reasons.push(err("E_GPU_FORM", "GPU kernels require collecting one-source loops.".into(), Some(form.span.clone())));
     } else {
         for (pattern, source) in &clauses {
             match pattern {
                 Pattern::Name(_) => match &source.ty {
                     Ty::Any => (),
                     Ty::Array(e) if **e == Ty::Any => (),
                     t if packed_array(t) => (),
                     t => reasons.push(err("E_PACKED_TYPE", format!("Packed source requires Array Float/Vec2/Vec3/Vec4, got {}.", show(t)), Some(source.form.span.clone()))),
                 },
                 _ => reasons.push(err("E_PACKED_FORM", "Packed parameters require name patterns.".into(), Some(form.span.clone()))),
             }
         }
     }
     if reasons.is_empty() { self.add_root(form, id.clone(), None); } else { for r in reasons { self.add_root(form, id.clone(), Some(r)); } }
     self.link_body(&bt, &id);
     value.packed = [id.clone()].into_iter().collect();
     let accs_ir = init.as_ref().map(|(p, t, _)| vec![(Self::pattern(p), t.clone())]).unwrap_or_default();
     (make_term(form, ty, Node::Loop { kind, accs: accs_ir, clauses, skip, body: Box::new(bt), zone: id }), value)
 }
 fn link_body(&mut self, term: &Term, producer: &Path) {
     match &term.node {
         Node::Hof(_, _) | Node::Loop { .. } | Node::Fn { .. } => return,
         Node::Op { op: name, .. }
             if op::find(&self.ops, name, &context::value())
                 .and_then(|operator| op::packed_kind(&operator))
                 == Some(op::PackedKind::Exact) => return,
         _ => (),
     }
     if let Some(path) = &term.path {
         let next = PackedForm::Candidate([producer.clone()].into_iter().collect());
         self.packed.entry(path.clone()).and_modify(|old| *old = Self::merge_packed(old.clone(), next.clone())).or_insert(next);
     }
     let visit: Vec<&Term> = match &term.node {
         Node::Vec(ts) => ts.iter().collect(),
         Node::Get(t, _) | Node::Bypass(t) | Node::Expanded { body: t, .. } => vec![t.as_ref()],
         Node::Op { args, .. } => args.iter().map(|(_, t)| t).collect(),
         Node::Let(bindings, result) => bindings.iter().map(|(_, t)| t).chain(std::iter::once(result.as_ref())).collect(),
         Node::WithError(a, b) => vec![a.as_ref(), b.as_ref()],
         Node::ErrorRows { rows, mapper, .. } => vec![rows.as_ref(), mapper.as_ref()],
         Node::If(c, a, b) => vec![c.as_ref(), a.as_ref(), b.as_ref()],
         Node::Cond(arms, d) => arms.iter().flat_map(|(a, b)| [a, b]).chain(std::iter::once(d.as_ref())).collect(),
         Node::Case(s, arms, d) => std::iter::once(s.as_ref()).chain(arms.iter().map(|(_, t)| t)).chain(std::iter::once(d.as_ref())).collect(),
         Node::CallFn { args, body, .. } => args.iter().chain(body.iter().map(|b| b.as_ref())).collect(),
         _ => Vec::new(),
     };
     for child in visit {
         self.link_body(child, producer);
     }
 }
 fn make_fn(&mut self,cx:&Cx,f:&Syntax,args:&[Syntax],name:&str,at:Option<Path>)->(Term,Info){if args.len()!=2{return self.fail_term(f,"E_FN","fn is (fn [params] body).")}let SNode::Vec(ps)=&args[0].node else{return self.fail_term(f,"E_FN","fn is (fn [params] body).")};let path=match at{Some(p)=>p,None=>self.inline_path(cx,"~fn")};let mut bcx=cx.clone();bcx.path=path.clone();let mut params=Vec::new();let mut env=cx.env.clone();let mut seen=BTreeSet::new();for p in ps{let(mut pat,mut annot)=(p.clone(),None);if let SNode::List(parts)=&p.node{if parts.len()==3&&matches!(&parts[1].node,SNode::Sym(s) if s==":"){pat=parts[0].clone();annot=ty::of_syntax(&parts[2]);if annot.is_none(){self.error(p,"E_PARAM","Invalid fn parameter annotation.")}}}if !self.check_pattern(cx,&pat,&mut seen,&env,"fn"){continue}params.push((pat.clone(),annot.clone()));self.bind(&bcx,&pat,Info::leaf(annot.unwrap_or(Ty::Any)),":",&mut env)}let mut inner=cx.clone();inner.env=env.clone();inner.path=path.clone();inner.scope=format!("{} › {name}",cx.scope);let(bt,bv)=self.body(&inner,&args[1]);let callable=Rc::new(Callable::Local(Rc::new(Closure{params:params.iter().map(|(p,t)|(p.clone(),t.clone())).collect(),body:args[1].clone(),env:cx.env.clone(),ctx:cx.ctx.clone(),path:path.clone(),name:name.into()})));let ir_params=params.iter().map(|(p,t)|(Self::pattern(p),t.clone())).collect();let mut v=Info::leaf(Ty::Fn(None));v.live=bv.live;v.state_dep=bv.state_dep;v.callable=Some(callable);(make_term(f,Ty::Fn(None),Node::Fn{params:ir_params,body:Box::new(bt),zone:path,capture:None}),v)}
 fn fn_value(&mut self, cx: &Cx, form: &Syntax, what: &str) -> (Term, Info) {
     if let SNode::Sym(name) = &form.node {
         if !cx.env.contains_key(name) && !name.contains('.') && !SPECIAL.contains(&name.as_str()) {
             if self.defs.contains_key(name) {
                 if self.call_sites {
                     self.check_def_body(name, true);
                 }
                 let mut value = Info::leaf(Ty::Fn(None));
                 value.callable = Some(Rc::new(Callable::Def(name.clone())));
                 return (make_term(form, Ty::Fn(None), Node::FnRef(name.clone())), value);
             }
             if let Some(operator) = op::find(&self.ops, name, &cx.ctx) {
                 let mut value = Info::leaf(Ty::Fn(None));
                 value.callable = Some(Rc::new(Callable::Op(name.clone())));
                 return (make_term(form, Ty::Fn(None), Node::FnRef(operator.name.clone())), value);
             }
             if let Ok(kind) = catalog::resolve_kind(&self.catalog, &cx.ctx, name) {
                 self.kind_fns.insert(name.clone(), (kind.qualified.clone(), kind.context.clone(), kind.slots.clone()));
                 let mut value = Info::leaf(Ty::Fn(None));
                 value.callable = Some(Rc::new(Callable::Kind(kind)));
                 return (make_term(form, Ty::Fn(None), Node::FnRef(name.clone())), value);
             }
             if self.macros.contains_key(name) {
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
 fn call_closure(&mut self,cx:&Cx,f:&Syntax,c:&Closure,args:&[Arg])->(Term,Info){
     if args.len()!=c.params.len(){return self.fail_term(f,"E_ARITY",&format!("{} takes {} argument{}; got {}.",c.name,c.params.len(),if c.params.len()==1{""}else{"s"},args.len()))}
     if cx.depth>64{return self.fail_term(f,"E_DEPTH","Call depth exceeds 64.")}
     let mut env=c.env.clone();
     let mut pcx=cx.clone();pcx.path=c.path.clone();
     for ((pattern,annotation),arg) in c.params.iter().zip(args){
         let mut value=arg.info.clone();
         if let Some(want)=annotation{
             if !ty::fits(&value.ty,want){self.error(&arg.form,"E_TYPE",&format!("{} {}: expected {}, got {}.",c.name,Self::pattern_key(pattern),show(want),show(&value.ty)))}
             value.ty=ty::coerce(&value.ty,want);
         }
         self.bind(&pcx,pattern,value,":",&mut env);
     }
     let mut inner=cx.clone();
     inner.env=env;
     inner.ctx=c.ctx.clone();
     inner.path=c.path.clone();
     inner.scope=format!("{} › {}",cx.scope,c.name);
     inner.depth=cx.depth+1;
     inner.stack.extend(cx.stack.clone());inner.stack.sort();inner.stack.dedup();
     inner.zone=None;
     inner.zbody=false;
     self.body(&inner,&c.body)
 }
 fn apply_callable(&mut self,cx:&Cx,f:&Syntax,value:&Info,args:&[Arg])->(Term,Info){
     match value.callable.as_deref(){
         Some(Callable::Local(closure))=>self.call_closure(cx,f,closure,args),
         Some(Callable::Def(name))=>self.apply_def(cx,f,name,args),
         Some(Callable::Op(name))=>match op::find(&self.ops,name,&cx.ctx){Some(operator)=>self.apply_op(cx,f,&operator,args,Vec::new()),None=>self.fail_term(f,"E_UNKNOWN_KIND","Unknown operator.")},
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
             (make_term(f,result.clone(),Node::Nil),Info::derive(result,std::slice::from_ref(value)))
         }
     }
 }
 fn fn_argument(&mut self, cx: &Cx, form: &Syntax) -> (Term, Info) {
     if let SNode::List(items) = &form.node {
         if items.first().is_some_and(|head| matches!(&head.node, SNode::Sym(name) if name == "list")) {
             let mut terms = Vec::new();
             let mut values = Vec::new();
             for item in items.iter().skip(1) {
                 let (term, value) = self.fn_value(cx, item, "function input");
                 terms.push(term);
                 values.push(value);
             }
             let ty = Ty::List(Box::new(Ty::Fn(None)));
             let mut value = Info::derive(ty.clone(), &values);
             value.len = Some(terms.len());
             return (make_term(form, ty, Node::ListLit(terms)), value);
         }
     }
     self.fn_value(cx, form, "function input")
 }

 fn args_of(
     &mut self,
     cx: &Cx,
     forms: &[Syntax],
     fn_slot: &dyn Fn(Option<usize>, Option<&str>) -> bool,
 ) -> Vec<Arg> {
     let mut result = Vec::new();
     let mut at = 0;
     let mut positional = 0;
     while at < forms.len() {
         if let SNode::Kw(key) = &forms[at].node {
             if at + 1 == forms.len() {
                 self.error(&forms[at], "E_MISSING_VALUE", &format!(":{key} has no value"));
                 break;
             }
             let form = &forms[at + 1];
             let (term, info) = if fn_slot(None, Some(key)) && syntax::head(form) != Some("fn") {
                 self.fn_argument(cx, form)
             } else {
                 self.input(cx, form, &format!(":{key}"))
             };
             result.push(Arg { key: Some(key.clone()), form: forms[at].clone(), term, info });
             at += 2;
         } else {
             let form = &forms[at];
             let (term, info) = if fn_slot(Some(positional), None) && syntax::head(form) != Some("fn") {
                 self.fn_argument(cx, form)
             } else {
                 self.input(cx, form, &positional.to_string())
             };
             result.push(Arg { key: None, form: form.clone(), term, info });
             positional += 1;
             at += 1;
         }
     }
     result
 }
 fn call_named(&mut self, cx: &Cx, form: &Syntax, name: &str, forms: &[Syntax]) -> (Term, Info) {
     let local = cx.env.get(name).cloned();
     if let Some(local) = local.as_ref().filter(|local| ty::fits(&local.ty, &Ty::Fn(None))) {
         if forms.iter().any(|f| matches!(f.node, SNode::Kw(_))) {
             return self.fail_term(form, "E_ARGS", &format!("{name} is a local function; it takes positional arguments only."));
         }
         let args = self.args_of(cx, forms, &|_, _| false);
         let (result, rv) = self.apply_callable(cx, form, local, &args);
         let body = match (local.callable.as_deref(), &result.node) {
             (Some(Callable::Local(_)), _) => Some(Box::new(result.clone())),
             (Some(Callable::Def(_)), Node::CallFn { body, .. }) => body.clone(),
             _ => None,
         };
         return (make_term(form, rv.ty.clone(), Node::CallFn { fn_name: name.to_owned(), args: args.iter().map(|a| a.term.clone()).collect(), body }), rv);
     }
     if self.macros.contains_key(name) {
         let definitions = self.macro_forms.clone();
         return match macros::expand(&definitions, form) {
             Ok(expanded) => {
                 let (term, value) = self.infer(cx, &expanded);
                 (make_term(form, term.ty.clone(), Node::Expanded { macro_name: name.into(), body: Box::new(term) }), value)
             }
             Err(problem) => {
                 self.diags.push(problem);
                 self.fail_term_no_diag(form)
             }
         };
     }
     if let Some(definition) = self.defs.get(name).cloned() {
         let fn_slot = |position: Option<usize>, key: Option<&str>| {
             let parameter = if let Some(index) = position {
                 definition.params.get(index)
             } else {
                 key.and_then(|key| definition.params.iter().find(|(name, _, _)| name == key))
             };
             parameter.is_some_and(|(_, ty, _)| matches!(ty, Ty::Fn(_)))
         };
         let args = self.args_of(cx, forms, &fn_slot);
         return self.apply_sig(cx, form, &definition, &args);
     }
     if let Some(operator) = op::find(&self.ops, name, &cx.ctx) {
         if operator.name == "scene/merge" {
             let mut skip = Vec::new();
             let mut args_form = Vec::new();
             let mut at = 0;
             while at < forms.len() {
                 if matches!(&forms[at].node, SNode::Kw(key) if key == "skip") && at + 1 < forms.len() {
                     if let Some(tuples) = skip_tuples(&forms[at + 1]) {
                         skip.extend(tuples);
                     } else {
                         self.error(&forms[at + 1], "E_SKIP", ":skip is a list of iteration tuples of non-negative integers, for example :skip [[0 1] [2 0]].");
                     }
                     at += 2;
                 } else {
                     args_form.push(forms[at].clone());
                     at += 1;
                 }
             }
             let args = self.args_of(cx, &args_form, &|_, _| false);
             return self.apply_op(cx, form, &operator, &args, skip);
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
         let forms = syntax::attribute_args(name, forms);
         let args = self.args_of(cx, &forms, &fn_slot);
         return self.apply_op(cx, form, &operator, &args, Vec::new());
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
             let args = self.args_of(cx, forms, &fn_slot);
             self.apply_kind(cx, form, &kind, &args)
         }
         Err((code, message)) => {
             if code == "E_UNKNOWN_KIND" && message.starts_with(&format!("Unknown node {name}.")) {
                 let suffix = &message[format!("Unknown node {name}.").len()..];
                 self.fail_term(form, &code, &format!("Unknown operator “{name}”.{suffix}"))
             } else {
                 self.fail_term(form, &code, &message)
             }
         }
     }
 }
 fn apply_def(&mut self,cx:&Cx,f:&Syntax,name:&str,args:&[Arg])->(Term,Info){let Some(d)=self.defs.get(name).cloned()else{return self.fail_term(f,"E_UNKNOWN_KIND","Unknown definition.")};self.apply_sig(cx,f,&d,args)}
 fn apply_sig(&mut self,cx:&Cx,f:&Syntax,d:&Sig,args:&[Arg])->(Term,Info){if d.ctx!=cx.ctx&&d.ctx!=context::value(){return self.fail_term(f,"E_WRONG_CONTEXT",&format!("{} belongs to {}; it cannot run in {}.",d.name,context_name(&d.ctx),context_name(&cx.ctx)))}if cx.stack.contains(&d.name){let mut stack=cx.stack.clone();stack.push(d.name.clone());return self.fail_term(f,"E_RECURSION",&format!("Recursive call: {}. Use fold for repetition.",stack.join(" → ")))}let pos=args.iter().filter(|a|a.key.is_none()).collect::<Vec<_>>();if pos.len()>d.params.len(){self.error(f,"E_ARITY",&format!("{} takes {} inputs; got {} positional.",d.name,d.params.len(),pos.len()))}let mut env=BTreeMap::new();let mut terms=Vec::new();for(i,(n,want,default))in d.params.iter().enumerate(){let named=args.iter().find(|a|a.key.as_deref()==Some(n));let bypos=pos.get(i).copied();if bypos.is_some()&&named.is_some(){self.error(&named.unwrap().form,"E_DUPLICATE_PARAM",&format!("{} input {n} is given twice.",d.name))}let(t,v)=if let Some(a)=bypos.or(named){if !ty::fits(&a.info.ty,want){self.error(&a.form,"E_TYPE",&format!("{} :{n} expects {}; got {}.",d.name,show(want),show(&a.info.ty)))}let mut v=a.info.clone();v.ty=ty::coerce(&v.ty,want);(a.term.clone(),v)}else if let Some(default)=default{let dcx=Cx{env:BTreeMap::new(),ctx:d.ctx.clone(),path:vec![format!("def:{}",d.name),format!(":{n}")],scope:format!("ƒ {}",d.name),in_def:true,stack:vec![d.name.clone()],depth:0,zone:None,zbody:false};let(t,v)=self.infer(&dcx,default);if !ty::fits(&v.ty,want){self.error(default,"E_TYPE","Definition default differs from its declared input type.")}(t,v)}else{self.error(f,"E_ARGS",&format!("{} needs :{n} ({}).",d.name,show(want)));(make_term(f,want.clone(),Node::Nil),Info::leaf(want.clone()))};env.insert(n.clone(),v);terms.push(t)}for a in args{if let Some(k)=&a.key{if !d.params.iter().any(|(n,_,_)|n==k){self.error(&a.form,"E_UNKNOWN_PARAM",&format!("{} has no input :{k}.",d.name))}}}let mut inner=Cx{env,ctx:d.ctx.clone(),path:vec![format!("def:{}",d.name)],scope:format!("ƒ {}",d.name),in_def:true,stack:cx.stack.clone(),depth:cx.depth+1,zone:None,zbody:false};inner.stack.push(d.name.clone());let(body,v)=self.body(&inner,&d.body);(make_term(f,v.ty.clone(),Node::CallFn{fn_name:d.name.clone(),args:terms,body:Some(Box::new(body))}),v)}
 fn apply_op(&mut self, cx: &Cx, form: &Syntax, operator: &op::Op, arguments: &[Arg], skip: Vec<Vec<usize>>) -> (Term, Info) {
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
             Some(Ty::Fn(Some(signature))) => self.instantiate_fn(cx, argument, &signature, true),
             _ => argument.clone(),
         });
     }
     if operator.name == "ui/canvas" {
         for argument in &mut args {
             if argument.key.is_none() && argument.info.ty == ty::image() {
                 argument.term = make_term(
                     &argument.term.form,
                     ty::drawing(),
                     Node::Op {
                         op: "draw/image".into(),
                         args: vec![("image".into(), argument.term.clone())],
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
         self.error(form, "E_ARITY", &format!("{} takes {} positional inputs; got {}.", operator.name, operator.signature.pos.len(), positional.len()));
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
             if !ty::fits(&argument.info.ty, want) && !compatible_number {
                 self.error(&argument.form, "E_TYPE", &format!("{} {} expects {}; got {}.", operator.name, name, show(want), show(&argument.info.ty)));
             }
             named.push((name.clone(), argument.term.clone()));
         } else if let Some((name, want)) = &operator.signature.rest {
             let element = ty::elem(&argument.info.ty);
             if let Some(element) = element {
                 if !ty::fits(&element, want) {
                     self.error(&argument.form, "E_TYPE", &format!("{} {} expects {}; got {}.", operator.name, name, show(want), show(&argument.info.ty)));
                 }
                 if argument.info.live_len && shape_ty(want) {
                     self.error(&argument.form, "E_TIME_COUNT", &format!("The list passed to {} changes length with t. The network keeps its shape while playing; animate parameters instead, for example scale a piece to 0.", operator.name));
                 }
             } else if !ty::fits(&argument.info.ty, want) {
                 self.error(&argument.form, "E_TYPE", &format!("{} {} expects {}; got {}.", operator.name, name, show(want), show(&argument.info.ty)));
             }
             named.push((name.clone(), argument.term.clone()));
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
                 if !ty::fits(&argument.info.ty, want) {
                     self.error(&argument.form, "E_TYPE", &format!("{} :{key} expects {}; got {}.", operator.name, show(want), show(&argument.info.ty)));
                 }
                 named.push((key.to_owned(), argument.term.clone()));
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
     let typed = named.iter().map(|(name, term)| (name.clone(), term.ty.clone(), literal_param(term))).collect::<Vec<_>>();
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
     let literal_int = |index: usize| positional.get(index).and_then(|argument| match literal_param(&argument.term) {
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
     let capability = op::packed_kind(operator);
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
         self.add_root(form, cx.path.clone(), Some(err("E_GPU_FORM", "Ordered reductions stay on the CPU.".into(), Some(form.span.clone()))));
         info.packed.insert(cx.path.clone());
     } else if operator.name == "image/map" {
         self.add_root(form, cx.path.clone(), None);
         for argument in &args {
             if let Node::Fn { body, zone, .. } = &argument.term.node {
                 self.link_body(body, &cx.path);
                 self.packed.entry(zone.clone()).and_modify(|old| *old = Self::merge_packed(old.clone(), PackedForm::Candidate([cx.path.clone()].into_iter().collect()))).or_insert_with(|| PackedForm::Candidate([cx.path.clone()].into_iter().collect()));
             }
         }
         info.packed.insert(cx.path.clone());
     }
     (make_term(form, result, Node::Op { op: operator.name.clone(), args: named, skip }), info)
 }

 fn literal_checks(&mut self, form: &Syntax, operator: &op::Op, args: &[Arg]) {
     let positional = args.iter().filter(|argument| argument.key.is_none()).collect::<Vec<_>>();
     let integer = |argument: &Arg| match literal_param(&argument.term) {
         Some(param::Value::Int(value)) => Some(value),
         _ => None,
     };
     let number = |argument: &Arg| match literal_param(&argument.term) {
         Some(param::Value::Int(value)) => Some(value as f64),
         Some(param::Value::Float(value)) => Some(value),
         _ => None,
     };
     let text = |argument: &Arg| match &argument.term.node {
         Node::Text(bytes) => std::str::from_utf8(bytes).ok().map(str::to_owned),
         Node::Lit(param::Value::Text(bytes)) => std::str::from_utf8(bytes).ok().map(str::to_owned),
         Node::Lit(param::Value::Choice(value)) => Some(value.clone()),
         _ => None,
     };
     match operator.name.as_str() {
         "sop/context_seed" => match positional.as_slice() {
             [argument] if text(argument).is_some_and(|value| !value.trim().is_empty()) => (),
             [argument] => self.error(&argument.form, "E_SEED_NAMESPACE", "Context seed requires a literal nonblank namespace."),
             _ => (),
         },
         "image/map" => {
             for argument in &positional {
                 match &argument.info.ty {
                     Ty::Fn(Some(signature)) if signature.params == [Ty::Vec2] && signature.result == Ty::Vec4 => (),
                     Ty::Fn(None) | Ty::Any => (),
                     _ => self.error(&argument.form, "E_TYPE", "image/map needs a vec2-to-vec4 pixel function."),
                 }
             }
             for argument in args {
                 if matches!(argument.key.as_deref(), Some("width" | "height"))
                     && integer(argument).is_some_and(|value| value <= 0)
                 {
                     self.error(&argument.form, "E_RANGE", "Image dimensions must be positive.");
                 }
             }
         }
         "length" => {
             for argument in &positional {
                 if argument.info.ty != Ty::Vec3 && argument.info.ty != Ty::Any {
                     self.error(&argument.form, "E_TYPE", "length needs a vec3.");
                 }
             }
         }
         "material/standard" => for argument in args {
             match (argument.key.as_deref(), &argument.term.node) {
                 (Some("color" | "emission"), Node::Text(bytes)) if !hex_colour(bytes) => self.error(&argument.form, "E_TYPE", "A material colour is #rrggbb or a vec3."),
                 (Some("roughness"), _) if number(argument).is_some_and(|value| !(0.0..=1.0).contains(&value)) => self.error(&argument.form, "E_RANGE", "Material roughness must be in [0,1]."),
                 _ => (),
             }
         },
         "range" => match positional.iter().map(|argument| integer(argument)).collect::<Vec<_>>().as_slice() {
             [Some(count)] if *count > MAX_ITERATIONS as i64 => self.error(form, "E_ITER_BOUND", &format!("range 0‥{count} exceeds 4,096 iterations.")),
             [Some(start), Some(finish)] if *finish as i128 - *start as i128 > MAX_ITERATIONS as i128 => self.error(form, "E_ITER_BOUND", &format!("range {start}‥{finish} exceeds 4,096 iterations.")),
             _ => (),
         },
         "linspace" if positional.len() == 3 && integer(positional[2]).is_some_and(|count| count > MAX_ITERATIONS as i64) => {
             self.error(form, "E_ITER_BOUND", "linspace exceeds 4,096 values.");
         }
         "ui/split" | "ui/split-at" => {
             if let Some(axis) = positional.first().and_then(|argument| text(argument)) {
                 let choices = operator.choices.iter().find(|(name, _)| name == "axis").map(|(_, values)| values);
                 if choices.is_some_and(|choices| !choices.contains(&axis)) {
                     self.error(form, "E_RANGE", "Split axis is horizontal or vertical.");
                 }
             }
             if operator.name == "ui/split-at" && positional.len() > 1 && number(positional[1]).is_some_and(|value| !(0.1..=0.9).contains(&value)) {
                 self.error(form, "E_RANGE", "Split ratio is 0.1–0.9.");
             }
             let mut fixed = Vec::new();
             for argument in args.iter().filter(|argument| matches!(argument.key.as_deref(), Some("first_size" | "second_size"))) {
                 if number(argument).is_some_and(|value| value < 1.0) {
                     self.error(&argument.form, "E_RANGE", "A fixed split size is 1 point or more.");
                 }
                 fixed.push(argument);
             }
             if fixed.len() > 1 {
                 self.error(form, "E_RANGE", "A split fixes one side: :first_size or :second_size.");
             }
         }
         "ui/graph" => if let Some(argument) = positional.first() {
             if let Some(name) = text(argument) {
                 let valid = self.graphs.contains_key(&name)
                     || name.strip_prefix("def:").is_some_and(|name| self.defs.contains_key(name));
                 if !valid {
                     self.error(&argument.form, "E_UNKNOWN_GRAPH", &format!("Unknown graph: {name}."));
                 }
             }
         },
         _ => (),
     }
 }
 fn instantiate_fn(&mut self, cx: &Cx, argument: &Arg, signature: &FnSignature, coerce_params: bool) -> Arg {
     if let Node::ListLit(entries) = &argument.term.node {
         let mut terms = Vec::with_capacity(entries.len());
         let mut values = Vec::with_capacity(entries.len());
         for entry in entries {
             let (term, value) = self.fn_value(cx, &entry.form, "function port");
             let nested = Arg { key: None, form: entry.form.clone(), term, info: value };
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
             term: make_term(&argument.term.form, ty, Node::ListLit(terms)),
             info: value,
         };
     }
     let Some(callable) = argument.info.callable.clone() else {
         return argument.clone();
     };
     let mut closure = match callable.as_ref() {
         Callable::Local(closure) => Some((**closure).clone()),
         Callable::Def(name) => self.defs.get(name).map(|definition| Closure {
             params: definition.params.iter().map(|(name, ty, _)| {
                 (syntax::make(SNode::Sym(name.clone())), Some(ty.clone()))
             }).collect(),
             body: definition.body.clone(),
             env: BTreeMap::new(),
             ctx: definition.ctx.clone(),
             path: vec![format!("def:{name}")],
             name: name.clone(),
         }),
         _ => None,
     };
     let mut inputs = Vec::with_capacity(signature.params.len());
     for (index, ty) in signature.params.iter().enumerate() {
         let name = format!("__flow_port_{index}");
         let form = syntax::make(SNode::Sym(name.clone()));
         let value = Info::leaf(ty.clone());
         inputs.push(Arg {
             key: None,
             form: argument.form.clone(),
             term: make_term(&form, ty.clone(), Node::RefBinding(name, Vec::new())),
             info: value,
         });
     }
     let (body, value) = if let Some(closure) = closure.as_ref() {
         self.call_closure(cx, &argument.form, closure, &inputs)
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
             (params, c.path.clone())
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
     Arg {
         key: argument.key.clone(),
         form: argument.form.clone(),
         term: make_term(&argument.term.form, ty, Node::Fn {
             params,
             body: Box::new(body),
             zone,
             capture: Some(Box::new(argument.term.clone())),
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
 fn apply_kind(&mut self, cx: &Cx, form: &Syntax, kind: &catalog::Kind, args: &[Arg]) -> (Term, Info) {
     let mut position = 0;
     let mut prepared: Vec<Arg> = Vec::with_capacity(args.len());
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
             Some(signature) => self.instantiate_fn(cx, arg, &signature, true),
             None => arg.clone(),
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
     let mut named: Vec<(String, Term)> = Vec::new();
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
                     named.push((slot.name.clone(), arg.term.clone()));
                 } else if index >= nslots {
                     self.error(&arg.form, "E_EXTRA_POSITIONAL", &format!("{} takes {} geometry input{}; this one is extra", short, nslots, plural(nslots)));
                 } else {
                     let slot = &kind.slots[index];
                     if seen.contains(&slot.name) {
                         self.error(&arg.form, "E_DUPLICATE_PARAM", &format!("Input {} is given twice: by :{} and by position", slot.name, slot.name));
                     } else {
                         seen.insert(slot.name.clone());
                         self.slot_arg(kind, arg, slot, false);
                         named.push((slot.name.clone(), arg.term.clone()));
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
                             named.push((name.clone(), arg.term.clone()));
                         }
                         (None, Some(parameter)) => {
                             if kind.qualified == "sop/material" && name == "material" && arg.info.ty == ty::material() {
                             } else if matches!((&parameter.ty, &arg.info.ty, &arg.info.callable), (Some(port_type::PortType::Fn(_)), Ty::Fn(None), None)) {
                             } else {
                                 validate_parameter(Some(arg.info.ty.clone()), &mut |severity, code, message| self.add(severity, &arg.form, code, message.to_owned()), parameter, &arg.term);
                             }
                             if let Node::Text(bytes) = &arg.term.node {
                                 let group = String::from_utf8_lossy(&bytes[..]).into_owned();
                                 if group_reader(parameter) && !group.is_empty() && !groups_in.contains(&group) {
                                     self.warn_owned(&arg.form, "W_UNKNOWN_GROUP", format!("Group \"{group}\" is not made by any node upstream of {short}."));
                                 } else if group_writer(kind, parameter) {
                                     writes.push(group);
                                 }
                             }
                             named.push((name.clone(), arg.term.clone()));
                         }
                         (None, None) => {
                             let names = kind.slots.iter().map(|slot| slot.name.clone()).chain(kind.parameters.iter().map(|parameter| parameter.name.clone())).collect::<Vec<_>>();
                             self.error(&arg.form, "E_UNKNOWN_PARAM", &(format!("{} has no parameter :{}.", short, name) + &catalog::suggestion(name, &names)));
                         }
                     }
                 }
             }
         }
     }

     for slot in &kind.slots {
         if slot.required && !slot.rest && !seen.contains(&slot.name) {
             if slot_ty(Some(slot), kind) == ty::geometry() {
                 named.push((slot.name.clone(), make_term(form, ty::geometry(), Node::Nil)));
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
             outputs => Ty::Record(outputs.iter().map(|(name, port)| (name.clone(), port_ty(port))).collect()),
         }
     };
     let values = args.iter().map(|arg| arg.info.clone()).collect::<Vec<_>>();
     let mut value = Info::derive(out.clone(), &values);
    value.groups = groups_in.iter().cloned().chain(writes.iter().cloned()).collect();
     value.packed.clear();
     value.approx_sources.clear();
     (make_term(form, out, Node::Call { kind: kind.qualified.clone(), ctx: kind.context.clone(), args: named }), value)
 }
 fn link_path(&mut self, path: Path, producer: &Path) {
     let next = PackedForm::Candidate([producer.clone()].into_iter().collect());
     self.packed.entry(path).and_modify(|old| *old = Self::merge_packed(old.clone(), next.clone())).or_insert(next);
 }
 fn value_arg(&self, x: &Syntax, v: &Info) -> Arg {
     Arg { key: None, form: x.clone(), term: make_term(x, v.ty.clone(), Node::RefBinding("_".into(), Vec::new())), info: v.clone() }
 }
 fn hof(&mut self, cx: &Cx, x: &Syntax, name: &str, forms: &[Syntax]) -> (Term, Info) {
     let (lo, hi) = match name { "map" => (2, 4), "filter" | "sort-by" => (2, 2), _ => (3, 3) };
     let n = forms.len();
     if n < lo || n > hi {
         return self.fail_term(x, "E_ARITY", match name {
             "map" => "map is (map f list …) with 1–3 lists.",
             "filter" => "filter is (filter pred list).",
             "reduce" => "reduce is (reduce f init list).",
             _ => "sort-by is (sort-by key list).",
         });
     }
     let f_form = &forms[0];
     let (ft, fv) = if syntax::head(f_form) == Some("fn") { self.input(cx, f_form, "0") } else { self.fn_value(cx, f_form, name) };
     let reduce = name == "reduce";
     let init = if reduce { Some(self.input(cx, &forms[1], "1")) } else { None };
     if let Some((_, iv)) = &init { self.no_fn(x, &iv.ty, "The reduce accumulator"); }
     let first = if reduce { 2 } else { 1 };
     let mut lists: Vec<(Syntax, Term, Info)> = Vec::new();
     for (i, a) in forms[first..].iter().enumerate() {
         let (t, v) = self.input(cx, a, &(i + first).to_string());
         if ty::elem(&v.ty).is_none() && v.ty != Ty::Any {
             self.error(a, "E_TYPE", &format!("{name} iterates a list; got {}.", show(&v.ty)));
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
     if let Some((t, _)) = &init { terms.push(t.clone()); }
     terms.extend(lists.iter().map(|(_, t, _)| t.clone()));
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
             (HofKind::Filter, l0.ty.clone(), any_len || rv.live)
         }
         "sort-by" => {
             if !matches!(rv.ty, Ty::Int | Ty::Float | Ty::Bool | Ty::Any) { self.error(x, "E_TYPE", &format!("sort-by's key returns a number; it returns {}.", show(&rv.ty))); }
             (HofKind::SortBy, l0.ty.clone(), any_len)
         }
         _ => {
             let it = init.as_ref().map(|(_, iv)| iv.ty.clone()).unwrap_or(Ty::Any);
             self.no_fn(x, &rv.ty, "The reduce result");
             if !ty::fits(&rv.ty, &it) {
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
     let specialized = match &result.node { Node::CallFn { body: Some(body), .. } => (**body).clone(), _ => result.clone() };
     let eligible = packed_body(&self.ops, &specialized);
     let params: Option<Vec<(Syntax, Option<Ty>)>> = match fv.callable.as_deref() {
         Some(Callable::Local(c)) => Some(c.params.clone()),
         Some(Callable::Def(def)) => self.defs.get(def).map(|d| d.params.iter().map(|(pn, pty, _)| (syntax::make(SNode::Sym(pn.clone())), Some(pty.clone()))).collect()),
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
             match &t.ty {
                 Ty::Any => (),
                 Ty::Array(e) if **e == Ty::Any => (),
                 ty if packed_array(ty) => (),
                 ty => reasons.push(err("E_PACKED_TYPE", format!("Packed source requires Array Float/Vec2/Vec3/Vec4, got {}.", show(ty)), Some(t.form.span.clone()))),
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
     if reasons.is_empty() { self.add_root(x, cx.path.clone(), None); } else { for r in reasons { self.add_root(x, cx.path.clone(), Some(r)); } }
     self.link_body(&specialized, &cx.path);
     match fv.callable.as_deref() {
         Some(Callable::Local(c)) => { let cid = c.path.clone(); self.link_path(cid, &cx.path); }
         Some(Callable::Def(def)) => self.link_path(vec![format!("def:{def}")], &cx.path),
         _ => (),
     }
     v.live_len = live_len;
     v.live = v.live || live_len;
     v.packed = [cx.path.clone()].into_iter().collect();
     v.approx = kind == HofKind::Map && packed_array(&out) && inputs_fit && lists.iter().all(|(_, _, l)| packed_array(&l.ty)) && eligible;
     (make_term(x, out, Node::Hof(kind, terms)), v)
 }
 fn iterate(&mut self,cx:&Cx,f:&Syntax,forms:&[Syntax])->(Term,Info){if forms.len()!=3&&forms.len()!=5{return self.fail_term(f,"E_ARITY","iterate is (iterate n init (fn [x] body)) with optional :until.")}let(n,nv)=self.input(cx,&forms[0],"0");if !ty::fits(&nv.ty,&Ty::Int)&&nv.ty!=Ty::Any{self.error(&forms[0],"E_TYPE","iterate's count is an int.")}let(init,iv)=self.input(cx,&forms[1],"1");let(ft,fv)=self.fn_value(cx,&forms[2],"iterate");let arg=Arg{key:None,form:forms[1].clone(),term:init.clone(),info:iv.clone()};let(body,bv)=self.apply_callable(cx,f,&fv,std::slice::from_ref(&arg));if !ty::fits(&bv.ty,&iv.ty){self.error(f,"E_TYPE","iterate's function returns the input type.")}let mut terms=vec![ft,init,n];let mut vs=vec![nv,iv.clone(),fv,bv.clone()];if forms.len()==5{if !matches!(&forms[3].node,SNode::Kw(k) if k=="until"){self.error(&forms[3],"E_ARGS","iterate's optional clause is :until.")}let(gt,gv)=self.fn_value(cx,&forms[4],"iterate :until");let next=Arg{key:None,form:forms[1].clone(),term:body,info:bv.clone()};let(_,uv)=self.apply_callable(cx,f,&gv,&[arg,next]);if !ty::fits(&uv.ty,&Ty::Bool){self.error(&forms[4],"E_TYPE","iterate's :until returns bool.")}terms.push(gt);vs.push(gv);vs.push(uv)}let out=ty::join(&iv.ty,&bv.ty).unwrap_or(iv.ty);if shape_ty(&out){self.error(f,"E_ARRAY_TYPE","iterate carries data; it cannot create graph nodes.")}self.add_root(f,cx.path.clone(),Some(err("E_GPU_FORM","GPU kernels require collecting maps.".into(),Some(f.span.clone()))));let mut v=Info::derive(out.clone(),&vs);v.packed.insert(cx.path.clone());(make_term(f,out,Node::Hof(HofKind::Iterate,terms)),v)}
 fn need(&mut self, a: &Arg, what: &str, want: &Ty) -> bool {
     if ty::fits(&a.info.ty, want) || (a.info.ty == Ty::Fn(None) && a.info.callable.is_none() && matches!(want, Ty::Fn(_))) { return true; }
     self.error(&a.form, "E_TYPE", &format!("{what}: expected {}, got {}.", show(want), show(&a.info.ty)));
     false
 }
 fn ref_form(&mut self, cx: &Cx, x: &Syntax, forms: &[Syntax]) -> (Term, Info) {
     let Some(Syntax { node: SNode::Sym(g), .. }) = forms.first() else { return self.fail_term(x, "E_ARGS", "ref takes a graph name, then :input value pairs.") };
     let Some(sg) = self.graphs.get(g).cloned() else { return self.fail_term(&forms[0], "E_UNKNOWN_GRAPH", &format!("Unknown graph reference: {g}.")) };
     if cx.in_def { return self.fail_term(x, "E_REF_IN_DEF", "Reusable functions cannot capture project graphs; add an explicit parameter."); }
     let gv = self.graph_info(x, g);
     let mut overrides: Vec<(String, Term)> = Vec::new();
     let mut vs = vec![gv.clone()];
     let mut rest = &forms[1..];
     loop {
         match rest {
             [Syntax { node: SNode::Kw(k), .. }, value, more @ ..] => {
                 let kf = &rest[0];
                 match sg.params.iter().find(|(n, _, _)| n == k) {
                     None => {
                         let inputs = if sg.params.is_empty() { "none".to_owned() } else { sg.params.iter().map(|(n, _, _)| n.clone()).collect::<Vec<_>>().join(", ") };
                         self.error(kf, "E_UNKNOWN_PARAM", &format!("Graph {g} has no input :{k}. Inputs: {inputs}."));
                     }
                     Some((_, want, _)) => {
                         let (t, v) = self.infer(cx, value);
                         self.exact_sink(cx, value, &format!("ref {g} :{k}"), &v);
                         self.no_fn(value, &v.ty, &format!("Input :{k} of {g}"));
                         let arg = self.value_arg(value, &v);
                         self.need(&arg, &format!(":{k} of {g}"), want);
                         overrides.push((k.clone(), t));
                         vs.push(v);
                     }
                 }
                 rest = more;
             }
             [] => break,
             [y, ..] => { self.error(y, "E_ARGS", "ref takes a graph name, then :input value pairs."); break; }
         }
     }
     let out = context::result(sg.ctx);
     let mut info = Info::derive(out.clone(), &vs);
     info.groups.clear();
     info.approx = gv.approx;
     (make_term(x, out, Node::GraphRef { graph: g.clone(), inputs: overrides }), info)
 }
 fn with_error(&mut self,cx:&Cx,f:&Syntax,forms:&[Syntax])->(Term,Info){
     if forms.len()!=2{return self.fail_term(f,"E_ARITY","with_error takes a protected value and a pure error mapper.")}
     let(protected,protected_info)=self.input(cx,&forms[0],"0");
     let(mapper_form,mapper_info)=self.fn_value(cx,&forms[1],"with_error");
     let payload=Ty::Record(vec![("code".into(),Ty::Text),("message".into(),Ty::Text),("cause".into(),Ty::List(Box::new(Ty::Text)))]);
     let argument=Arg{key:None,form:forms[1].clone(),term:make_term(&forms[1],payload.clone(),Node::RefBinding("_".into(),Vec::new())),info:Info::leaf(payload.clone())};
     let(mapper,mapper_result)=self.apply_callable(cx,f,&mapper_info,std::slice::from_ref(&argument));
     if !ty::fits(&mapper_result.ty,&payload){self.error(&forms[1],"E_DIAGNOSTIC_MAP","An error mapper takes and returns a diagnostic record.")}
     self.no_fn(&forms[0],&protected_info.ty,"A protected value");
     if cooked(&protected_info.ty)&&protected_info.ty!=ty::geometry()&&protected_info.ty!=ty::image(){self.error(&forms[0],"E_ERROR_SCOPE","An error boundary completes a direct cooked result, not a cooked container.")}
     if mapper_result.live||mapper_result.state_dep{self.error(&forms[1],"E_DIAGNOSTIC_MAP","An error mapper is pure and independent of frame/state.")}
     let result=Info::derive(protected_info.ty.clone(),&[protected_info,mapper_result]);
     (make_term(f,result.ty.clone(),Node::WithError(Box::new(protected),Box::new(mapper))),result)
 }
 fn error_rows(&mut self,cx:&Cx,f:&Syntax,forms:&[Syntax])->(Term,Info){let(rows_form,policy,mf)=if forms.len()==2{(&forms[0],DiagnosticPolicy::First,&forms[1])}else if forms.len()==4&&matches!(&forms[1].node,SNode::Kw(k) if k=="policy"){let p=match &forms[2].node{SNode::Str(s) if s==b"range-last"=>DiagnosticPolicy::RangeLast,SNode::Str(s) if s==b"first"=>DiagnosticPolicy::First,_=>{self.error(&forms[2],"E_DIAGNOSTIC_POLICY","Diagnostic policy is literal first or range-last.");DiagnosticPolicy::First}};(&forms[0],p,&forms[3])}else{return self.fail_term(f,"E_ARGS","error_rows takes rows, a pure diagnostic mapper, and optional :policy text.")};let(rows,rv)=self.input(cx,rows_form,"0");if rv.ty!=Ty::Array(Box::new(Ty::Int)){self.error(rows_form,"E_DIAGNOSTIC_ROWS","Diagnostic rows require an integer 0/1 array.")}let(mt,mv)=self.fn_value(cx,mf,"error_rows");let arg=Arg{key:None,form:mf.clone(),term:mt.clone(),info:Info::leaf(Ty::Int)};let(mapper,mapped)=self.apply_callable(cx,f,&mv,&[arg]);let out=ty::geometry();(make_term(f,out.clone(),Node::ErrorRows{rows:Box::new(rows),policy,mapper:Box::new(mapper)}),Info::derive(out,&[rv,mapped]))}
 fn graph_info(&mut self, x: &Syntax, name: &str) -> Info {
     if let Some(v) = self.ginfo.get(name) { return v.clone(); }
     let Some(sg) = self.graphs.get(name).cloned() else { return Info::leaf(Ty::Any) };
     if self.graph_stack.iter().any(|entry| entry == name) {
         let mut cycle = self.graph_stack.clone();
         cycle.push(name.to_owned());
         self.error(x, "E_GRAPH_CYCLE", &format!("Graph cycle: {}.", cycle.join(" → ")));
         return Info::leaf(context::result(sg.ctx));
     }
     self.graph_stack.push(name.to_owned());
     let cx0 = Cx { env: BTreeMap::new(), ctx: sg.ctx, path: vec![name.to_owned()], scope: name.to_owned(), in_def: false, stack: Vec::new(), depth: 0, zone: None, zbody: false };
     let mut env: BTreeMap<String, Info> = BTreeMap::new();
     let mut inputs = Vec::new();
     for (n, t, df) in &sg.params {
         let mut cx = cx0.clone();
         cx.env = env.clone();
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
         self.mark(vec![name.to_owned(), format!(":{n}")], &v);
         env.insert(n.clone(), v);
         inputs.push((n.clone(), t.clone(), dt));
     }
     let mut cx = cx0.clone();
     cx.env = env;
     let (bt, bv) = self.body(&cx, &sg.body);
     let want = context::result(sg.ctx);
     if !ty::fits(&bv.ty, &want) {
         self.error(&sg.body, "E_TYPE", &format!("{name} must return {}, but its result is {}.", show(&want), show(&bv.ty)));
     }
     self.graph_stack.pop();
     self.graph_terms.insert(name.to_owned(), Graph { name: name.to_owned(), context: sg.ctx, inputs, body: bt, form: sg.form.clone() });
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
            || self.declared_names.contains(name)
            || (head || !macros::valid_name(name))
                && (context::all().iter().any(|c| op::find(&self.ops, name, c).is_some())
                    || match catalog::resolve_kind(&self.catalog, &context::sop(), name) {
                        Ok(_) => true,
                        Err((code, _)) => code == "E_WRONG_CONTEXT" || code == "E_AMBIGUOUS",
                    })
    }

    fn metadata(&mut self, form: &Syntax, allowed: &[&str]) {
        for metadata in &form.meta {
            if !allowed.contains(&metadata.as_str()) {
                self.warn(form, "W_UNKNOWN_META", &format!("Unknown metadata ^:{metadata} here; it is ignored."));
            }
        }
    }

    fn parse_signature(&mut self, form: &Syntax, head: &str, name: &str) -> Option<Sig> {
        let SNode::List(items) = &form.node else {
            self.error(form, "E_SHAPE", "Expected a graph or defn declaration.");
            return None;
        };
        if items.len() < 5 || !matches!(&items[2].node, SNode::Kw(key) if key == "context") {
            self.error(form, "E_SHAPE", &format!("Invalid shape for {head}: expected ({head} {name} :context ctx [params] body)."));
            return None;
        }
        let SNode::Sym(context_name) = &items[3].node else {
            self.error(form, "E_CONTEXT_UNKNOWN", "Unknown context.");
            return None;
        };
        let ctx = match context::of_string(context_name) {
            Ok(ctx) => ctx,
            Err(problem) => { self.diags.push(problem); return None; }
        };
        let (parameters, body): (&[Syntax], &Syntax) = if head == "graph" && items.len() == 5 {
            (&[], &items[4])
        } else if let SNode::Vec(parameters) = &items[4].node {
            if items.len() != 6 {
                self.error(form, "E_SHAPE", &format!("Invalid shape for {head}: expected ({head} {name} :context ctx [params] body)."));
                return None;
            }
            (parameters, &items[5])
        } else {
            self.error(form, "E_SHAPE", &format!("Invalid shape for {head}: expected ({head} {name} :context ctx [params] body)."));
            return None;
        };
        if head == "defn" && items.len() != 6 {
            self.error(form, "E_SHAPE", &format!("defn {name} needs a parameter vector."));
            return None;
        }
        let mut params = Vec::new();
        let mut seen = BTreeSet::new();
        for parameter in parameters {
            let SNode::List(parts) = &parameter.node else {
                self.error(parameter, "E_PARAM", &format!("Invalid parameter in {name}. Each is (name : type default?)."));
                continue;
            };
            if !(3..=4).contains(&parts.len())
                || !matches!(&parts[0].node, SNode::Sym(_))
                || !matches!(&parts[1].node, SNode::Sym(colon) if colon == ":")
            {
                self.error(parameter, "E_PARAM", &format!("Invalid parameter in {name}. Each is (name : type default?)."));
                continue;
            }
            let SNode::Sym(param_name) = &parts[0].node else { continue };
            let Some(param_ty) = ty::of_syntax(&parts[2]) else {
                self.error(parameter, "E_PARAM", &format!("Invalid parameter in {name}. Each is (name : type default?)."));
                continue;
            };
            let default = parts.get(3).cloned();
            if !macros::valid_name(param_name) || self.reserved(param_name) || !seen.insert(param_name.clone()) {
                self.error(parameter, "E_PARAM", &format!("Invalid or repeated parameter in {name}."));
                continue;
            }
            if ty::has_fn(&param_ty) && (param_ty != Ty::Fn(None) || head == "graph") {
                self.error(parameter, "E_FN_ESCAPES", &format!("{name} input {param_name}: a function value cannot be stored or returned (E_FN_ESCAPES)."));
                continue;
            }
            if param_ty == Ty::Fn(None) && default.is_some() {
                self.error(parameter, "E_PARAM", &format!("{name} input {param_name}: a fn input has no default; callers pass a function."));
                continue;
            }
            if head == "graph" && default.is_none() {
                self.error(parameter, "E_INPUT_DEFAULT", &format!("Graph input {param_name} of {name} needs a default, so the graph runs on its own."));
            }
            params.push((param_name.clone(), param_ty, default));
        }
        Some(Sig { name: name.to_owned(), ctx, params, body: body.clone(), form: form.clone() })
    }

    fn check_def_body(&mut self, name: &str, own: bool) {
        if own && !self.checked_defs.insert(name.to_owned()) {
            return;
        }
        let Some(definition) = self.defs.get(name).cloned() else { return };
        let mut env = BTreeMap::new();
        let mut inputs = Vec::new();
        for (input, want, default) in &definition.params {
            let value = if let Some(default) = default {
                let cx = Cx {
                    env: BTreeMap::new(),
                    ctx: definition.ctx.clone(),
                    path: vec![format!("def:{name}"), format!(":{input}")],
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
            env.insert(input.clone(), Info::leaf(want.clone()));
            inputs.push((input.clone(), want.clone(), value));
        }
        let cx = Cx {
            env,
            ctx: definition.ctx.clone(),
            path: vec![format!("def:{name}")],
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
            make_term(&definition.body, Ty::Any, Node::Nil)
        };
        self.def_terms.insert(name.to_owned(), Graph {
            name: name.to_owned(),
            context: definition.ctx,
            inputs,
            body,
            form: definition.form,
        });
    }

    fn contains_body_context_op(term: &Term) -> bool {
        let mut todo = vec![term.clone()];
        while let Some(term) = todo.pop() {
            if matches!(&term.node, Node::Op { op, .. } if BODY_CONTEXT_OPS.contains(&op.as_str())) {
                return true;
            }
            todo.extend(children(&term));
        }
        false
    }

    fn pure_boundary(&mut self, cx: &Cx, mapper: bool, code: &str, message: &str, root: &Term) {
        let mut visited = BTreeSet::new();
        let mut todo = vec![root.clone()];
        while let Some(term) = todo.pop() {
            if mapper && cooked(&term.ty) {
                self.error(&term.form, code, message);
            }
            match &term.node {
                Node::State { .. } | Node::GraphRef { .. } => self.error(&term.form, code, message),
                Node::Time if mapper => self.error(&term.form, code, message),
                Node::Call { ctx: call_context, .. } if mapper || (*call_context != context::sop() && *call_context != context::image()) => {
                    self.error(&term.form, code, message);
                }
                Node::Op { op: name, .. } => {
                    let operator = op::find(&self.ops, name, &cx.ctx);
                    let allowed = operator.as_ref().is_some_and(|operator| {
                        if mapper {
                            operator.ctx == context::value() && !operator.live && !BODY_CONTEXT_OPS.contains(&operator.name.as_str())
                        } else {
                            operator.ctx == context::value() || operator.ctx == context::sop() || operator.ctx == context::image()
                        }
                    });
                    if !allowed {
                        self.error(&term.form, code, message);
                    }
                }
                Node::RefBinding(name, _) if mapper => {
                    if cx.env.get(name).is_some_and(|value| value.live || value.state_dep || cooked(&value.ty)) {
                        self.error(&term.form, code, message);
                    }
                }
                Node::FnRef(name) => self.pure_helper(cx, mapper, code, message, &term, name, &mut visited, &mut todo),
                Node::CallFn { fn_name, body: None, .. } => self.pure_helper(cx, mapper, code, message, &term, fn_name, &mut visited, &mut todo),
                Node::ErrorRows { .. } if mapper => self.error(&term.form, code, message),
                _ => (),
            }
            let nested = children(&term);
            todo.extend(nested);
        }
    }


    fn pure_helper(
        &mut self,
        cx: &Cx,
        mapper: bool,
        code: &str,
        message: &str,
        form: &Term,
        name: &str,
        visited: &mut BTreeSet<String>,
        todo: &mut Vec<Term>,
    ) {
        if !visited.insert(name.to_owned()) {
            return;
        }
        if let Some(definition) = self.def_terms.get(name) {
            todo.push(definition.body.clone());
            return;
        }
        let operator = op::find(&self.ops, name, &cx.ctx);
        if operator.is_some_and(|operator| {
            operator.ctx == context::value()
                && (!mapper || !operator.live)
                && !BODY_CONTEXT_OPS.contains(&operator.name.as_str())
        }) {
            return;
        }
        self.error(&form.form, code, message);
    }
    fn check_node_cycles(&mut self) {
        let definitions = self.nodes.iter().map(|node| (node.kind.qualified.clone(), node.clone())).collect::<BTreeMap<_, _>>();
        fn visit(
            checker: &mut Checker,
            name: &str,
            stack: &mut Vec<String>,
            visited: &mut BTreeSet<String>,
            definitions: &BTreeMap<String, NodeDefinition>,
        ) {
            if let Some(position) = stack.iter().position(|entry| entry == name) {
                if let Some(node) = definitions.get(name) {
                    let mut cycle = stack[position..].to_vec();
                    cycle.push(name.to_owned());
                    checker.error(&node.body.form, "E_RECURSION", &format!("Recursive node call: {}.", cycle.join(" → ")));
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
        for name in self.node_order.clone() {
            visit(self, &name, &mut Vec::new(), &mut visited, &definitions);
        }
    }

    fn phase(&mut self, forms: &[Syntax]) -> (Vec<String>, Vec<String>) {
        let mut declarations = Vec::new();
        let mut seen = BTreeSet::new();
        let mut graph_names = Vec::new();
        let mut def_names = Vec::new();
        let mut node_names = Vec::new();
        for form in forms {
            self.metadata(form, &[]);
            let SNode::List(items) = &form.node else {
                self.error(form, "E_FORM", "Workspace children are graph, defn, defnode and defmacro forms.");
                continue;
            };
            let Some((SNode::Sym(head), SNode::Sym(name))) = items.get(0).zip(items.get(1)).map(|(head, name)| (&head.node, &name.node)) else {
                self.error(form, "E_FORM", "Workspace children are graph, defn, defnode and defmacro forms.");
                continue;
            };
            if !["graph", "defn", "defnode", "defmacro"].contains(&head.as_str()) {
                self.error(form, "E_FORM", "Workspace children are graph, defn, defnode and defmacro forms.");
                continue;
            }
            let valid = if head == "defnode" { defnode::valid_name(name) } else { macros::valid_name(name) };
            let duplicate_kind = head == "defnode" && self.catalog.kinds.iter().any(|kind| kind.qualified == *name);
            let operator_name = op::of_context(&self.ops, &context::value()).iter().any(|operator| operator.name == *name);
            let already_declared = self.declared_names.contains(name);
            if !valid || self.reserved(name) || !seen.insert(name.clone()) || already_declared || duplicate_kind || operator_name {
                self.error(form, "E_NAME", &format!("Invalid, reserved or duplicate name: {name}."));
                continue;
            }
            self.declared_names.insert(name.clone());
            declarations.push((head.clone(), name.clone(), form.clone()));
        }

        for (head, name, form) in &declarations {
            if head != "defnode" {
                continue;
            }
            match defnode::parse(form) {
                Ok(definition) => {
                    if !self.catalog.kinds.iter().any(|kind| kind.qualified == definition.kind.qualified) {
                        self.catalog.kinds.push(definition.kind.clone());
                    }
                    self.node_defs.insert(name.clone(), definition);
                    self.node_order.push(name.clone());
                    node_names.push(name.clone());
                }
                Err(problem) => self.diags.push(problem),
            }
        }

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

        for (head, name, form) in &declarations {
            if head != "graph" && head != "defn" {
                continue;
            }
            let Some(signature) = self.parse_signature(form, head, name) else { continue };
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

        for name in &def_names {
            self.check_def_body(name, !self.call_sites);
        }

        for name in node_names {
            let Some(definition) = self.node_defs.get(&name).cloned() else { continue };
            let env = definition.inputs.iter().map(|(input, ty, _)| (input.clone(), Info::leaf(ty.clone()))).collect();
            let base = Cx {
                env,
                ctx: context::sop(),
                path: vec![format!("node:{name}")],
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
                    cx.env.clear();
                    cx.path.push(format!(":{input}"));
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
                if Self::contains_body_context_op(&term) {
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
                cx.env.clear();
                let (term, value) = self.fn_value(&cx, form, "normalizer");
                let signature = FnSignature { params: vec![Ty::Text], result: Ty::Text };
                let argument = Arg { key: None, form: form.clone(), term, info: value };
                let normalized = self.instantiate_fn(&cx, &argument, &signature, false);
                if normalized.info.ty != Ty::Fn(Some(Box::new(signature))) {
                    self.error(form, "E_TYPE", "An encoded normalizer takes one text and returns text.");
                }
                self.pure_boundary(&cx, true, "E_PARAM_NORMALIZE", "An encoded normalizer is pure: no frame, state, context or cooked values.", &normalized.term);
                normalizers.push((input.clone(), normalized.term));
            }
            self.nodes.push(NodeDefinition {
                kind: definition.kind,
                slots: definition.slots,
                keyword_inputs: definition.keyword_inputs,
                fields: definition.fields,
                label: definition.label,
                category: definition.category,
                description: definition.description,
                operation: definition.operation,
                version: definition.version,
                body: Graph { name: name.clone(), context: context::sop(), inputs, body, form: definition.form },
                validations,
                normalizers,
            });
        }
        self.check_node_cycles();
        for name in &graph_names {
            if let Some(signature) = self.graphs.get(name) {
                let form = signature.form.clone();
                self.graph_info(&form, name);
            }
        }
        (graph_names, self.def_order.clone())
    }
}

fn catalog_term(term: &Term) -> catalog::Term {
    let ty = ty_port(&term.ty);
    let node = match &term.node {
        Node::Lit(value) => catalog::TermNode::Literal(value.clone()),
        Node::Text(bytes) => catalog::TermNode::Literal(param::Value::Text(bytes.clone())),
        Node::Nil => catalog::TermNode::Nil,
        Node::Vec(terms) => catalog::TermNode::Vector(terms.iter().map(|component| { let mut c = catalog_term(component); if c.ty.is_none() { c.ty = Some(port_type::PortType::Float); } c }).collect()),
        _ => catalog::TermNode::Reference("_".into(), "_".into()),
    };
    catalog::Term { node, ty }
}
fn literal_param(term: &Term) -> Option<param::Value> {
    match &term.node {
        Node::Lit(value) => Some(value.clone()),
        Node::Text(bytes) => Some(param::Value::Text(bytes.clone())),
        Node::Expanded { body, .. } => literal_param(body),
        _ => None,
    }
}

pub fn validate_parameter(
    ty_override: Option<Ty>,
    report: &mut dyn FnMut(Severity, &str, &str),
    parameter: &catalog::Parameter,
    term: &Term,
) {
    let want = parameter_ty(parameter);
    let have = ty_override.unwrap_or_else(|| term.ty.clone());
    if have == Ty::Any {
        return;
    }
    if !ty::fits(&have, &want) {
        report(
            Severity::ErrorSeverity,
            "E_TYPE",
            &format!(":{} takes {}, but this is {}", parameter.name, show(&want), show(&have)),
        );
        return;
    }
    match (&want, &term.node) {
        (Ty::Color, Node::Text(bytes)) => {
            if !hex_colour(bytes) {
                let text = lisp::flat(&syntax::make(SNode::Str(bytes.clone())));
                report(
                    Severity::ErrorSeverity,
                    "E_TYPE",
                    &format!(":{} is a colour: write \"#rrggbb\" or a vec3, not {text}", parameter.name),
                );
            }
        }
        (Ty::Color, _) => {
            if have == Ty::Vec3 {
                let mut typed = parameter.clone();
                typed.ty = Some(port_type::PortType::Vec3);
                catalog::validate_parameter(report, &typed, &catalog_term(term));
            }
        }
        (Ty::Vec3, _) if have != Ty::Vec3 => (),
        (Ty::Text, Node::Text(_)) => catalog::validate_parameter(report, parameter, &catalog_term(term)),
        (Ty::Text, _) => (),
        _ => catalog::validate_parameter(report, parameter, &catalog_term(term)),
    }
}

pub fn prelude(
    ops: &[op::Op],
    library: bool,
    call_sites: bool,
    catalog: &catalog::Catalog,
    forms: &[Syntax],
) -> Prelude {
    let mut checker = Checker::new(catalog.clone(), ops, library, call_sites);
    checker.phase(forms);
    Prelude { checker }
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
        let mut checker = prelude.checker.clone();
        checker.ops = ops.to_vec();
        checker.library = library;
        checker.call_sites = call_sites;
        checker
    } else {
        Checker::new(catalog.clone(), ops, library, call_sites)
    };
    let workspace_form = match forms {
        [] => return (None, vec![err("E_NO_WORKSPACE", "Expected (workspace name …).".into(), None)]),
        [form] => form,
        [_, form, ..] => return (None, vec![err("E_ONE_WORKSPACE", "A file holds one workspace form.".into(), Some(form.span.clone()))]),
    };
    let SNode::List(items) = &workspace_form.node else {
        return (None, vec![err("E_WORKSPACE", "Expected (workspace name …).".into(), Some(workspace_form.span.clone()))]);
    };
    if items.first().and_then(|form| match &form.node { SNode::Sym(name) => Some(name.as_str()), _ => None }) != Some("workspace") {
        checker.error(workspace_form, "E_WORKSPACE", "Expected (workspace name …).");
        return (None, checker.diags);
    }
    let Some(SNode::Sym(name)) = items.get(1).map(|form| &form.node) else {
        checker.error(workspace_form, "E_WORKSPACE", "Expected (workspace name …).");
        return (None, checker.diags);
    };
    if !macros::valid_name(name) {
        checker.error(workspace_form, "E_WORKSPACE", "Expected (workspace name …).");
        return (None, checker.diags);
    }
    checker.metadata(workspace_form, &["allow-warnings"]);
    let children = &items[2..];
    let (graph_names, def_names) = checker.phase(children);
    if !library && graph_names.is_empty() {
        checker.error(workspace_form, "E_NO_GRAPH", "A workspace needs at least one graph.");
    }
    let mut diagnostics = Vec::new();
    for problem in checker.diags {
        if !diagnostics.iter().any(|old: &Diagnostic| {
            old.code == problem.code && old.span == problem.span && old.message == problem.message
        }) {
            diagnostics.push(problem);
        }
    }
    if diagnostics.iter().any(|problem| problem.severity == Severity::ErrorSeverity) {
        return (None, diagnostics);
    }
    let packed = checker.packed.into_iter().collect::<Vec<_>>();
    let approx_reasons = packed.iter().map(|(path, form)| {
        let reasons = match form {
            PackedForm::Refused(reasons) => reasons.clone(),
            PackedForm::Candidate(_) => vec![err("E_PACKED_PENDING", "Actual producer captures have not been qualified.".into(), None)],
        };
        (path.clone(), reasons)
    }).collect();
    let mut packed_roots = checker.roots;
    packed_roots.sort_by(|(a, p),(b, q)| p.cmp(q).then(a.id.cmp(&b.id)));
    let workspace = Workspace {
        name: name.clone(),
        graphs: graph_names.iter().filter_map(|name| checker.graph_terms.get(name).cloned()).collect(),
        defs: def_names.iter().filter_map(|name| checker.def_terms.get(name).cloned()).collect(),
        nodes: checker.nodes,
        macros: checker.macro_forms,
        source: forms.to_vec(),
        live: checker.live,
        invariant: checker.invariant,
        approx: Paths::new(),
        packed,
        packed_roots,
        approx_reasons,
        kind_fns: checker.kind_fns.into_iter().collect(),
        ops: checker.ops,
    };
    (Some(workspace), diagnostics)
}