//! Port of lib/flow_ir/core_lower.ml: frame lowering of residuals into structured core programs.
//! OCaml exceptions (`Unsupported`, `Diagnostic.Error`) are the `Stop` error; statement lists are
//! built by pushing (the OCaml code prepends and reverses); a scope is a statement list with its
//! cache, copied or fresh exactly where the OCaml code copies or creates a Hashtbl.
//! Argument evaluation order follows OCaml (right to left in literal lists) wherever it numbers locals.
#![allow(non_camel_case_types, non_snake_case, clippy::too_many_arguments)]

use crate::core_ir as C;
use crate::{fallback, packed};
use flow_check::op::{self, Reduction};
use flow_check::workspace::{self as W, HofKind, LoopKind, Node, Pattern, Path, Term};
use flow_eval as E;
use flow_syntax::packed_ops as PO;
use flow_syntax::syntax::Node as SNode;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{self as V, ResidualId, Value};
use flow_syntax::{context, Diagnostic};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

const MAX_ARRAY: i64 = (1i64 << 54) - 1;
const MAX_INT: i64 = i64::MAX >> 1;

/// OCaml's `Unsupported` and `Diagnostic.Error`.
#[derive(Clone, Debug)]
pub enum Stop { Unsupported(String), Diag(Diagnostic) }
impl From<Diagnostic> for Stop {
    fn from(d: Diagnostic) -> Stop { if d.code == "$unsupported" { Stop::Unsupported(d.message) } else { Stop::Diag(d) } }
}
type R<T> = Result<T, Stop>;
fn refusal(name: &str) -> Stop { Stop::Unsupported(name.to_owned()) }
/// A diagnostic that carries a refusal through code that returns diagnostics (an operator body's node callback).
fn refusal_diag(name: &str) -> Diagnostic {
    let mut d = V::fail("$unsupported", name);
    d.message = name.to_owned();
    d
}

pub type Trace = Arc<dyn Fn(&Result<packed::T, Vec<Diagnostic>>) + Send + Sync>;
pub type RegionAllowed = Rc<dyn Fn(&C::RegionT) -> bool>;
pub type LoweredHook = Rc<dyn Fn(ResidualId)>;

fn static_sop(r: ResidualId) -> bool {
    match E::Residual::residual_context(r) {
        Some(c) => c == context::sop() && !E::frame_dependent(&Value::Residual(r)) && !E::state_dependent(&Value::Residual(r)),
        None => false,
    }
}
/// a SOP cook lowers what its kernels capture as producers, frame-driven or not
fn sop_cook(r: ResidualId) -> bool { E::Residual::residual_context(r).is_some_and(|c| c == context::sop()) }

fn holds_arrays(ty: &Ty) -> bool {
    match ty { Ty::Array(_) => true, Ty::Record(fields) => fields.iter().any(|(_, t)| holds_arrays(t)), _ => false }
}
fn holds_array_residual(v: &Value) -> bool {
    match v {
        Value::Residual(r) => E::Residual::residual_ty(*r).is_some_and(|ty| holds_arrays(&ty)),
        Value::Fn(f) => E::Residual::function_bindings(*f).iter().any(|(_, v)| holds_array_residual(v)),
        Value::Record(fields) => fields.iter().any(|(_, v)| holds_array_residual(v)),
        _ => false,
    }
}

fn failure(d: &Diagnostic) -> C::Primitive {
    if d.span.is_none() && d.position.is_none() && d.cause.is_none() && d.hints.is_empty() && d.trace.is_empty() {
        C::Primitive::Fail(d.code.clone(), d.message.clone())
    } else {
        C::Primitive::Fail_diagnostic(d.clone())
    }
}

fn closure_free(v: &Value) -> bool {
    match v {
        Value::Fn(_) | Value::Residual(_) => false,
        Value::List(values) => values.iter().all(closure_free),
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().all(|(_, v)| closure_free(v)),
        _ => true,
    }
}

fn geometry_collection(ty: &Ty) -> bool { *ty == Ty::List(Box::new(ty::geometry())) }

fn term_refs(term: &Term) -> Vec<&Term> {
    match &term.node {
        Node::Vec(ts) | Node::Hof(_, ts) | Node::ListLit(ts) | Node::Str(ts) | Node::ListOp(_, ts) => ts.iter().collect(),
        Node::Call { args, .. } | Node::Op { args, .. } | Node::Record(args) | Node::GraphRef { inputs: args, .. } => args.iter().map(|(_, t)| t).collect(),
        Node::CallFn { args, body, .. } => args.iter().chain(body.iter().map(Box::as_ref)).collect(),
        Node::Let(bindings, body) => bindings.iter().map(|(_, t)| t).chain(std::iter::once(body.as_ref())).collect(),
        Node::Loop { accs, clauses, body, .. } => accs.iter().chain(clauses).map(|(_, t)| t).chain(std::iter::once(body.as_ref())).collect(),
        Node::State { init, step, .. } => vec![init.as_ref(), step.as_ref()],
        Node::WithError(a, b) => vec![a.as_ref(), b.as_ref()],
        Node::ErrorRows { rows, mapper, .. } => vec![rows.as_ref(), mapper.as_ref()],
        Node::If(a, b, c) => vec![a.as_ref(), b.as_ref(), c.as_ref()],
        Node::Cond(arms, d) => arms.iter().flat_map(|(a, b)| [a, b]).chain(std::iter::once(d.as_ref())).collect(),
        Node::Case(s, arms, d) => std::iter::once(s.as_ref()).chain(arms.iter().map(|(_, t)| t)).chain(std::iter::once(d.as_ref())).collect(),
        Node::Fn { body, capture, .. } => std::iter::once(body.as_ref()).chain(capture.iter().map(Box::as_ref)).collect(),
        Node::Get(v, _) | Node::Expanded { body: v, .. } => vec![v.as_ref()],
        Node::Bypass(call) => W::bypass_input_ref(call).into_iter().collect(),
        Node::Assoc(r, updates) => std::iter::once(r.as_ref()).chain(updates.iter().map(|(_, t)| t)).collect(),
        _ => Vec::new(),
    }
}
// OCaml compares the step's direct nodes physically: collect their addresses in the one view the lowering walks
fn state_step_nodes(term: &Term) -> HashSet<usize> {
    fn direct(term: &Term, out: &mut HashSet<usize>) {
        out.insert(term as *const Term as usize);
        match &term.node {
            Node::If(..) | Node::Cond(..) | Node::Case(..) => (),
            _ => for c in term_refs(term) { direct(c, out); },
        }
    }
    let mut out = HashSet::new();
    if let Node::State { step, .. } = &term.node { direct(step, &mut out); }
    out
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum CacheKey { Residual(i64), Cooked(String, i64), CookedList(Vec<i64>), Frame(u8) }
fn frame_key(f: C::FrameField) -> u8 {
    use C::FrameField::*;
    match f { Time => 0, Dt => 1, Index => 2, Tick => 3, Width => 4, Height => 5, Pointer_x => 6, Pointer_y => 7, Input => 8 }
}

type Env = BTreeMap<String, Lowered>;

#[derive(Clone, Debug)]
pub enum Lowered {
    Known(Value),
    Computed(i64, Ty),
    Record_fields(Vec<(String, Lowered)>),
    Record_choice(Box<Lowered>, Box<Lowered>, Box<Lowered>),
    Selected(Vec<Lowered>, Box<Lowered>),
    Callable(Callable),
}
use Lowered::*;

#[derive(Clone, Debug)]
pub enum Callable { Closure(Arc<Closure>), Named { name: String, identity: i64 } }
#[derive(Clone, Debug)]
pub struct Closure {
    params: Vec<(Pattern, Option<Ty>)>,
    body: Term,
    env: Env,
    instance: i64,
    prefix: Path,
    zone: Path,
    identity: i64,
}
fn identity_of(f: &Callable) -> i64 { match f { Callable::Closure(c) => c.identity, Callable::Named { identity, .. } => *identity } }

fn text(s: &str) -> Value { Value::Text(s.as_bytes().to_vec()) }
fn int(n: i64) -> Lowered { Known(Value::Int(n)) }
fn float(x: f64) -> Lowered { Known(Value::Float(x)) }
fn boolean(b: bool) -> Lowered { Known(Value::Bool(b)) }

/// A statement list with the cache of what it already computed (OCaml's `statements cache`).
pub struct Scope { st: Vec<C::Statement>, cache: HashMap<CacheKey, Lowered> }
impl Scope {
    fn fresh() -> Scope { Scope { st: Vec::new(), cache: HashMap::new() } }
    fn fork(&self) -> Scope { Scope { st: Vec::new(), cache: self.cache.clone() } }
}

fn shipped(o: &op::Op) -> bool {
    op::all().iter().any(|s| s.name == o.name && Arc::ptr_eq(&s.body, &o.body))
}

fn packed_candidate(x: &Term) -> (bool, bool) {
    fn text_input(terms: &[&Term]) -> bool { terms.iter().any(|t| t.ty == Ty::Array(Box::new(Ty::Text))) }
    fn geometry_accumulator(ty: &Ty) -> bool {
        *ty == ty::geometry() || matches!(ty, Ty::Record(fields) if fields.iter().any(|(_, t)| geometry_accumulator(t)))
    }
    let arr = |t: &Term| matches!(t.ty, Ty::Array(_));
    fn rest(ts: &[Term]) -> Vec<&Term> { ts.iter().skip(1).collect() }
    fn clause_terms(clauses: &[(Pattern, Term)]) -> Vec<&Term> { clauses.iter().map(|(_, t)| t).collect() }
    let numeric = !geometry_collection(&x.ty)
        && x.ty != Ty::Array(Box::new(Ty::Text))
        && x.ty != Ty::Text
        && match &x.node {
            Node::Loop { kind: LoopKind::Fold, .. } if geometry_accumulator(&x.ty) => false,
            Node::Hof(_, ts) if !ts.is_empty() && text_input(&rest(ts)) => false,
            Node::Loop { clauses, .. } if text_input(&clause_terms(clauses)) => false,
            Node::Hof(HofKind::Map | HofKind::Reduce, ts) if !ts.is_empty() => ts[1..].iter().any(arr),
            Node::Loop { clauses, .. } => clauses.iter().any(|(_, t)| arr(t)),
            Node::Op { op, .. } if op == "array/sum" => true,
            _ => false,
        };
    let text = match &x.node {
        Node::Hof(HofKind::Map, ts) if !ts.is_empty() => x.ty == Ty::Array(Box::new(Ty::Text)) || text_input(&rest(ts)),
        Node::Loop { kind: LoopKind::For, clauses, .. } => x.ty == Ty::Array(Box::new(Ty::Text)) || text_input(&clause_terms(clauses)),
        _ => false,
    };
    (numeric, text)
}

/// What a draw lowering writes through: `value` is the local holding a constant or residual value.
/// `Err` carries a refusal name.
pub trait DrawContext {
    fn value(&mut self, source: &C::Source, value: &Value) -> Result<i64, String>;
    fn local(&mut self, ty: &Ty) -> i64;
    fn emit(&mut self, source: &C::Source, node: C::StatementNode);
}

fn constant_storage(mut program: C::Program) -> C::Program {
    let n = program.locals.len();
    let mut uses: Vec<Vec<Option<i64>>> = vec![Vec::new(); n];
    let mut writes = vec![0i64; n];
    fn unsafe_use(uses: &mut [Vec<Option<i64>>], n: i64) { uses[n as usize].push(None); }
    fn write(writes: &mut [i64], n: i64) { writes[n as usize] += 1; }
    fn scan(program: &C::Program, uses: &mut [Vec<Option<i64>>], writes: &mut [i64], st: &C::Statement) {
        use C::StatementNode as N;
        match &st.node {
            N::Assign(dst, expression) => {
                write(writes, *dst);
                match expression {
                    C::Expr::Local(src) if program.locals[*src as usize] == program.locals[*dst as usize] => uses[*src as usize].push(Some(*dst)),
                    C::Expr::Host_source { ty, args, .. } if ty::is_cooked(ty) => {
                        for (_, src) in args {
                            match &program.locals[*src as usize] { Ty::Array(_) => uses[*src as usize].push(Some(-1)), _ => unsafe_use(uses, *src) }
                        }
                    }
                    C::Expr::Constant(_) | C::Expr::Frame(_) | C::Expr::Call_index(_) | C::Expr::Source(_) => {}
                    C::Expr::Local(src) | C::Expr::Field(src, _) => unsafe_use(uses, *src),
                    C::Expr::Prim(_, args) | C::Expr::Fields(args) | C::Expr::Host_source { args, .. } | C::Expr::Source_descriptor { args, .. } => {
                        for (_, src) in args { unsafe_use(uses, *src); }
                    }
                    C::Expr::Vec(args) | C::Expr::Items(args) => for a in args { unsafe_use(uses, *a); },
                }
            }
            N::If { condition, .. } => unsafe_use(uses, *condition),
            N::Counted { first, last, count, index, item, stop, .. } => {
                for u in [*first, *last].iter().chain(stop.iter()) { unsafe_use(uses, *u); }
                for w in [*count, *index, *item] { write(writes, w); }
            }
            N::Iterate { sequence, index, item, .. } => {
                unsafe_use(uses, *sequence);
                for w in [*index, *item] { write(writes, w); }
            }
            N::Store { sequence, index, value } => for u in [*sequence, *index, *value] { unsafe_use(uses, u); },
            N::Invoke { args, result, .. } => {
                for a in args { unsafe_use(uses, *a); }
                write(writes, *result);
            }
            N::State_read { result, initial, .. } => { unsafe_use(uses, *initial); write(writes, *result); }
            N::State_peek { found, value, .. } => for w in [*found, *value] { write(writes, w); },
            N::State_write { value, .. } => unsafe_use(uses, *value),
            N::Diagnose_rows { rows, index, diagnostic, result, .. } => {
                for u in [*rows, *diagnostic] { unsafe_use(uses, u); }
                for w in [*index, *result] { write(writes, w); }
            }
            N::Protect { value, failure, mapped, result, .. } => {
                for u in [*value, *mapped] { unsafe_use(uses, u); }
                for w in [*failure, *result] { write(writes, w); }
            }
            N::Reset_call(_) => {}
            N::Draw(draw) => {
                use C::Draw as D;
                match draw {
                    D::Background(n) => unsafe_use(uses, *n),
                    D::Point { at, ink } => for u in [*at, *ink] { unsafe_use(uses, u); },
                    D::Mark { args, .. } | D::Batch { args, .. } | D::Path { args, .. } => for (_, n) in args { unsafe_use(uses, *n); },
                    D::Points { positions, ink } => for u in [*positions, *ink] { unsafe_use(uses, u); },
                    D::Text_run { at, text, ink, size } => for u in [*at, *text, *ink, *size] { unsafe_use(uses, u); },
                    D::Image { at, scale, angle, .. } => for u in [*at, *scale, *angle] { unsafe_use(uses, u); },
                    D::Push(_, n) => unsafe_use(uses, *n),
                    D::Pop => {}
                }
            }
        }
        for child in C::children(st) { scan(program, uses, writes, child); }
    }
    for st in &program.body { scan(&program, &mut uses, &mut writes, st); }
    for r in &program.results { uses[*r as usize].push(None); }
    for slot in &program.state_slots {
        for it in &slot.source.iter { if let C::Iteration::Local_index(n) = it { uses[*n as usize].push(None); } }
    }
    fn safe(writes: &[i64], uses: &[Vec<Option<i64>>], seen: &mut Vec<i64>, n: i64) -> bool {
        writes[n as usize] == 1
            && !seen.contains(&n)
            && !uses[n as usize].is_empty()
            && uses[n as usize].iter().all(|u| match u {
                Some(-1) => true,
                Some(next) => { seen.push(n); let ok = safe(writes, uses, seen, *next); seen.pop(); ok }
                None => false,
            })
    }
    fn rewrite(program: &C::Program, writes: &[i64], uses: &[Vec<Option<i64>>], st: &C::Statement) -> C::Statement {
        use C::StatementNode as N;
        let list = |xs: &Vec<C::Statement>| xs.iter().map(|s| rewrite(program, writes, uses, s)).collect::<Vec<_>>();
        let node = match &st.node {
            N::Assign(dst, C::Expr::Prim(p @ (C::Primitive::Array_int | C::Primitive::Array_text | C::Primitive::Array_float | C::Primitive::Array_vec3 | C::Primitive::Array_vec4), args))
                if safe(writes, uses, &mut Vec::new(), *dst) =>
            {
                N::Assign(*dst, C::Expr::Source_descriptor {
                    name: format!("$core/constant-array/{}", C::primitive_name(p)),
                    ty: program.locals[*dst as usize].clone(),
                    args: args.clone(),
                })
            }
            N::If { condition, yes, no } => N::If { condition: *condition, yes: list(yes), no: list(no) },
            N::Diagnose_rows { rows, policy, index, handler, diagnostic, result } => N::Diagnose_rows { rows: *rows, policy: *policy, index: *index, handler: list(handler), diagnostic: *diagnostic, result: *result },
            N::Protect { body, value, failure, handler, mapped, result } => N::Protect { body: list(body), value: *value, failure: *failure, handler: list(handler), mapped: *mapped, result: *result },
            N::Counted { first, last, count, index, item, limit, stop, init, body } => N::Counted { first: *first, last: *last, count: *count, index: *index, item: *item, limit: *limit, stop: *stop, init: list(init), body: list(body) },
            N::Iterate { sequence, index, item, limit, body } => N::Iterate { sequence: *sequence, index: *index, item: *item, limit: *limit, body: list(body) },
            N::State_read { slot, result, init, initial } => N::State_read { slot: *slot, result: *result, init: list(init), initial: *initial },
            node => node.clone(),
        };
        C::Statement { id: st.id, source: st.source.clone(), node }
    }
    let body: Vec<C::Statement> = program.body.iter().map(|s| rewrite(&program, &writes, &uses, s)).collect();
    program.body = body;
    program
}

type Lower<'a> = &'a mut dyn FnMut(&mut Lowerer, &mut Scope) -> R<Lowered>;
type LowerItem<'a> = &'a mut dyn FnMut(&mut Lowerer, &mut Scope, &Lowered) -> R<Lowered>;

pub struct Lowerer {
    options: packed::CompileOptions,
    strict: bool,
    region_allowed: RegionAllowed,
    has_draw: bool,
    initial: Option<Value>,
    root: Option<ResidualId>,
    root_ops: Option<Vec<op::Op>>,
    lowered_hook: LoweredHook,
    constants: Vec<C::Literal>,
    locals: Vec<Ty>,
    next_statement: i64,
    budget: i64,
    active: HashSet<i64>,
    calls: HashMap<i64, i64>,
    next_call: i64,
    next_function: i64,
    mixed: HashSet<i64>,
    current: Option<ResidualId>,
    step_nodes: HashSet<usize>,
    regions: Vec<Arc<C::RegionT>>,
    slots: Vec<C::StateSlot>,
    next_slot: i64,
    plane: C::Plane,
    geometry: bool,
}

fn statement(id: i64, source: &C::Source, node: C::StatementNode) -> C::Statement { C::Statement { id, source: source.clone(), node } }

fn known_values(args: &[(String, Lowered)]) -> Option<Vec<(String, Value)>> {
    let mut out = Vec::new();
    for (name, v) in args {
        match v {
            Known(v) if C::literal_of_value(v).is_some() => out.push((name.clone(), v.clone())),
            _ => return None,
        }
    }
    Some(out)
}

fn storage_argument(ty: &Ty) -> bool {
    match ty { Ty::Array(_) => true, Ty::Record(fields) => fields.iter().any(|(_, t)| storage_argument(t)), _ => false }
}

fn primitive_of_name(name: &str) -> Option<C::Primitive> {
    use C::Primitive as P;
    if let Some(op) = PO::binary(name) { return Some(P::Numeric(op)); }
    if let Some(op) = PO::unary(name) { return Some(P::Math(op)); }
    Some(match name {
        "exp" => P::Exp,
        "floor" => P::Floor,
        "ceil" => P::Ceil,
        "round" => P::Round,
        "int" => P::Int_convert,
        "float" => P::Float_convert,
        "equal?" => P::Equal,
        "text/blank?" => P::Text_blank,
        "text/trim" => P::Text_trim,
        "text/lower" => P::Text_lower,
        "text/repr" => P::Text_repr,
        "error" => P::Error,
        "fma" => P::Fma,
        "float/next_after" => P::Next_after,
        "float/mul_checked" => P::Checked_mul,
        "float/fma_checked" => P::Checked_fma,
        "text/table_decode" => P::Text_table_decode,
        "text/table_encode" => P::Text_table_encode,
        "text/split" => P::Text_split,
        "text/tokens" => P::Text_tokens,
        "text/to_float" => P::Text_to_float,
        "text/to_int" => P::Text_to_int,
        "array/grade" => P::Array_grade,
        "array/index_of" => P::Array_index_of,
        "array/compact" => P::Array_compact,
        "array/scan_sum" => P::Array_scan_sum,
        "array/scatter_add" => P::Array_scatter_add,
        "array/min" => P::Array_reduce(Reduction::Minimum),
        "array/max" => P::Array_reduce(Reduction::Maximum),
        "array/segment_sum" => P::Array_segment(Reduction::Sum),
        "array/segment_min" => P::Array_segment(Reduction::Minimum),
        "array/segment_max" => P::Array_segment(Reduction::Maximum),
        "array/segment_argmax" => P::Array_segment_argmax,
        "int/add" => P::Integer(PO::Integer::Iadd),
        "int/sub" => P::Integer(PO::Integer::Isub),
        "int/mul" => P::Integer(PO::Integer::Imul),
        "int/div" => P::Integer(PO::Integer::Idiv),
        "int/mod" => P::Integer(PO::Integer::Imod),
        "int/and" => P::Integer(PO::Integer::Iand),
        "int/or" => P::Integer(PO::Integer::Ior),
        "int/xor" => P::Integer(PO::Integer::Ixor),
        "int/shl" => P::Integer(PO::Integer::Ishl),
        "int/shr" => P::Integer(PO::Integer::Ishr),
        "int/lt" => P::Integer_compare(PO::Comparison::Ilt),
        "int/le" => P::Integer_compare(PO::Comparison::Ile),
        "int/gt" => P::Integer_compare(PO::Comparison::Igt),
        "int/ge" => P::Integer_compare(PO::Comparison::Ige),
        "int/eq" => P::Integer_compare(PO::Comparison::Ieq),
        "array/max_length" => P::Array_max_length,
        "array/range" => P::Array_range,
        "array/int" => P::Array_int,
        "array/text" => P::Array_text,
        "array/float" => P::Array_float,
        "array/vec3" => P::Array_vec3,
        "array/vec4" => P::Array_vec4,
        "array/nth" => P::Array_nth,
        "array/count" => P::Array_count,
        "array/slice" => P::Array_slice,
        "array/concat" => P::Array_concat,
        "value/rand" => P::Rand,
        "rand/at" => P::Rand_at,
        "noise3" => P::Noise3,
        _ => return None,
    })
}

fn num(b: PO::Binary) -> C::Primitive { C::Primitive::Numeric(b) }

impl Lowerer {
    fn local(&mut self, ty: &Ty) -> i64 {
        let n = self.locals.len() as i64;
        self.locals.push(ty.clone());
        n
    }
    fn number(&mut self) -> i64 {
        let n = self.next_statement;
        self.next_statement += 1;
        n
    }
    fn new_identity(&mut self) -> i64 {
        let n = self.next_function;
        self.next_function -= 1;
        n
    }
    fn call_slot(&mut self, identity: i64) -> i64 {
        if let Some(n) = self.calls.get(&identity) { return *n; }
        let n = self.next_call;
        self.next_call += 1;
        self.calls.insert(identity, n);
        n
    }
    fn push(&mut self, s: &mut Scope, source: &C::Source, node: C::StatementNode) {
        let id = self.number();
        s.st.push(statement(id, source, node));
    }
    fn root(&self) -> R<ResidualId> {
        self.root.ok_or_else(|| Stop::Diag(V::fail("E_INTERNAL", "root residual not available")))
    }
    fn ops(&mut self) -> R<Vec<op::Op>> {
        if self.root_ops.is_none() {
            let root = self.root()?;
            self.root_ops = Some(E::Residual::residual_ops(root));
        }
        Ok(self.root_ops.clone().unwrap())
    }
    fn find_op(&mut self, name: &str) -> R<Option<op::Op>> {
        let ops = self.ops()?;
        Ok(op::find(&ops, name, &context::value()))
    }
    fn view(&self, r: ResidualId) -> R<E::Residual::View> { Ok(E::Residual::residual_view(r)?) }

    fn emit(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, expression: C::Expr) -> Lowered {
        let n = self.local(ty);
        let mixed = *ty == Ty::Float
            && match &expression {
                C::Expr::Prim(C::Primitive::Numeric(_), _) | C::Expr::Prim(C::Primitive::Sequence_get, _) | C::Expr::Field(..) => true,
                C::Expr::Local(old) => self.mixed.contains(old),
                _ => false,
            };
        if mixed { self.mixed.insert(n); }
        self.push(s, source, C::StatementNode::Assign(n, expression));
        Computed(n, ty.clone())
    }
    fn emit_n(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, expression: C::Expr) -> i64 {
        match self.emit(s, source, ty, expression) { Computed(n, _) => n, _ => unreachable!() }
    }

    fn frame(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, field: C::FrameField) -> Lowered {
        let key = CacheKey::Frame(frame_key(field));
        if let Some(v) = s.cache.get(&key) { return v.clone(); }
        let value = self.emit(s, source, ty, C::Expr::Frame(field));
        s.cache.insert(key, value.clone());
        value
    }

    fn resolve(&mut self, s: &mut Scope, v: &Lowered) -> R<Lowered> {
        match v { Known(Value::Residual(r)) => self.residual(s, *r), v => Ok(v.clone()) }
    }

    fn ensure(&mut self, s: &mut Scope, source: &C::Source, v: &Lowered) -> R<i64> {
        match v {
            Computed(n, _) => Ok(*n),
            Callable(_) => Err(refusal("fn")),
            Selected(items, index) => {
                let ty = self.type_of(v);
                let chosen = self.select(s, source, &ty, items, index, &mut |_, _, v| Ok(v.clone()))?;
                self.ensure(s, source, &chosen)
            }
            Known(Value::Residual(r)) => {
                let l = self.residual(s, *r)?;
                self.ensure(s, source, &l)
            }
            Known(value @ (Value::Source(..) | Value::Struct(_, Ty::Array(_), _))) if closure_free(value) => {
                Ok(self.emit_n(s, source, &V::ty_of(value), C::Expr::Source(value.clone())))
            }
            Known(Value::Source(name, ty, fields)) | Known(Value::Struct(name, ty @ Ty::Array(_), fields)) => {
                // a capture inside a source is lowered here, so the program holds plain data only
                let mut args = Vec::new();
                for (field, v) in fields { args.push((field.clone(), self.ensure(s, source, &Known(v.clone()))?)); }
                Ok(self.emit_n(s, source, ty, C::Expr::Host_source { name: name.clone(), ty: ty.clone(), args }))
            }
            Known(Value::Deferred(ty, id)) if ty::is_cooked(ty) => {
                let key = CacheKey::Cooked(ty::to_string(ty), *id);
                let value = match s.cache.get(&key) {
                    Some(v) => v.clone(),
                    None => {
                        let value = self.emit(s, source, ty, C::Expr::Source(Value::Source("$core/cooked".into(), ty.clone(), vec![("id".into(), Value::Int(*id))])));
                        s.cache.insert(key, value.clone());
                        value
                    }
                };
                self.ensure(s, source, &value)
            }
            Known(Value::Record(fields)) => {
                let l = Record_fields(fields.iter().map(|(n, v)| (n.clone(), Known(v.clone()))).collect());
                self.ensure(s, source, &l)
            }
            Known(Value::List(items)) if C::literal_of_value(&Value::List(items.clone())).is_none() => {
                let ty = self.type_of(v);
                if items.len() > 4096 && !geometry_collection(&ty) { return Err(refusal("list_limit")); }
                let cooked = geometry_collection(&ty)
                    && items.iter().all(|i| match i { Value::Deferred(t, _) => *t == ty::geometry(), Value::No_geo => true, _ => false });
                let key = CacheKey::CookedList(items.iter().map(|i| match i { Value::Deferred(_, id) => *id, _ => i64::MIN }).collect());
                let cached = if cooked { s.cache.get(&key).cloned() } else { None };
                let result = match cached {
                    Some(r) => r,
                    None => {
                        let result = self.primitive(s, source, &ty, C::Primitive::Sequence_build, vec![("count", int(items.len() as i64))])?;
                        for (index, item) in items.iter().enumerate() {
                            self.store(s, source, &result, &int(index as i64), &Known(item.clone()))?;
                        }
                        if cooked { s.cache.insert(key, result.clone()); }
                        result
                    }
                };
                self.ensure(s, source, &result)
            }
            Record_choice(condition, yes, no) => {
                let condition = self.ensure(s, source, condition)?;
                let id = self.number();
                let result = self.local(&self.type_of(v));
                let arm = |l: &mut Lowerer, s: &mut Scope, value: &Lowered| -> R<Vec<C::Statement>> {
                    let mut body = s.fork();
                    let n = l.ensure(&mut body, source, value)?;
                    let sid = l.number();
                    body.st.push(statement(sid, source, C::StatementNode::Assign(result, C::Expr::Local(n))));
                    Ok(body.st)
                };
                let yes = arm(self, s, yes)?;
                let no = arm(self, s, no)?;
                s.st.push(statement(id, source, C::StatementNode::If { condition, yes, no }));
                Ok(result)
            }
            Record_fields(fields) => {
                let ty = Ty::Record(fields.iter().map(|(n, v)| (n.clone(), self.type_of(v))).collect());
                let mut out = Vec::new();
                for (name, v) in fields { out.push((name.clone(), self.ensure(s, source, v)?)); }
                Ok(self.emit_n(s, source, &ty, C::Expr::Fields(out)))
            }
            Known(value) => {
                let literal = C::literal_of_value(value).ok_or_else(|| refusal("capture"))?;
                let index = self.constants.len() as i64;
                self.constants.push(literal);
                Ok(self.emit_n(s, source, &V::ty_of(value), C::Expr::Constant(index)))
            }
        }
    }

    fn primitive(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, op: C::Primitive, args: Vec<(&str, Lowered)>) -> R<Lowered> {
        // Geometry rest slots already have native collection capacity. This internal
        // builder flag follows their checked type, never an authored count option.
        let mut args = args;
        if op == C::Primitive::Sequence_build && geometry_collection(ty) { args.push(("cooked", boolean(true))); }
        let mut lowered = Vec::new();
        for (name, v) in &args { lowered.push((name.to_string(), self.ensure(s, source, v)?)); }
        Ok(self.emit(s, source, ty, C::Expr::Prim(op, lowered)))
    }

    fn type_of(&self, l: &Lowered) -> Ty {
        match l {
            Selected(items, _) => items.iter().fold(Ty::Any, |ty, v| ty::join(&ty, &self.type_of(v)).unwrap_or(ty)),
            Known(Value::Residual(r)) => E::Residual::residual_ty(*r).unwrap_or(Ty::Any),
            Known(Value::List(items)) => Ty::List(Box::new(items.iter().fold(Ty::Any, |ty, v| ty::join(&ty, &self.type_of(&Known(v.clone()))).unwrap_or(ty)))),
            Known(v) => V::ty_of(v),
            Computed(_, ty) => ty.clone(),
            Callable(_) => Ty::Fn(None),
            Record_fields(fields) => Ty::Record(fields.iter().map(|(n, v)| (n.clone(), self.type_of(v))).collect()),
            Record_choice(_, yes, no) => ty::join(&self.type_of(yes), &self.type_of(no)).unwrap_or(Ty::Any),
        }
    }

    fn source_of_view(&self, view: &E::Residual::View) -> C::Source {
        C::Source {
            site: view.site.iter().skip(view.prefix.len()).cloned().collect(),
            owner: view.owner.clone(),
            instance: view.instance,
            prefix: view.prefix.clone(),
            iter: view.iter.iter().map(|n| C::Iteration::Static_index(*n)).collect(),
            span: None,
        }
    }

    fn field(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, value: &Lowered, name: &str) -> R<Lowered> {
        let resolved = self.resolve(s, value)?;
        match &resolved {
            Selected(items, index) => {
                self.select(s, source, ty, items, index, &mut |l, sc, v| l.field(sc, source, ty, v, name))
            }
            Record_choice(condition, yes, no) => {
                let ty = match self.type_of(&resolved) {
                    Ty::Record(fields) => fields.iter().find(|(n, _)| n == name).map(|(_, t)| t.clone()).unwrap_or_else(|| ty.clone()),
                    _ => ty.clone(),
                };
                let (yes, no) = ((**yes).clone(), (**no).clone());
                self.branch(s, source, &ty, condition,
                    &mut |l, sc| l.field(sc, source, &ty, &yes, name),
                    &mut |l, sc| l.field(sc, source, &ty, &no, name))
            }
            Record_fields(fields) => match fields.iter().find(|(n, _)| n == name) {
                Some((_, v)) => self.resolve(s, v),
                None => Err(refusal("get")),
            },
            Known(v) => {
                let v = match (v, name) {
                    (Value::Record(xs), name) => xs.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()).ok_or_else(|| refusal("get"))?,
                    (Value::Vec2(x, _), "x") => Value::Float(*x),
                    (Value::Vec2(_, y), "y") => Value::Float(*y),
                    (Value::Vec3(x, _, _), "x") => Value::Float(*x),
                    (Value::Vec3(_, y, _), "y") => Value::Float(*y),
                    (Value::Vec3(_, _, z), "z") => Value::Float(*z),
                    (Value::Vec4(x, _, _, _), "x") => Value::Float(*x),
                    (Value::Vec4(_, y, _, _), "y") => Value::Float(*y),
                    (Value::Vec4(_, _, z, _), "z") => Value::Float(*z),
                    (Value::Vec4(_, _, _, w), "w") => Value::Float(*w),
                    _ => return Err(refusal("get")),
                };
                self.resolve(s, &Known(v))
            }
            Callable(_) => Err(refusal("get")),
            Computed(n, value_ty) => {
                let ty = match value_ty {
                    Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => Ty::Float,
                    Ty::Record(fields) => fields.iter().find(|(n, _)| n == name).map(|(_, t)| t.clone()).unwrap_or_else(|| ty.clone()),
                    _ => ty.clone(),
                };
                Ok(self.emit(s, source, &ty, C::Expr::Field(*n, name.to_owned())))
            }
        }
    }

    fn coerce(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, l: &Lowered) -> R<Lowered> {
        match l {
            Selected(items, index) => self.select(s, source, ty, items, index, &mut |l, sc, v| l.coerce(sc, source, ty, v)),
            Record_choice(..) => self.map_fields(s, source, l, &mut |l2, sc, fields| {
                match l2.coerce(sc, source, ty, &Record_fields(fields.clone()))? {
                    Record_fields(fields) => Ok(fields),
                    _ => Ok(fields),
                }
            }),
            Known(value) => match V::coerce_to(ty, value) {
                Ok(v) => Ok(Known(v)),
                Err(d) => {
                    if d.code == "$unsupported" { return Err(Stop::from(d)); }
                    self.primitive(s, source, ty, failure(&d), vec![])
                }
            },
            Record_fields(fields) => match ty {
                Ty::Any => Ok(l.clone()),
                Ty::Record(wanted) => {
                    let mut out = Vec::new();
                    for (name, v) in fields {
                        let v = match wanted.iter().find(|(n, _)| n == name) {
                            Some((_, ty)) => self.coerce(s, source, ty, v)?,
                            None => v.clone(),
                        };
                        out.push((name.clone(), v));
                    }
                    Ok(Record_fields(out))
                }
                _ => Ok(l.clone()),
            },
            Callable(_) if ty::has_fn(ty) || *ty == Ty::Any => Ok(l.clone()),
            Callable(_) => Err(refusal("fn")),
            Computed(n, have) if *ty == Ty::Any || (have == ty && !self.mixed.contains(n)) => Ok(l.clone()),
            value => self.primitive(s, source, ty, C::Primitive::Coerce(ty.clone()), vec![("value", value.clone())]),
        }
    }
}

impl Lowerer {
    fn select(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, items: &[Lowered], index: &Lowered, f: LowerItem) -> R<Lowered> {
        // ponytail: selecting a heterogeneous lazy capture scans at most 4,096
        // arms; use typed captured buffers if profiles justify compiling this case.
        self.choose(s, source, ty, items, index, f, 0)
    }
    fn choose(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, items: &[Lowered], index: &Lowered, f: LowerItem, at: usize) -> R<Lowered> {
        if at == items.len() {
            return self.primitive(s, source, ty, C::Primitive::Fail("E_LIST_RANGE".into(), "Index is outside its list.".into()), vec![]);
        }
        let matches = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", index.clone()), ("b", int(at as i64))])?;
        // the two arms share the callback: run them one after the other through a cell
        let cell = std::cell::RefCell::new(f);
        let item = items[at].clone();
        self.branch(s, source, ty, &matches,
            &mut |l, sc| (cell.borrow_mut())(l, sc, &item),
            &mut |l, sc| {
                let mut g = cell.borrow_mut();
                l.choose(sc, source, ty, items, index, &mut *g, at + 1)
            })
    }

    fn store(&mut self, s: &mut Scope, source: &C::Source, sequence: &Lowered, index: &Lowered, value: &Lowered) -> R<()> {
        let sequence = self.ensure(s, source, sequence)?;
        let index = self.ensure(s, source, index)?;
        let value = self.ensure(s, source, value)?;
        self.push(s, source, C::StatementNode::Store { sequence, index, value });
        Ok(())
    }

    fn assign(&mut self, s: &mut Scope, source: &C::Source, target: i64, value: &Lowered) -> R<()> {
        let value = self.ensure(s, source, value)?;
        self.push(s, source, C::StatementNode::Assign(target, C::Expr::Local(value)));
        Ok(())
    }

    fn hold(&mut self, s: &mut Scope, source: &C::Source, ty: Option<&Ty>, value: &Lowered) -> R<Lowered> {
        let resolved = self.resolve(s, value)?;
        match &resolved {
            Record_fields(fields) => {
                let mut out = Vec::new();
                for (name, v) in fields {
                    let fty = match ty { Some(Ty::Record(fs)) => fs.iter().find(|(n, _)| n == name).map(|(_, t)| t.clone()), _ => None };
                    out.push((name.clone(), self.hold(s, source, fty.as_ref(), v)?));
                }
                Ok(Record_fields(out))
            }
            Known(Value::Record(fields)) => {
                let l = Record_fields(fields.iter().map(|(n, v)| (n.clone(), Known(v.clone()))).collect());
                self.hold(s, source, ty, &l)
            }
            value => {
                let n = self.ensure(s, source, value)?;
                let ty = ty.cloned().unwrap_or_else(|| self.type_of(value));
                let result = self.emit(s, source, &ty, C::Expr::Local(n));
                if let Computed(n, _) = &result {
                    if ty == Ty::Int || ty == Ty::Float { self.mixed.insert(*n); }
                }
                Ok(result)
            }
        }
    }

    fn update(&mut self, s: &mut Scope, source: &C::Source, target: &Lowered, value: &Lowered) -> R<()> {
        match target {
            Computed(n, _) => self.assign(s, source, *n, value),
            Record_fields(fields) => {
                let values = self.record_fields(s, source, value)?;
                if fields.iter().map(|(n, _)| n).ne(values.iter().map(|(n, _)| n)) { return Err(refusal("accumulator_layout")); }
                // Read every next field before overwriting any accumulator field.
                let mut held = Vec::new();
                for (name, v) in &values { held.push((name.clone(), self.hold(s, source, None, v)?)); }
                for ((_, target), (_, value)) in fields.iter().zip(held.iter()) { self.update(s, source, target, value)?; }
                Ok(())
            }
            _ => Err(refusal("accumulator")),
        }
    }

    fn widen_like(&mut self, s: &mut Scope, source: &C::Source, old: &Lowered, value: &Lowered) -> R<Lowered> {
        let old = self.resolve(s, old)?;
        let value = self.resolve(s, &value.clone())?;
        match (self.type_of(&old), self.type_of(&value)) {
            (Ty::Int | Ty::Float, Ty::Int | Ty::Float) => {
                let widened = self.coerce(s, source, &Ty::Float, &value)?;
                let is_float = self.primitive(s, source, &Ty::Bool, C::Primitive::Equal, vec![("a", value.clone()), ("b", widened)])?;
                let v1 = value.clone();
                self.branch(s, source, &Ty::Float, &is_float,
                    &mut |_, _| Ok(v1.clone()),
                    &mut |l, sc| l.coerce_like(sc, source, &old, &value))
            }
            _ => self.coerce_like(s, source, &old, &value),
        }
    }

    fn increment(&mut self, s: &mut Scope, source: &C::Source, value: &Lowered) -> R<()> {
        let next = self.primitive(s, source, &Ty::Int, num(PO::Binary::Add), vec![("a", value.clone()), ("b", int(1))])?;
        self.update(s, source, value, &next)
    }

    fn counted(&mut self, limit: i64, stop: Option<&Lowered>, s: &mut Scope, source: &C::Source, first: &Lowered, last: &Lowered,
               f: &mut dyn FnMut(&mut Lowerer, &mut Scope, &Lowered) -> R<()>) -> R<()> {
        let first = self.ensure(s, source, first)?;
        let last = self.ensure(s, source, last)?;
        let stop = match stop { Some(l) => Some(self.ensure(s, source, l)?), None => None };
        let count = self.local(&Ty::Int);
        let index = self.local(&Ty::Int);
        let item = self.local(&Ty::Int);
        let id = self.number();
        let mut body = s.fork();
        f(self, &mut body, &Computed(item, Ty::Int))?;
        s.st.push(statement(id, source, C::StatementNode::Counted { first, last, count, index, item, limit, stop, init: Vec::new(), body: body.st }));
        Ok(())
    }

    fn slice(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, sequence: &Lowered, first: &Lowered, count: &Lowered) -> R<Lowered> {
        self.primitive(s, source, ty, C::Primitive::Sequence_slice, vec![("list", sequence.clone()), ("first", first.clone()), ("count", count.clone())])
    }

    fn sequence_count(&mut self, s: &mut Scope, source: &C::Source, sequence: &Lowered) -> R<Lowered> {
        let sequence = self.resolve(s, sequence)?;
        match &sequence {
            Selected(items, index) => self.select(s, source, &Ty::Int, items, index, &mut |l, sc, v| l.sequence_count(sc, source, v)),
            Known(Value::List(xs)) => Ok(int(xs.len() as i64)),
            Known(v @ (Value::Text_array(_) | Value::Int_array(_) | Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_))) => {
                Ok(int(V::array_length(v)? as i64))
            }
            _ => {
                let packed = matches!(self.type_of(&sequence), Ty::Array(_));
                self.primitive(s, source, &Ty::Int, if packed { C::Primitive::Array_count } else { C::Primitive::Sequence_length },
                    vec![(if packed { "array" } else { "list" }, sequence.clone())])
            }
        }
    }

    fn sequence_get(&mut self, s: &mut Scope, source: &C::Source, sequence: &Lowered, index: &Lowered) -> R<Lowered> {
        let sequence = self.resolve(s, sequence)?;
        match (&sequence, index) {
            (Selected(items, at), index) => {
                let ty = ty::elem(&self.type_of(&sequence)).unwrap_or(Ty::Any);
                let index = index.clone();
                self.select(s, source, &ty, items, at, &mut |l, sc, v| l.sequence_get(sc, source, v, &index))
            }
            (Known(Value::List(items)), Known(index)) => {
                let o = op::find(&[], "nth", &context::value()).ok_or_else(|| refusal("nth"))?;
                let args = vec![("list".to_owned(), Value::List(items.clone())), ("index".to_owned(), index.clone())];
                match (o.body)(&input::at_time(0.), &|_, _| Err(refusal_diag("nth")), &args) {
                    Ok(v) => self.resolve(s, &Known(v)),
                    Err(d) => {
                        if d.code == "$unsupported" { return Err(Stop::from(d)); }
                        self.primitive(s, source, &Ty::Any, failure(&d), vec![])
                    }
                }
            }
            (Known(Value::List(items)), index) if C::literal_of_value(&Value::List(items.clone())).is_none() => {
                if items.len() > 4096 { return Err(refusal("list_limit")); }
                let indices = Known(Value::List(Arc::new((0..items.len() as i64).map(Value::Int).collect())));
                let index = self.sequence_get(s, source, &indices, index)?;
                Ok(Selected(items.iter().map(|v| Known(v.clone())).collect(), Box::new(index)))
            }
            _ => {
                let ty = ty::elem(&self.type_of(&sequence)).unwrap_or(Ty::Any);
                let packed = matches!(self.type_of(&sequence), Ty::Array(_));
                self.primitive(s, source, &ty, if packed { C::Primitive::Array_nth } else { C::Primitive::Sequence_get },
                    vec![(if packed { "array" } else { "list" }, sequence.clone()), ("index", index.clone())])
            }
        }
    }

    fn iterate(&mut self, s: &mut Scope, source: &C::Source, sequence: &Lowered, f: &mut dyn FnMut(&mut Lowerer, &mut Scope, &Lowered, &Lowered) -> R<()>) -> R<()> {
        let sequence = self.resolve(s, sequence)?;
        match &sequence {
            Known(Value::List(items)) if C::literal_of_value(&Value::List(items.clone())).is_none() => {
                let items: Vec<Lowered> = items.iter().map(|v| Known(v.clone())).collect();
                self.counted(4096, None, s, source, &int(0), &int(items.len() as i64), &mut |l, sc, index| {
                    f(l, sc, index, &Selected(items.clone(), Box::new(index.clone())))
                })
            }
            _ => {
                let sequence_id = self.ensure(s, source, &sequence)?;
                let index = self.local(&Ty::Int);
                let ty = ty::elem(&self.type_of(&sequence)).unwrap_or(Ty::Any);
                let item = self.local(&ty);
                let id = self.number();
                if ty == Ty::Float { self.mixed.insert(item); }
                let mut body = s.fork();
                f(self, &mut body, &Computed(index, Ty::Int), &Computed(item, ty))?;
                let limit = match self.type_of(&sequence) {
                    Ty::Array(e) if matches!(*e, Ty::Int | Ty::Text) => MAX_ARRAY,
                    Ty::Array(element) => MAX_ARRAY / PO::scalar_width(&element).unwrap_or(1) as i64,
                    _ => 4096,
                };
                s.st.push(statement(id, source, C::StatementNode::Iterate { sequence: sequence_id, index, item, limit, body: body.st }));
                Ok(())
            }
        }
    }

    fn record_fields(&mut self, s: &mut Scope, source: &C::Source, value: &Lowered) -> R<Vec<(String, Lowered)>> {
        let resolved = self.resolve(s, value)?;
        match &resolved {
            Record_fields(fields) => Ok(fields.clone()),
            Known(Value::Record(fields)) => Ok(fields.iter().map(|(n, v)| (n.clone(), Known(v.clone()))).collect()),
            Record_choice(..) => {
                let n = self.ensure(s, source, &resolved)?;
                let c = Computed(n, self.type_of(&resolved));
                self.record_fields(s, source, &c)
            }
            Computed(_, Ty::Record(fields)) => {
                let mut out = Vec::new();
                for (name, ty) in fields { out.push((name.clone(), self.field(s, source, ty, &resolved, name)?)); }
                Ok(out)
            }
            _ => Err(refusal("record")),
        }
    }

    fn map_fields(&mut self, s: &mut Scope, source: &C::Source, value: &Lowered,
                  f: &mut dyn FnMut(&mut Lowerer, &mut Scope, Vec<(String, Lowered)>) -> R<Vec<(String, Lowered)>>) -> R<Lowered> {
        let resolved = self.resolve(s, value)?;
        match &resolved {
            Selected(items, index) => {
                let ty = self.type_of(value);
                let cell = std::cell::RefCell::new(f);
                self.select(s, source, &ty, items, index, &mut |l, sc, v| l.map_fields(sc, source, v, &mut *cell.borrow_mut()))
            }
            Record_choice(condition, yes, no) if matches!((self.type_of(yes), self.type_of(no)), (Ty::Record(_), Ty::Record(_))) => {
                let ty = self.type_of(value);
                let (yes, no) = ((**yes).clone(), (**no).clone());
                let cell = std::cell::RefCell::new(f);
                self.branch(s, source, &ty, condition,
                    &mut |l, sc| l.map_fields(sc, source, &yes, &mut *cell.borrow_mut()),
                    &mut |l, sc| l.map_fields(sc, source, &no, &mut *cell.borrow_mut()))
            }
            _ => {
                let fields = self.record_fields(s, source, &resolved)?;
                Ok(Record_fields(f(self, s, fields)?))
            }
        }
    }

    fn coerce_like(&mut self, s: &mut Scope, source: &C::Source, old: &Lowered, value: &Lowered) -> R<Lowered> {
        let old = self.resolve(s, old)?;
        let value = self.resolve(s, value)?;
        let (old_ty, value_ty) = (self.type_of(&old), self.type_of(&value));
        if matches!((&old_ty, &value_ty), (Ty::List(_), Ty::List(_))) {
            let count = self.sequence_count(s, source, &old)?;
            let empty = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", count), ("b", int(0))])?;
            let (v1, old1, value1) = (value.clone(), old.clone(), value.clone());
            return self.branch(s, source, &value_ty, &empty,
                &mut |_, _| Ok(v1.clone()),
                &mut |l, sc| {
                    let old_item = l.sequence_get(sc, source, &old1, &int(0))?;
                    let count = l.sequence_count(sc, source, &value1)?;
                    let result = l.primitive(sc, source, &value_ty, C::Primitive::Sequence_build, vec![("count", count)])?;
                    l.iterate(sc, source, &value1, &mut |l, sc, index, item| {
                        let v = l.coerce_like(sc, source, &old_item, item)?;
                        l.store(sc, source, &result, index, &v)
                    })?;
                    Ok(result)
                });
        }
        if let (Known(o), Known(v)) = (&old, &value) {
            if C::literal_of_value(o).is_some() && C::literal_of_value(v).is_some() {
                return self.coerce(s, source, &V::ty_of(o), &value);
            }
        }
        if let (Ty::Record(old_fields), Ty::Record(_)) = (&old_ty, &value_ty) {
            let old_fields = old_fields.clone();
            let old2 = old.clone();
            return self.map_fields(s, source, &value, &mut |l, sc, fields| {
                let mut out = Vec::new();
                for (name, v) in fields {
                    let v2 = match old_fields.iter().find(|(n, _)| *n == name) {
                        Some((_, ty)) => {
                            let o = l.field(sc, source, ty, &old2, &name)?;
                            l.coerce_like(sc, source, &o, &v)?
                        }
                        None => v,
                    };
                    out.push((name, v2));
                }
                Ok(out)
            });
        }
        if let Known(o) = &old { return self.coerce(s, source, &V::ty_of(o), &value); }
        let old_mixed = old_ty == Ty::Float || matches!(&old, Computed(n, Ty::Int) if self.mixed.contains(n));
        if old_mixed && matches!(value_ty, Ty::Int | Ty::Float | Ty::Bool) {
            let widened = self.coerce(s, source, &Ty::Float, &old)?;
            let is_float = self.primitive(s, source, &Ty::Bool, C::Primitive::Equal, vec![("a", old.clone()), ("b", widened)])?;
            let (v1, v2) = (value.clone(), value.clone());
            return self.branch(s, source, &Ty::Float, &is_float,
                &mut |l, sc| l.coerce(sc, source, &Ty::Float, &v1),
                &mut |l, sc| l.coerce(sc, source, &Ty::Int, &v2));
        }
        self.coerce(s, source, &old_ty, &value)
    }

    fn branch(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, condition: &Lowered, yes: Lower, no: Lower) -> R<Lowered> {
        if let Known(v) = condition {
            if C::literal_of_value(v).is_some() {
                return if V::truthy(v)? { yes(self, s) } else { no(self, s) };
            }
        }
        let condition = self.ensure(s, source, condition)?;
        let id = self.number();
        if !matches!(ty, Ty::Record(_)) {
            let result = self.local(ty);
            if *ty == Ty::Float { self.mixed.insert(result); }
            let yes_body = self.arm(s, source, result, yes)?;
            let no_body = self.arm(s, source, result, no)?;
            s.st.push(statement(id, source, C::StatementNode::If { condition, yes: yes_body, no: no_body }));
            Ok(Computed(result, ty.clone()))
        } else {
            let mut yes_scope = s.fork();
            let mut no_scope = s.fork();
            let a = yes(self, &mut yes_scope)?;
            let b = no(self, &mut no_scope)?;
            let result = self.join(source, condition, ty, &a, &b, &mut yes_scope, &mut no_scope)?;
            s.st.push(statement(id, source, C::StatementNode::If { condition, yes: yes_scope.st, no: no_scope.st }));
            Ok(result)
        }
    }

    fn arm(&mut self, s: &Scope, source: &C::Source, result: i64, lower: Lower) -> R<Vec<C::Statement>> {
        let mut body = s.fork();
        let v = lower(self, &mut body)?;
        let value = self.ensure(&mut body, source, &v)?;
        let id = self.number();
        body.st.push(statement(id, source, C::StatementNode::Assign(result, C::Expr::Local(value))));
        Ok(body.st)
    }

    fn join(&mut self, source: &C::Source, condition: i64, ty: &Ty, a: &Lowered, b: &Lowered, yes: &mut Scope, no: &mut Scope) -> R<Lowered> {
        fn fields(l: &Lowered) -> Option<Vec<(String, Lowered)>> {
            match l {
                Record_fields(fs) => Some(fs.clone()),
                Known(Value::Record(fs)) => Some(fs.iter().map(|(n, v)| (n.clone(), Known(v.clone()))).collect()),
                _ => None,
            }
        }
        match (fields(a), fields(b)) {
            (Some(fs), Some(gs)) if fs.iter().map(|(n, _)| n).eq(gs.iter().map(|(n, _)| n)) => {
                let mut out = Vec::new();
                for ((name, a), (_, b)) in fs.iter().zip(gs.iter()) {
                    let ty = ty::join(&self.type_of(a), &self.type_of(b)).unwrap_or(Ty::Any);
                    out.push((name.clone(), self.join(source, condition, &ty, a, b, yes, no)?));
                }
                Ok(Record_fields(out))
            }
            _ if matches!(ty, Ty::Record(_)) => Ok(Record_choice(Box::new(Computed(condition, Ty::Bool)), Box::new(a.clone()), Box::new(b.clone()))),
            _ => {
                let result = self.local(ty);
                if *ty == Ty::Float { self.mixed.insert(result); }
                for (scope, v) in [(&mut *yes, a), (&mut *no, b)] {
                    let value = self.ensure(scope, source, v)?;
                    let id = self.number();
                    scope.st.push(statement(id, source, C::StatementNode::Assign(result, C::Expr::Local(value))));
                }
                Ok(Computed(result, ty.clone()))
            }
        }
    }
}

impl Lowerer {
    fn residual(&mut self, s: &mut Scope, r: ResidualId) -> R<Lowered> {
        let id = E::Residual::residual_id(r);
        let key = CacheKey::Residual(id);
        if let Some(v) = s.cache.get(&key) { return Ok(v.clone()); }
        let view = self.view(r)?;
        if self.active.contains(&id) { return Err(refusal("cycle")); }
        self.active.insert(id);
        let saved = (self.plane, self.geometry);
        let saved_current = self.current;
        let saved_nodes = std::mem::replace(&mut self.step_nodes, state_step_nodes(&view.term));
        self.current = Some(r);
        self.plane = if view.host { C::Plane::Host } else { C::Plane::Frame };
        self.geometry = view.geometry;
        let result = self.residual_body(s, r, &view, key);
        self.active.remove(&id);
        self.plane = saved.0;
        self.geometry = saved.1;
        self.step_nodes = saved_nodes;
        self.current = saved_current;
        result
    }

    fn residual_body(&mut self, s: &mut Scope, r: ResidualId, view: &E::Residual::View, key: CacheKey) -> R<Lowered> {
        let env: Env = view.bindings.iter().map(|(n, v)| (n.clone(), Known(v.clone()))).collect();
        let source = self.source_of_view(view);
        // Draws and SOP cooks lower captured arrays as explicit producers, so compacted
        // counts and the regions consuming them share their Core locals.
        let captured_arrays = (self.initial.is_some() || self.has_draw || sop_cook(r))
            && E::Residual::free_names(&view.term).iter().any(|name| view.bindings.iter().find(|(n, _)| n == name).map(|(_, v)| holds_array_residual(v)).unwrap_or(false));
        let packed = if view.previous || captured_arrays { None } else { self.packed_region(r, view) };
        let value = if let Some(region) = packed {
            self.region(s, Vec::new(), region, view)
        } else if view.previous {
            let mut src = source.clone();
            src.span = Some(view.term.form.span.clone());
            self.state_term(s, &src, &env, &view.term, true)?
        } else {
            self.term(false, s, &source, &env, &view.term)?
        };
        s.cache.insert(key, value.clone());
        (self.lowered_hook.clone())(r);
        Ok(value)
    }

    fn descriptor(&mut self, s: &mut Scope, source: &C::Source, l: &Lowered) -> R<Lowered> {
        match l {
            Known(Value::Source(name, ty, fields)) | Known(Value::Struct(name, ty @ Ty::Array(_), fields)) if storage_argument(ty) => {
                let mut args = Vec::new();
                for (n, v) in fields { args.push((n.clone(), self.ensure(s, source, &Known(v.clone()))?)); }
                Ok(self.emit(s, source, ty, C::Expr::Source_descriptor { name: name.clone(), ty: ty.clone(), args }))
            }
            Known(Value::Record(fields)) => {
                let mut out = Vec::new();
                for (n, v) in fields { out.push((n.clone(), self.descriptor(s, source, &Known(v.clone()))?)); }
                Ok(Record_fields(out))
            }
            Record_fields(fields) => {
                let mut out = Vec::new();
                for (n, v) in fields { out.push((n.clone(), self.descriptor(s, source, v)?)); }
                Ok(Record_fields(out))
            }
            v => Ok(v.clone()),
        }
    }

    fn capture(&mut self, s: &mut Scope, source: &C::Source, parent: ResidualId, static_: bool, sop: bool, arguments: &mut Vec<i64>, l: &Lowered) -> R<Value> {
        match l {
            // Draws, SOP cooks and state-dependent captures use an explicit producer before
            // the region, which reads its immutable result as an argument.
            Known(Value::Residual(r))
                if self.initial.is_some() || self.has_draw || static_ || E::state_dependent(&Value::Residual(*r))
                    || (sop && holds_arrays(&self.view(*r)?.term.ty)) =>
            {
                let v = self.residual(s, *r)?;
                self.capture(s, source, parent, static_, sop, arguments, &v)
            }
            // a record local (a state of several arrays) is passed field by field: Packed takes
            // arrays and scalars, not a record argument
            Computed(id, Ty::Record(fields)) if self.has_draw || sop => {
                let mut out = Vec::new();
                for (name, ty) in fields { out.push((name.clone(), self.emit(s, source, ty, C::Expr::Field(*id, name.clone())))); }
                self.capture(s, source, parent, static_, sop, arguments, &Record_fields(out))
            }
            // a captured function's arrays follow the kernel's own rule
            Known(Value::Fn(f)) if sop => {
                let mut replaced = Vec::new();
                for (_, v) in E::Residual::function_bindings(*f) {
                    let n = self.capture(s, source, parent, static_, sop, arguments, &Known(v.clone()))?;
                    replaced.push((v, n));
                }
                let replace = move |v: &Value| replaced.iter().find(|(old, _)| old == v).map(|(_, n)| n.clone()).unwrap_or_else(|| v.clone());
                Ok(Value::Fn(E::bind_captures(*f, &replace)))
            }
            Known(value) => Ok(value.clone()),
            Computed(id, ty) => {
                let index = match arguments.iter().position(|a| a == id) {
                    Some(i) => i,
                    None => { arguments.push(*id); arguments.len() - 1 }
                };
                Ok(Value::Source("$core/argument".into(), ty.clone(), vec![("index".into(), Value::Int(index as i64))]))
            }
            Record_fields(fields) => {
                let mut out = Vec::new();
                for (name, v) in fields { out.push((name.clone(), self.capture(s, source, parent, static_, sop, arguments, v)?)); }
                Ok(Value::Record(out))
            }
            Callable(Callable::Closure(cl)) => {
                let mut bindings = Vec::new();
                for (name, v) in &cl.env { bindings.push((name.clone(), self.capture(s, source, parent, static_, sop, arguments, v)?)); }
                let f = E::Residual::compiled_function(parent, cl.instance, cl.prefix.clone(), cl.zone.clone(), cl.params.clone(), cl.body.clone(), bindings)?;
                Ok(Value::Fn(f))
            }
            _ => Err(refusal("packed_capture")),
        }
    }

    /// A packed kernel for this term under its parent residual, or why not.
    fn term_kernel(&mut self, s: &mut Scope, source: &C::Source, env: &Env, x: &Term, parent: ResidualId, static_: bool, sop: bool,
                   why: &mut Option<Vec<Diagnostic>>) -> R<Option<(ResidualId, Arc<C::RegionT>, Vec<i64>)>> {
        let names = E::Residual::free_names(x);
        let mut arguments: Vec<i64> = Vec::new();
        let attempt = (|| -> R<Option<(ResidualId, Arc<C::RegionT>, Vec<i64>)>> {
            let mut bindings = Vec::new();
            for name in &names {
                if let Some(v) = env.get(name) {
                    bindings.push((name.clone(), self.capture(s, source, parent, static_, sop, &mut arguments, v)?));
                }
            }
            let r = E::Residual::with_bindings(parent, x, &bindings)?;
            if E::state_dependent(&Value::Residual(r)) { return Ok(None); }
            match packed::compile_result(&self.options, r, x) {
                Err(d) => { *why = Some(d); Ok(None) }
                Ok(region) => Ok(if (self.region_allowed)(&region) { Some((r, Arc::new(region), arguments.clone())) } else { None }),
            }
        })();
        match attempt {
            Err(Stop::Unsupported(reason)) => {
                *why = Some(vec![rays_diagnostic::error("E_PACKED_CAPTURE".into(), format!("unsupported {reason}"), None, None, None, Vec::new(), Vec::new())]);
                Ok(None)
            }
            other => other,
        }
    }

    fn term(&mut self, borrow: bool, s: &mut Scope, source: &C::Source, env: &Env, x: &Term) -> R<Lowered> {
        self.budget -= 1;
        if self.budget < 0 { return Err(refusal("budget")); }
        let mut src = source.clone();
        src.site = x.path.clone().unwrap_or_else(|| source.site.clone());
        src.span = Some(x.form.span.clone());
        let source = &src;
        let (numeric_candidate, text_candidate) = packed_candidate(x);
        let candidate = numeric_candidate || text_candidate;
        // A State step's maps are deferred past the static pass. Compile those nested kernels
        // under their state residual; other nested candidates keep the existing strict/draw gate.
        let state_parent = self.current.is_some() && self.step_nodes.contains(&(x as *const Term as usize));
        // a kernel reading a state value takes it as an argument, lowered once before the region
        let reads_state = candidate
            && E::Residual::free_names(x).iter().any(|name| matches!(env.get(name), Some(Known(Value::Residual(r))) if E::state_dependent(&Value::Residual(*r))));
        let mut why: Option<Vec<Diagnostic>> = None;
        let static_ = self.current.map(static_sop).unwrap_or(false);
        let sop = self.current.map(sop_cook).unwrap_or(false);
        let kernel = if !candidate || !(self.strict || self.initial.is_some() || self.has_draw || sop || state_parent || reads_state) {
            None
        } else {
            match self.current {
                None => None,
                Some(parent) => self.term_kernel(s, source, env, x, parent, static_, sop, &mut why)?,
            }
        };
        if let Some((r, region, args)) = kernel {
            let view = self.view(r)?;
            return Ok(self.region(s, args, region, &view));
        }
        // the costliest fallback there is: an interpreted loop, element by element; reported before
        // a strict lowering refuses it, so the refusal names its reason too
        if let (Some(d), Some(parent)) = (why.as_ref().and_then(|w| w.first()), self.current) {
            let site = self.view(parent)?.site.join("/");
            fallback::note(&site, &format!("no kernel, interpreted: {}", d.message));
        }
        if numeric_candidate && self.strict { return Err(refusal("packed_kernel")); }
        match &x.node {
            Node::Lit(param::Value::Int(n)) => Ok(Known(Value::Int(*n))),
            Node::Lit(param::Value::Float(f)) => Ok(Known(Value::Float(*f))),
            Node::Lit(param::Value::Bool(b)) => Ok(Known(Value::Bool(*b))),
            Node::Text(t) => Ok(Known(Value::Text(t.clone()))),
            Node::Nil => Ok(Known(Value::No_geo)),
            Node::Record(fields) => {
                let mut out = Vec::new();
                for (name, t) in fields { out.push((name.clone(), self.term(borrow, s, source, env, t)?)); }
                Ok(Record_fields(out))
            }
            Node::ListLit(items) => {
                if items.len() > 4096 && !geometry_collection(&x.ty) { return Err(refusal("list_limit")); }
                let mut lowered = Vec::new();
                for t in items { lowered.push(self.term(false, s, source, env, t)?); }
                let pairs: Vec<(String, Lowered)> = lowered.iter().map(|v| (String::new(), v.clone())).collect();
                match known_values(&pairs) {
                    Some(items) => {
                        let values: Vec<Value> = items.into_iter().map(|(_, v)| v).collect();
                        let ty = V::elem_ty(&values);
                        let mut out = Vec::new();
                        for v in &values { out.push(V::coerce_to(&ty, v)?); }
                        Ok(Known(Value::List(Arc::new(out))))
                    }
                    None => {
                        let mut ids = Vec::new();
                        for v in &lowered { ids.push(self.ensure(s, source, v)?); }
                        Ok(self.emit(s, source, &x.ty, C::Expr::Items(ids)))
                    }
                }
            }
            Node::ListOp(_, items) => {
                let mut args = Vec::new();
                for item in items { args.push(("list", self.term(false, s, source, env, item)?)); }
                self.primitive(s, source, &x.ty, C::Primitive::Sequence_concat, args)
            }
            Node::Get(value, name) => {
                let v = self.term(borrow, s, source, env, value)?;
                self.field(s, source, &x.ty, &v, name)
            }
            Node::Assoc(record, updates) => {
                let mut record = self.term(false, s, source, env, record)?;
                for (name, t) in updates {
                    let value = self.term(false, s, source, env, t)?;
                    let name = name.clone();
                    record = self.map_fields(s, source, &record, &mut |l, sc, fields| {
                        match fields.iter().find(|(n, _)| *n == name) {
                            Some((_, old)) => {
                                let value = l.coerce_like(sc, source, old, &value)?;
                                Ok(fields.iter().map(|(n, v)| (n.clone(), if *n == name { value.clone() } else { v.clone() })).collect())
                            }
                            None => { let mut f = fields.clone(); f.push((name.clone(), value.clone())); Ok(f) }
                        }
                    })?;
                }
                Ok(record)
            }
            Node::Str(ts) => {
                let mut values = Vec::new();
                for t in ts { values.push(self.term(false, s, source, env, t)?); }
                let mut texts = Vec::new();
                for value in &values {
                    texts.push(match value {
                        Known(v) if C::literal_of_value(v).is_some() => Known(Value::Text(V::show_with(&|v| v.clone(), v))),
                        _ => self.primitive(s, source, &Ty::Text, C::Primitive::Show, vec![("value", value.clone())])?,
                    });
                }
                let pairs: Vec<(String, Lowered)> = texts.iter().map(|v| (String::new(), v.clone())).collect();
                match known_values(&pairs) {
                    Some(texts) => {
                        let mut out = Vec::new();
                        for (_, v) in texts { if let Value::Text(t) = v { out.extend(t); } }
                        Ok(Known(Value::Text(out)))
                    }
                    None => self.primitive(s, source, &Ty::Text, C::Primitive::Text_concat, texts.into_iter().map(|v| ("part", v)).collect()),
                }
            }
            Node::Time => Ok(self.frame(s, source, &Ty::Float, C::FrameField::Time)),
            Node::State { .. } => self.state_term(s, source, env, x, false),
            Node::Vec(ts) if ts.len() >= 2 && ts.len() <= 4 => {
                let mut args = Vec::new();
                for t in ts { args.push(self.term(false, s, source, env, t)?); }
                let pairs: Vec<(String, Lowered)> = args.iter().map(|v| (String::new(), v.clone())).collect();
                match known_values(&pairs) {
                    Some(values) => {
                        let mut nums = Vec::new();
                        for (_, v) in &values { nums.push(V::num(v)?); }
                        Ok(Known(C::value_of_literal(&C::Literal::Vector(nums))))
                    }
                    None => {
                        let mut ids = Vec::new();
                        for a in &args { ids.push(self.ensure(s, source, a)?); }
                        Ok(self.emit(s, source, &x.ty, C::Expr::Vec(ids)))
                    }
                }
            }
            Node::RefBinding(name, _) if name.starts_with("$elem:") => Err(refusal("element")),
            Node::RefBinding(name, fields) => {
                let start = match env.get(name) {
                    Some(v) => self.resolve(s, v)?,
                    None => return Err(refusal("binding")),
                };
                let mut value = start;
                for f in fields { value = self.field(s, source, &x.ty, &value, f)?; }
                if borrow { self.descriptor(s, source, &value) } else { Ok(value) }
            }
            // a program runs over values at hand: nothing it reads is live
            Node::Op { op, .. } if op == "live?" => Ok(boolean(false)),
            Node::Op { op, args, .. } => self.op_term(borrow, s, source, env, x, op, args),
            Node::Call { kind, ctx, args } => {
                if !ty::is_cooked(&x.ty) { return Err(refusal("call")); }
                let mut lowered = Vec::new();
                for (name, t) in args {
                    let v = self.term(storage_argument(&t.ty), s, source, env, t)?;
                    lowered.push((name.clone(), self.ensure(s, source, &v)?));
                }
                let mut all = Vec::new();
                all.push(("$name".to_owned(), self.ensure(s, source, &Known(text(kind)))?));
                all.push(("$context".to_owned(), self.ensure(s, source, &Known(text(context::name(*ctx))))?));
                all.extend(lowered);
                Ok(self.emit(s, source, &x.ty, C::Expr::Host_source { name: "$core/catalog".into(), ty: x.ty.clone(), args: all }))
            }
            Node::Let(bindings, body) => {
                let mut env = env.clone();
                for (pattern, value) in bindings {
                    let v = self.term(false, s, source, &env, value)?;
                    env = self.bind(s, source, env, pattern, v)?;
                }
                self.term(borrow, s, source, &env, body)
            }
            Node::ErrorRows { rows, policy, mapper } => {
                let rows = self.term(false, s, source, env, rows)?;
                let rows = self.ensure(s, source, &rows)?;
                let mut handler = Scope::fresh();
                let index = self.local(&Ty::Int);
                let m = self.term(false, &mut handler, source, env, mapper)?;
                let mapper = self.as_callable(source, &m)?;
                let payload = Ty::Record(vec![("code".into(), Ty::Text), ("message".into(), Ty::Text), ("cause".into(), Ty::List(Box::new(Ty::Text)))]);
                let d = self.call(false, &mut handler, source, &payload, None, &mapper, &[Computed(index, Ty::Int)])?;
                let diagnostic = self.ensure(&mut handler, source, &d)?;
                let result = self.local(&x.ty);
                self.push(s, source, C::StatementNode::Diagnose_rows { rows, policy: *policy, index, handler: handler.st, diagnostic, result });
                Ok(Computed(result, x.ty.clone()))
            }
            Node::WithError(protected, mapper) => {
                let mut body = Scope::fresh();
                let mut handler = Scope::fresh();
                let protected_value = self.term(false, &mut body, source, env, protected)?;
                let value = self.ensure(&mut body, source, &protected_value)?;
                let complete = ty::is_cooked(&protected.ty) && !matches!(protected_value, Known(Value::No_geo));
                let value = if complete {
                    let l = self.emit(&mut body, source, &protected.ty, C::Expr::Host_source { name: "$core/complete".into(), ty: protected.ty.clone(), args: vec![("value".into(), value)] });
                    self.ensure(&mut body, source, &l)?
                } else { value };
                let payload = Ty::Record(vec![("code".into(), Ty::Text), ("message".into(), Ty::Text), ("cause".into(), Ty::List(Box::new(Ty::Text)))]);
                let failure = self.local(&payload);
                let m = self.term(false, &mut handler, source, env, mapper)?;
                let mapper = self.as_callable(source, &m)?;
                let mapped = self.call(false, &mut handler, source, &payload, None, &mapper, &[Computed(failure, payload.clone())])?;
                let mapped = self.ensure(&mut handler, source, &mapped)?;
                let result = self.local(&x.ty);
                self.push(s, source, C::StatementNode::Protect { body: body.st, value, failure, handler: handler.st, mapped, result });
                Ok(Computed(result, x.ty.clone()))
            }
            Node::If(condition, yes, no) => {
                let c = self.term(false, s, source, env, condition)?;
                self.branch(s, source, &x.ty, &c,
                    &mut |l, sc| l.term(borrow, sc, source, env, yes),
                    &mut |l, sc| l.term(borrow, sc, source, env, no))
            }
            Node::Cond(arms, default) => self.cond_arms(borrow, s, source, env, &x.ty, arms, default),
            Node::Case(scrutinee, arms, default) => {
                let scrutinee = self.term(false, s, source, env, scrutinee)?;
                self.case_arms(borrow, s, source, env, &x.ty, &scrutinee, arms, default)
            }
            Node::CallFn { fn_name, args, body } => {
                let mut values = Vec::new();
                for a in args { values.push(self.term(false, s, source, env, a)?); }
                let value = match env.get(fn_name) {
                    Some(f) => {
                        let c = self.as_callable(source, f)?;
                        self.call(borrow, s, source, &x.ty, body.as_deref(), &c, &values)?
                    }
                    None => self.definition(borrow, s, source, fn_name, body.as_deref(), &values)?,
                };
                if borrow { self.descriptor(s, source, &value) } else { Ok(value) }
            }
            Node::FnRef(name) => {
                let identity = self.new_identity();
                let slot = self.call_slot(identity);
                self.push(s, source, C::StatementNode::Reset_call(slot));
                Ok(Callable(Callable::Named { name: name.clone(), identity }))
            }
            Node::Fn { params, body, zone, capture } => {
                let closure = match capture {
                    Some(original) => {
                        let o = self.term(false, s, source, env, original)?;
                        match self.as_callable(source, &o)? {
                            Callable::Closure(cl) => Closure { params: params.clone(), body: (**body).clone(), ..(*cl).clone() },
                            Callable::Named { identity, .. } => Closure {
                                params: params.clone(), body: (**body).clone(), zone: zone.clone(), env: Env::new(),
                                instance: source.instance, prefix: source.prefix.clone(), identity,
                            },
                        }
                    }
                    None => {
                        let identity = self.new_identity();
                        let slot = self.call_slot(identity);
                        self.push(s, source, C::StatementNode::Reset_call(slot));
                        Closure { params: params.clone(), body: (**body).clone(), zone: zone.clone(), env: env.clone(), instance: source.instance, prefix: source.prefix.clone(), identity }
                    }
                };
                Ok(Callable(Callable::Closure(Arc::new(closure))))
            }
            Node::Expanded { body, .. } => self.term(borrow, s, source, env, body),
            Node::Bypass(inner) => match W::bypass_input(inner) {
                Some(input) => {
                    let v = self.term(borrow, s, source, env, &input)?;
                    self.coerce(s, source, &inner.ty, &v)
                }
                None => Err(refusal("bypass")),
            },
            // A drawing or structured value's filter of a range: its predicate as an Int mask map (its free names read from the
            // lowering's scope, as the inline fn reads them), which packs like any map (all cores, or the
            // device in fast precision), then array/compact keeps the indices.  Any other filter, or a mask
            // that does not pack, keeps the element loop below.
            Node::Hof(HofKind::Filter, ts) if (self.has_draw || self.initial.is_some()) && filter_range(ts, &x.ty).is_some() => {
                let (fn_term, items, elem) = filter_range(ts, &x.ty).unwrap();
                let Node::Fn { body, .. } = &fn_term.node else { unreachable!() };
                let lit = |n: i64| Term { node: Node::Lit(param::Value::Int(n)), ty: Ty::Int, path: None, ..(**body).clone() };
                let mask_body = Term { node: Node::If(body.clone(), Box::new(lit(1)), Box::new(lit(0))), ty: Ty::Int, path: None, ..(**body).clone() };
                let Node::Fn { params, zone, .. } = &fn_term.node else { unreachable!() };
                let mask_fn = Term {
                    ty: Ty::Fn(Some(Box::new(ty::FnSignature { params: vec![elem.clone()], result: Ty::Int }))),
                    node: Node::Fn { params: params.clone(), body: Box::new(mask_body), zone: zone.clone(), capture: None },
                    ..fn_term.clone()
                };
                let map = Term { node: Node::Hof(HofKind::Map, vec![mask_fn, items.clone()]), ty: Ty::Array(Box::new(Ty::Int)), path: None, ..x.clone() };
                let mask = self.term(false, s, source, env, &map)?;
                let kept = self.primitive(s, source, &Ty::Array(Box::new(Ty::Int)), C::Primitive::Array_compact, vec![("mask", mask)])?;
                if elem == Ty::Int { Ok(kept) } else { self.primitive(s, source, &x.ty, C::Primitive::Coerce(x.ty.clone()), vec![("value", kept)]) }
            }
            Node::Hof(kind, ts) if !ts.is_empty() => {
                let f = self.term(false, s, source, env, &ts[0])?;
                let f = self.as_callable(source, &f)?;
                self.hof(s, source, env, &x.ty, *kind, &f, &ts[1..])
            }
            Node::Hof(_, _) => Err(refusal("higher_order")),
            Node::GraphRef { graph, inputs } if inputs.is_empty() => {
                // The referenced graph's body in its own default instance, as the live walker re-runs it.
                let root = self.root()?;
                let g = E::Residual::residual_graph(root, graph).ok_or_else(|| refusal("ref"))?;
                let zone = vec![graph.clone()];
                let mut src2 = source.clone();
                src2.instance = E::Residual::graph_instance(root, graph).unwrap_or(source.instance);
                src2.prefix = Vec::new();
                src2.site = zone.clone();
                src2.owner = zone;
                src2.iter = Vec::new();
                let saved = self.plane;
                self.plane = if g.context == context::host() { C::Plane::Host } else { C::Plane::Frame };
                let mut genv = Env::new();
                for (name, ty, default) in &g.inputs {
                    match default {
                        Some(t) => {
                            let v = self.term(false, s, &src2, &genv, t)?;
                            let v = self.coerce(s, &src2, ty, &v)?;
                            genv.insert(name.clone(), v);
                        }
                        None => return Err(refusal("ref")),
                    }
                }
                let value = self.term(false, s, &src2, &genv, &g.body)?;
                self.plane = saved;
                Ok(value)
            }
            Node::Loop { kind: kind @ (LoopKind::For | LoopKind::Sum), accs, clauses, skip, body, zone }
                if accs.is_empty() && clauses.len() == 1 && skip.is_empty() && matches!(clauses[0].0, Pattern::Name(_)) =>
            {
                let Pattern::Name(name) = &clauses[0].0 else { unreachable!() };
                let items = self.term(false, s, source, env, &clauses[0].1)?;
                match &items {
                    Known(Value::List(items)) if items.len() <= 32 => {
                        let mut values = Vec::new();
                        for (index, item) in items.iter().enumerate() {
                            let mut src2 = source.clone();
                            src2.iter.push(C::Iteration::Static_index(index as i64));
                            let mut e2 = env.clone();
                            e2.insert(name.clone(), Known(item.clone()));
                            values.push(self.term(false, s, &src2, &e2, body)?);
                        }
                        if values.iter().all(|v| matches!(v, Known(_))) {
                            let vals: Vec<Value> = values.iter().map(|v| match v { Known(v) => v.clone(), _ => unreachable!() }).collect();
                            if *kind == LoopKind::For {
                                let ty = V::elem_ty(&vals);
                                let mut out = Vec::new();
                                for v in &vals { out.push(V::coerce_to(&ty, v)?); }
                                Ok(Known(Value::List(Arc::new(out))))
                            } else if vals.is_empty() {
                                Ok(Known(Value::Int(0)))
                            } else {
                                let mut acc = vals[0].clone();
                                for v in &vals[1..] { acc = V::arith("sum", &|a, b| a + b, &acc, v)?; }
                                Ok(Known(acc))
                            }
                        } else if *kind == LoopKind::For {
                            let mut ids = Vec::new();
                            for v in &values { ids.push(self.ensure(s, source, v)?); }
                            Ok(self.emit(s, source, &x.ty, C::Expr::Items(ids)))
                        } else if values.is_empty() {
                            Ok(Known(Value::Int(0)))
                        } else {
                            let mut acc = values[0].clone();
                            for v in &values[1..] {
                                acc = self.primitive(s, source, &x.ty, num(PO::Binary::Add), vec![("a", acc), ("b", v.clone())])?;
                            }
                            Ok(acc)
                        }
                    }
                    _ => self.loop_(s, source, env, &x.ty, *kind, &[], &[(Pattern::Name(name.clone()), None)], &[], body, zone, Some(items.clone())),
                }
            }
            Node::Loop { kind, accs, clauses, skip, body, zone } => {
                let clauses: Vec<(Pattern, Option<&Term>)> = clauses.iter().map(|(p, t)| (p.clone(), Some(t))).collect();
                self.loop_(s, source, env, &x.ty, *kind, accs, &clauses, skip, body, zone, None)
            }
            Node::Vec(_) => Err(refusal("vector")),
            Node::Lit(_) => Err(refusal("literal")),
            Node::GraphRef { .. } => Err(refusal("ref")),
        }
    }

    fn cond_arms(&mut self, borrow: bool, s: &mut Scope, source: &C::Source, env: &Env, ty: &Ty, arms: &[(Term, Term)], default: &Term) -> R<Lowered> {
        match arms.split_first() {
            None => self.term(borrow, s, source, env, default),
            Some(((condition, value), rest)) => {
                let c = self.term(false, s, source, env, condition)?;
                self.branch(s, source, ty, &c,
                    &mut |l, sc| l.term(borrow, sc, source, env, value),
                    &mut |l, sc| l.cond_arms(borrow, sc, source, env, ty, rest, default))
            }
        }
    }

    fn case_arms(&mut self, borrow: bool, s: &mut Scope, source: &C::Source, env: &Env, ty: &Ty, scrutinee: &Lowered, arms: &[(flow_syntax::syntax::Syntax, Term)], default: &Term) -> R<Lowered> {
        match arms.split_first() {
            None => self.term(borrow, s, source, env, default),
            Some(((lit, value), rest)) => {
                let lit: Value = match &lit.node {
                    SNode::Str(t) => Value::Text(t.clone()),
                    SNode::Sym(t) => Value::Bool(t == "true"),
                    SNode::Num(t) => {
                        match if t.contains('.') { None } else { t.parse::<i64>().ok() } {
                            Some(n) => Value::Int(n),
                            None => Value::Float(cfmt::parse_ocaml_float(t).ok_or_else(|| Stop::Diag(V::fail("E_INTERNAL", "float_of_string")))?),
                        }
                    }
                    _ => return Err(refusal("case")),
                };
                let condition = match scrutinee {
                    Known(sv) if C::literal_of_value(sv).is_some() => {
                        let truthy = |v: &Value| -> R<bool> { Ok(match v { Value::Bool(b) => *b, v => V::num(v)? != 0. }) };
                        Known(Value::Bool(match (sv, &lit) {
                            (Value::Bool(_), _) | (_, Value::Bool(_)) => truthy(sv)? == truthy(&lit)?,
                            (Value::Text(a), Value::Text(b)) => a == b,
                            (Value::Int(_) | Value::Float(_), Value::Int(_) | Value::Float(_)) => V::num(sv)? == V::num(&lit)?,
                            _ => false,
                        }))
                    }
                    _ if self.type_of(scrutinee) == Ty::Bool || V::ty_of(&lit) == Ty::Bool => {
                        let a = self.primitive(s, source, &Ty::Bool, C::Primitive::Truthy, vec![("value", scrutinee.clone())])?;
                        let b = Known(Value::Bool(V::truthy(&lit)?));
                        self.primitive(s, source, &Ty::Bool, C::Primitive::Equal, vec![("a", a), ("b", b)])?
                    }
                    _ if matches!(self.type_of(scrutinee), Ty::Int | Ty::Float) && matches!(V::ty_of(&lit), Ty::Int | Ty::Float) => {
                        self.primitive(s, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", scrutinee.clone()), ("b", Known(lit.clone()))])?
                    }
                    _ => self.primitive(s, source, &Ty::Bool, C::Primitive::Equal, vec![("a", scrutinee.clone()), ("b", Known(lit.clone()))])?,
                };
                self.branch(s, source, ty, &condition,
                    &mut |l, sc| l.term(borrow, sc, source, env, value),
                    &mut |l, sc| l.case_arms(borrow, sc, source, env, ty, scrutinee, rest, default))
            }
        }
    }
}

/// A filter whose function is an `fn` and whose items are an int or float `array/range` of the filter's own type.
fn filter_range<'a>(ts: &'a [Term], ty: &Ty) -> Option<(&'a Term, &'a Term, Ty)> {
    if ts.len() != 2 { return None; }
    let (f, items) = (&ts[0], &ts[1]);
    if !matches!(f.node, Node::Fn { .. }) { return None; }
    let Node::Op { op, .. } = &items.node else { return None };
    if op != "array/range" { return None; }
    let Ty::Array(elem) = &items.ty else { return None };
    if !matches!(**elem, Ty::Int | Ty::Float) { return None; }
    if *ty != Ty::Array(elem.clone()) { return None; }
    Some((f, items, (**elem).clone()))
}

impl Lowerer {
    fn packed_region(&mut self, r: ResidualId, view: &E::Residual::View) -> Option<Arc<C::RegionT>> {
        // Packed-array forms run as one region through Packed.force, unchanged; list
        // loops stay structured core. A refusal by Packed leaves the structured path.
        let is_array = |ty: &Ty| matches!(ty, Ty::Array(_));
        let sources: Option<Vec<&Term>> = match &view.term.node {
            Node::Hof(HofKind::Map | HofKind::Reduce, ts) if !ts.is_empty() => Some(ts[1..].iter().collect()),
            Node::Loop { clauses, .. } => Some(clauses.iter().map(|(_, t)| t).collect()),
            Node::Op { op, args, .. } if op == "array/sum" => Some(args.iter().map(|(_, t)| t).collect()),
            _ => None,
        };
        match sources {
            // Core keeps state in its own statements: a region over a state-reading residual stays structured
            Some(_) if E::state_dependent(&Value::Residual(r)) => None,
            Some(sources) if is_array(&view.term.ty) || sources.iter().any(|t| is_array(&t.ty)) => {
                packed::compile(&self.options, r, &view.term).filter(|region| (self.region_allowed)(region)).map(Arc::new)
            }
            _ => None,
        }
    }

    fn region(&mut self, s: &mut Scope, args: Vec<i64>, packed: Arc<C::RegionT>, view: &E::Residual::View) -> Lowered {
        let index = self.regions.len() as i64;
        self.regions.push(packed);
        let mut source = self.source_of_view(view);
        source.span = Some(view.term.form.span.clone());
        let result = self.local(&view.term.ty);
        self.push(s, &source, C::StatementNode::Invoke { region: index, args, result });
        Computed(result, view.term.ty.clone())
    }

    fn state_term(&mut self, s: &mut Scope, source: &C::Source, env: &Env, x: &Term, previous_only: bool) -> R<Lowered> {
        let Node::State { binder, init, step, zone } = &x.node else { return Err(refusal("state")) };
        if self.geometry { return Err(refusal("state")); }
        let ty = x.ty.clone();
        let numeric = ty == Ty::Int || ty == Ty::Float;
        let slot = self.next_slot;
        self.next_slot += 1;
        self.slots.push(C::StateSlot { source: source.clone(), prefix: source.prefix.clone(), zone: zone.clone(), plane: self.plane, ty: ty.clone() });
        let mut initial_body = s.fork();
        let v = self.term(false, &mut initial_body, source, env, init)?;
        let initial = self.ensure(&mut initial_body, source, &v)?;
        let previous = self.local(&ty);
        if numeric { self.mixed.insert(previous); }
        self.push(s, source, C::StatementNode::State_read { slot, result: previous, init: initial_body.st, initial });
        let previous = Computed(previous, ty.clone());
        if previous_only { return Ok(previous); }
        let env = self.bind(s, source, env.clone(), binder, previous)?;
        let found = self.local(&Ty::Bool);
        let pending = self.local(&ty);
        let result = self.local(&ty);
        if numeric { self.mixed.insert(pending); self.mixed.insert(result); }
        self.push(s, source, C::StatementNode::State_peek { slot, found, value: pending });
        let id = self.number();
        let yes_id = self.number();
        let yes = vec![statement(yes_id, source, C::StatementNode::Assign(result, C::Expr::Local(pending)))];
        let mut body = s.fork();
        let v = self.term(false, &mut body, source, &env, step)?;
        let v = self.coerce(&mut body, source, &ty, &v)?;
        let value = self.ensure(&mut body, source, &v)?;
        let wid = self.number();
        body.st.push(statement(wid, source, C::StatementNode::State_write { slot, value }));
        let aid = self.number();
        body.st.push(statement(aid, source, C::StatementNode::Assign(result, C::Expr::Local(value))));
        s.st.push(statement(id, source, C::StatementNode::If { condition: found, yes, no: body.st }));
        Ok(Computed(result, ty))
    }

    fn bind(&mut self, s: &mut Scope, source: &C::Source, mut env: Env, pattern: &Pattern, value: Lowered) -> R<Env> {
        match pattern {
            Pattern::Name(name) => { env.insert(name.clone(), value); Ok(env) }
            Pattern::Keys(keys) => {
                let value = self.resolve(s, &value)?;
                for name in keys {
                    let f = self.field(s, source, &Ty::Any, &value, name)?;
                    env.insert(name.clone(), f);
                }
                Ok(env)
            }
            Pattern::Seq(patterns) => {
                let value = self.resolve(s, &value)?;
                let parts: Vec<Lowered> = match &value {
                    Known(v) => match v {
                        Value::List(xs) => xs.iter().map(|v| Known(v.clone())).collect(),
                        Value::Vec2(x, y) => vec![float(*x), float(*y)],
                        Value::Vec3(x, y, z) => vec![float(*x), float(*y), float(*z)],
                        Value::Vec4(x, y, z, w) => vec![float(*x), float(*y), float(*z), float(*w)],
                        _ => return Err(refusal("pattern")),
                    },
                    Computed(_, ty) => match ty {
                        Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => {
                            let names: &[&str] = match ty { Ty::Vec2 => &["x", "y"], Ty::Vec3 => &["x", "y", "z"], _ => &["x", "y", "z", "w"] };
                            let mut out = Vec::new();
                            for n in names { out.push(self.field(s, source, &Ty::Float, &value, n)?); }
                            out
                        }
                        Ty::List(elem) => {
                            let count = self.primitive(s, source, &Ty::Int, C::Primitive::Sequence_length, vec![("list", value.clone())])?;
                            let short = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Lt), vec![("a", count), ("b", int(patterns.len() as i64))])?;
                            self.branch(s, source, &Ty::Int, &short,
                                &mut |l, sc| l.primitive(sc, source, &Ty::Int, C::Primitive::Fail("E_PATTERN".into(), "Not enough elements to destructure.".into()), vec![]),
                                &mut |_, _| Ok(int(0)))?;
                            let mut out = Vec::new();
                            for index in 0..patterns.len() {
                                out.push(self.primitive(s, source, elem, C::Primitive::Sequence_get, vec![("list", value.clone()), ("index", int(index as i64))])?);
                            }
                            out
                        }
                        _ => return Err(refusal("pattern")),
                    },
                    Selected(..) => {
                        let mut out = Vec::new();
                        for index in 0..patterns.len() { out.push(self.sequence_get(s, source, &value, &int(index as i64))?); }
                        out
                    }
                    Callable(_) | Record_fields(_) | Record_choice(..) => return Err(refusal("pattern")),
                };
                if parts.len() < patterns.len() { return Err(refusal("pattern")); }
                for (p, v) in patterns.iter().zip(parts.into_iter()) { env = self.bind(s, source, env, p, v)?; }
                Ok(env)
            }
        }
    }

    fn as_callable(&mut self, _source: &C::Source, l: &Lowered) -> R<Callable> {
        match l {
            Callable(f) => Ok(f.clone()),
            Known(Value::Fn(f)) => {
                let identity = E::Residual::function_id(*f);
                match (E::Residual::function_body(*f), E::Residual::function_origin(*f)) {
                    (Some((params, body)), Some((instance, prefix, zone))) => {
                        let env: Env = E::Residual::function_bindings(*f).into_iter().map(|(n, v)| (n, Known(v))).collect();
                        Ok(Callable::Closure(Arc::new(Closure { params, body, env, instance, prefix, zone, identity })))
                    }
                    _ => match E::Residual::function_name(*f) {
                        Some(name) => Ok(Callable::Named { name, identity }),
                        None => Err(refusal("fn")),
                    },
                }
            }
            _ => Err(refusal("fn")),
        }
    }

    fn call(&mut self, borrow: bool, s: &mut Scope, source: &C::Source, ty: &Ty, specialized: Option<&Term>, f: &Callable, values: &[Lowered]) -> R<Lowered> {
        let identity = identity_of(f);
        let slot = self.call_slot(identity);
        let ci = self.emit(s, source, &Ty::Int, C::Expr::Call_index(slot));
        let index = self.ensure(s, source, &ci)?;
        let mut source = source.clone();
        source.iter.push(C::Iteration::Local_index(index));
        match f {
            Callable::Closure(cl) => {
                if cl.params.len() != values.len() { return Err(refusal("call_fn")); }
                source.instance = cl.instance;
                source.prefix = cl.prefix.clone();
                source.site = cl.zone.clone();
                source.owner = cl.zone.clone();
                let mut env = cl.env.clone();
                for ((pattern, pty), value) in cl.params.iter().zip(values.iter()) {
                    let value = match pty { None => value.clone(), Some(t) => self.coerce(s, &source, t, value)? };
                    env = self.bind(s, &source, env, pattern, value)?;
                }
                let body = specialized.unwrap_or(&cl.body);
                self.term(borrow, s, &source, &env, body)
            }
            Callable::Named { name, .. } => {
                let root = self.root()?;
                if E::Residual::residual_definition(root, name).is_some() {
                    return self.definition(borrow, s, &source, name, specialized, values);
                }
                let o = match self.find_op(name)? {
                    Some(o) if o.ctx == context::value() && o.shape == op::Shape::Scalar && shipped(&o) => o,
                    _ => return Err(refusal(name)),
                };
                let names: Vec<String> = o.signature.pos.iter().chain(o.signature.opt.iter()).map(|(n, _)| n.clone()).collect();
                let mut args = Vec::new();
                for (i, v) in values.iter().enumerate() {
                    let n = if i < names.len() { names[i].clone() } else {
                        match &o.signature.rest { Some((n, _)) => n.clone(), None => return Err(refusal(name)) }
                    };
                    args.push((n, v.clone()));
                }
                match known_values(&args) {
                    Some(known) if !o.live => {
                        let nm = name.clone();
                        let _ = nm;
                        Ok(Known((o.body)(&input::at_time(0.), &|_, _| Err(refusal_diag(name)), &known)?))
                    }
                    _ => self.operation(s, &source, ty, name, &args),
                }
            }
        }
    }

    fn definition(&mut self, borrow: bool, s: &mut Scope, source: &C::Source, name: &str, specialized: Option<&Term>, values: &[Lowered]) -> R<Lowered> {
        let root = self.root()?;
        let declaration = E::Residual::residual_definition(root, name).ok_or_else(|| refusal("call_fn"))?;
        if declaration.inputs.len() != values.len() { return Err(refusal("call_fn")); }
        let zone = vec![format!("def:{name}")];
        let mut source = source.clone();
        source.prefix = source.prefix.iter().chain(source.site.iter()).cloned().collect();
        source.site = zone.clone();
        source.owner = zone;
        let mut env = Env::new();
        for ((n, ty, _), value) in declaration.inputs.iter().zip(values.iter()) {
            let v = self.coerce(s, &source, ty, value)?;
            env.insert(n.clone(), v);
        }
        self.term(borrow, s, &source, &env, specialized.unwrap_or(&declaration.body))
    }

    fn host(&mut self, s: &mut Scope, source: &C::Source, borrow: bool, args: &[(String, Lowered)], v: &Value) -> R<Lowered> {
        match v {
            Value::Source(name, _, fields) if name == "$core/argument" && matches!(fields.as_slice(), [(k, Value::Int(_))] if k == "index") => {
                let Value::Int(index) = &fields[0].1 else { unreachable!() };
                Ok(args[*index as usize].1.clone())
            }
            Value::Source(name, ty, fields) | Value::Struct(name, ty @ Ty::Array(_), fields) => {
                let mut all_known = true;
                for (_, f) in fields {
                    if !matches!(self.host(s, source, borrow, args, f)?, Known(_)) { all_known = false; break; }
                }
                if all_known { return Ok(Known(v.clone())); }
                let mut lowered = Vec::new();
                for (n, f) in fields {
                    let h = self.host(s, source, borrow, args, f)?;
                    lowered.push((n.clone(), self.ensure(s, source, &h)?));
                }
                Ok(self.emit(s, source, ty, if borrow && storage_argument(ty) {
                    C::Expr::Source_descriptor { name: name.clone(), ty: ty.clone(), args: lowered }
                } else {
                    C::Expr::Host_source { name: name.clone(), ty: ty.clone(), args: lowered }
                }))
            }
            Value::Record(fields) => {
                let mut out = Vec::new();
                for (n, f) in fields { out.push((n.clone(), self.host(s, source, borrow, args, f)?)); }
                Ok(Record_fields(out))
            }
            value => Ok(Known(value.clone())),
        }
    }

    fn op_term(&mut self, borrow: bool, s: &mut Scope, source: &C::Source, env: &Env, x: &Term, name: &str, args: &[(String, Term)]) -> R<Lowered> {
        let o = match self.find_op(name)? { Some(o) => o, None => return Err(refusal(name)) };
        let mut largs: Vec<(String, Lowered)> = Vec::new();
        for (n, t) in args {
            let v = if ty::is_cooked(&x.ty) && storage_argument(&t.ty) { self.term(true, s, source, env, t)? } else { self.term(false, s, source, env, t)? };
            largs.push((n.clone(), v));
        }
        fn data_source(v: &Value) -> bool {
            match v {
                Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => true,
                Value::Record(fields) => fields.iter().any(|(_, v)| data_source(v)),
                _ => false,
            }
        }
        let prototype: Option<Value> = if o.live { None } else {
            let proto: Vec<(String, Value)> = largs.iter().enumerate().map(|(i, (n, value))| {
                (n.clone(), match value {
                    Known(v) => v.clone(),
                    _ => Value::Source("$core/argument".into(), self.type_of(value), vec![("index".into(), Value::Int(i as i64))]),
                })
            }).collect();
            (o.body)(&input::at_time(0.), &|_, _| Err(refusal_diag(name)), &proto).ok()
        };
        if ty::is_cooked(&x.ty) {
            let mut lowered = Vec::new();
            lowered.push(("$name".to_owned(), self.ensure(s, source, &Known(text(name)))?));
            for (n, v) in &largs { lowered.push((n.clone(), self.ensure(s, source, v)?)); }
            return Ok(self.emit(s, source, &x.ty, C::Expr::Host_source { name: "$core/operator".into(), ty: x.ty.clone(), args: lowered }));
        }
        if let Some(value) = &prototype {
            if data_source(value) {
                let value = self.host(s, source, borrow, &largs, value)?;
                return if borrow { self.descriptor(s, source, &value) } else { Ok(value) };
            }
        }
        if o.ctx != context::value() || o.shape != op::Shape::Scalar || !shipped(&o) { return Err(refusal(name)); }
        match known_values(&largs) {
            Some(values) if !o.live => match (o.body)(&input::at_time(0.), &|_, _| Err(refusal_diag(name)), &values) {
                Ok(v) => Ok(Known(v)),
                Err(d) => {
                    if d.code == "$unsupported" { return Err(Stop::from(d)); }
                    self.primitive(s, source, &x.ty, failure(&d), vec![])
                }
            },
            _ => self.operation(s, source, &x.ty, name, &largs),
        }
    }

    fn calc(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, op: PO::Binary, a: &Lowered, b: &Lowered) -> R<Lowered> {
        self.primitive(s, source, ty, num(op), vec![("a", a.clone()), ("b", b.clone())])
    }

    fn hof(&mut self, s: &mut Scope, source: &C::Source, env: &Env, ty: &Ty, kind: HofKind, f: &Callable, rest: &[Term]) -> R<Lowered> {
        match (kind, rest) {
            (HofKind::Reduce, [init, items]) => {
                let init = self.term(false, s, source, env, init)?;
                let accumulator = self.hold(s, source, Some(ty), &init)?;
                let items = self.term(false, s, source, env, items)?;
                self.iterate(s, source, &items, &mut |l, sc, _, item| {
                    let value = l.call(false, sc, source, ty, None, f, &[accumulator.clone(), item.clone()])?;
                    let value = l.widen_like(sc, source, &accumulator, &value)?;
                    l.update(sc, source, &accumulator, &value)
                })?;
                Ok(accumulator)
            }
            (HofKind::Iterate, [init, count, until @ ..]) => {
                // Lower the body once; [:until] stops the counted scope after committing its next value.
                let init = self.term(false, s, source, env, init)?;
                let accumulator = self.hold(s, source, Some(ty), &init)?;
                let count = self.term(false, s, source, env, count)?;
                let mut stop = Vec::new();
                for g in until {
                    let g = self.term(false, s, source, env, g)?;
                    stop.push(self.as_callable(source, &g)?);
                }
                let finished = if stop.is_empty() { None } else { Some(self.hold(s, source, None, &boolean(false))?) };
                self.counted(MAX_INT, finished.as_ref(), s, source, &int(0), &count, &mut |l, sc, _| {
                    let value = l.call(false, sc, source, ty, None, f, &[accumulator.clone()])?;
                    let value = l.widen_like(sc, source, &accumulator, &value)?;
                    match (stop.first(), &finished) {
                        (Some(g), Some(finished)) => {
                            let next = l.hold(sc, source, Some(ty), &value)?;
                            let reached = l.call(false, sc, source, &Ty::Bool, None, g, &[accumulator.clone(), next.clone()])?;
                            l.update(sc, source, finished, &reached)?;
                            l.update(sc, source, &accumulator, &next)
                        }
                        _ => l.update(sc, source, &accumulator, &value),
                    }
                })?;
                Ok(accumulator)
            }
            (HofKind::SortBy, [items]) => {
                let items = self.term(false, s, source, env, items)?;
                self.sort_by(s, source, ty, f, &items)
            }
            (HofKind::Map | HofKind::Filter, items) => {
                // A packed result is packed storage built element by element.
                let width: i64 = match ty {
                    Ty::Array(e) => match **e {
                        Ty::Int | Ty::Text | Ty::Float | Ty::Any => 1,
                        Ty::Vec2 => 2,
                        Ty::Vec3 => 3,
                        Ty::Vec4 => 4,
                        _ => return Err(refusal("packed_higher_order")),
                    },
                    _ => 0,
                };
                let mut lowered_items = Vec::new();
                for item in items {
                    let value = self.term(false, s, source, env, item)?;
                    let homogeneous = match &value {
                        Known(Value::List(xs)) => {
                            geometry_collection(&self.type_of(&value))
                                && xs.iter().all(|v| match v { Value::Deferred(t, _) => *t == ty::geometry(), Value::No_geo => true, _ => false })
                        }
                        _ => false,
                    };
                    // Homogeneous handles need no lazy heterogeneous selection tree.
                    // Publish their builder before the loop; residual elements stay lazy.
                    lowered_items.push(if homogeneous { self.hold(s, source, None, &value)? } else { value });
                }
                let mut counts = Vec::new();
                for item in &lowered_items { counts.push(self.sequence_count(s, source, item)?); }
                let count = match counts.split_first() {
                    None => return Err(refusal("higher_order")),
                    Some((first, rest)) => {
                        let mut count = first.clone();
                        for next in rest { count = self.primitive(s, source, &Ty::Int, num(PO::Binary::Min), vec![("a", count), ("b", next.clone())])?; }
                        count
                    }
                };
                let mut build_args = vec![("count", count.clone())];
                if width > 0 {
                    build_args.push(("width", int(width)));
                    match ty {
                        Ty::Array(e) if **e == Ty::Int => build_args.push(("type", Known(text("int")))),
                        Ty::Array(e) if **e == Ty::Text => build_args.push(("type", Known(text("text")))),
                        _ => {}
                    }
                }
                let result = self.primitive(s, source, ty, C::Primitive::Sequence_build, build_args)?;
                let kept = self.hold(s, source, None, &int(0))?;
                let limit = if *ty == Ty::Array(Box::new(Ty::Int)) || *ty == Ty::Array(Box::new(Ty::Text)) { MAX_ARRAY }
                    else if width > 0 { MAX_ARRAY }
                    else if geometry_collection(ty) { MAX_ARRAY }
                    else { 4096 };
                self.counted(limit, None, s, source, &int(0), &count, &mut |l, sc, index| {
                    let mut args = Vec::new();
                    for sequence in &lowered_items { args.push(l.sequence_get(sc, source, sequence, index)?); }
                    let item = args[0].clone();
                    let result_ty = if kind == HofKind::Map { ty::elem(ty).unwrap_or(Ty::Any) } else { Ty::Bool };
                    let value = l.call(false, sc, source, &result_ty, None, f, &args)?;
                    if kind == HofKind::Map {
                        l.store(sc, source, &result, index, &value)
                    } else {
                        l.branch(sc, source, &Ty::Int, &value,
                            &mut |l2, sc2| {
                                l2.store(sc2, source, &result, &kept, &item)?;
                                l2.increment(sc2, source, &kept)?;
                                Ok(int(0))
                            },
                            &mut |_, _| Ok(int(0)))?;
                        Ok(())
                    }
                })?;
                if kind == HofKind::Filter && width > 0 {
                    self.primitive(s, source, ty, C::Primitive::Array_slice, vec![("array", result), ("first", int(0)), ("count", kept)])
                } else if kind == HofKind::Filter {
                    self.slice(s, source, ty, &result, &int(0), &kept)
                } else if width > 0 || geometry_collection(ty) {
                    Ok(result)
                } else {
                    self.primitive(s, source, ty, C::Primitive::Sequence_concat, vec![("list", result)])
                }
            }
            _ => Err(refusal("higher_order")),
        }
    }

    fn sort_by(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, f: &Callable, items: &Lowered) -> R<Lowered> {
        if matches!(ty, Ty::Array(_)) { return Err(refusal("packed_higher_order")); }
        let count = self.sequence_count(s, source, items)?;
        let keys = self.primitive(s, source, &Ty::List(Box::new(Ty::Float)), C::Primitive::Sequence_build, vec![("count", count.clone())])?;
        let b = self.primitive(s, source, &Ty::List(Box::new(Ty::Int)), C::Primitive::Sequence_build, vec![("count", count.clone())])?;
        let current = self.hold(s, source, None, &b)?;
        let b = self.primitive(s, source, &Ty::List(Box::new(Ty::Int)), C::Primitive::Sequence_build, vec![("count", count.clone())])?;
        let scratch = self.hold(s, source, None, &b)?;
        self.iterate(s, source, items, &mut |l, sc, index, item| {
            let key = l.call(false, sc, source, &Ty::Float, None, f, &[item.clone()])?;
            let key = l.primitive(sc, source, &Ty::Float, C::Primitive::Float_convert, vec![("x", key)])?;
            l.store(sc, source, &keys, index, &key)?;
            l.store(sc, source, &current, index, index)
        })?;
        let widths = Known(Value::List(Arc::new((0..12).map(|i| Value::Int(1i64 << i)).collect())));
        self.iterate(s, source, &widths, &mut |l, sc, _, width| {
            let needed = l.calc(sc, source, &Ty::Int, PO::Binary::Lt, width, &count)?;
            l.branch(sc, source, &Ty::Int, &needed,
                &mut |l, sc| {
                    let twice = l.calc(sc, source, &Ty::Int, PO::Binary::Mul, width, &int(2))?;
                    let sum = l.calc(sc, source, &Ty::Int, PO::Binary::Add, &count, &twice)?;
                    let rounded = l.calc(sc, source, &Ty::Int, PO::Binary::Sub, &sum, &int(1))?;
                    let blocks = l.primitive(sc, source, &Ty::Int, C::Primitive::Integer(PO::Integer::Idiv), vec![("a", rounded), ("b", twice.clone())])?;
                    l.counted(4096, None, sc, source, &int(0), &blocks, &mut |l, sc, block| {
                        let start = l.calc(sc, source, &Ty::Int, PO::Binary::Mul, block, &twice)?;
                        let a = l.calc(sc, source, &Ty::Int, PO::Binary::Add, &start, width)?;
                        let middle = l.calc(sc, source, &Ty::Int, PO::Binary::Min, &a, &count)?;
                        let a = l.calc(sc, source, &Ty::Int, PO::Binary::Add, &start, &twice)?;
                        let finish = l.calc(sc, source, &Ty::Int, PO::Binary::Min, &a, &count)?;
                        let left = l.hold(sc, source, None, &start)?;
                        let right = l.hold(sc, source, None, &middle)?;
                        l.counted(4096, None, sc, source, &start, &finish, &mut |l, sc, target| {
                            let left_available = l.calc(sc, source, &Ty::Int, PO::Binary::Lt, &left, &middle)?;
                            let choose_left = l.branch(sc, source, &Ty::Bool, &left_available,
                                &mut |l, sc| {
                                    let right_available = l.calc(sc, source, &Ty::Int, PO::Binary::Lt, &right, &finish)?;
                                    l.branch(sc, source, &Ty::Bool, &right_available,
                                        &mut |l, sc| {
                                            let li = l.sequence_get(sc, source, &current, &left)?;
                                            let ri = l.sequence_get(sc, source, &current, &right)?;
                                            let a = l.sequence_get(sc, source, &keys, &li)?;
                                            let b = l.sequence_get(sc, source, &keys, &ri)?;
                                            l.calc(sc, source, &Ty::Int, PO::Binary::Le, &a, &b)
                                        },
                                        &mut |_, _| Ok(boolean(true)))
                                },
                                &mut |_, _| Ok(boolean(false)))?;
                            l.branch(sc, source, &Ty::Int, &choose_left,
                                &mut |l, sc| {
                                    let value = l.sequence_get(sc, source, &current, &left)?;
                                    l.store(sc, source, &scratch, target, &value)?;
                                    l.increment(sc, source, &left)?;
                                    Ok(int(0))
                                },
                                &mut |l, sc| {
                                    let value = l.sequence_get(sc, source, &current, &right)?;
                                    l.store(sc, source, &scratch, target, &value)?;
                                    l.increment(sc, source, &right)?;
                                    Ok(int(0))
                                })?;
                            Ok(())
                        })
                    })?;
                    let old = l.hold(sc, source, None, &current)?;
                    l.update(sc, source, &current, &scratch)?;
                    l.update(sc, source, &scratch, &old)?;
                    Ok(int(0))
                },
                &mut |_, _| Ok(int(0)))?;
            Ok(())
        })?;
        let result = self.primitive(s, source, ty, C::Primitive::Sequence_build, vec![("count", count.clone())])?;
        self.counted(4096, None, s, source, &int(0), &count, &mut |l, sc, index| {
            let at = l.sequence_get(sc, source, &current, index)?;
            let value = l.sequence_get(sc, source, items, &at)?;
            l.store(sc, source, &result, index, &value)
        })?;
        Ok(result)
    }
}

/// What a loop's innermost body needs (OCaml closes over these).
struct LoopCtx {
    zone_source: C::Source,
    source: C::Source,
    init: Option<(Pattern, Lowered)>,
    attempted: Lowered,
    kept: Lowered,
    total: Option<Lowered>,
    output: Option<Lowered>,
    array_loop: bool,
    skip: Vec<Vec<usize>>,
    body: Term,
    ty: Ty,
    clauses: Vec<(Pattern, Option<Term>)>,
    first: Option<Lowered>,
}

impl Lowerer {
    fn loop_(&mut self, s: &mut Scope, source: &C::Source, env: &Env, ty: &Ty, kind: LoopKind, accs: &[(Pattern, Term)],
             clauses: &[(Pattern, Option<&Term>)], skip: &[Vec<usize>], body: &Term, zone: &Path, first: Option<Lowered>) -> R<Lowered> {
        let mut zone_source = source.clone();
        zone_source.site = zone.clone();
        zone_source.owner = zone.clone();
        let zone_source = &zone_source;
        let init = match accs {
            [(pattern, value)] => {
                let acc_ty = if kind == LoopKind::Scan { ty::elem(ty).unwrap_or(Ty::Any) } else { ty.clone() };
                let v = self.term(false, s, zone_source, env, value)?;
                Some((pattern.clone(), self.hold(s, zone_source, Some(&acc_ty), &v)?))
            }
            [] => None,
            _ => return Err(refusal("accumulator")),
        };
        let attempted = self.hold(s, zone_source, None, &int(0))?;
        let kept = self.hold(s, zone_source, None, &int(0))?;
        let total = if kind == LoopKind::Sum { Some(self.hold(s, zone_source, None, &int(0))?) } else { None };
        let collecting = kind == LoopKind::For || kind == LoopKind::Scan;
        let array_loop = clauses.iter().any(|(_, value)| match value {
            Some(v) => matches!(v.ty, Ty::Array(_)),
            None => first.as_ref().map(|f| matches!(self.type_of(f), Ty::Array(_))).unwrap_or(false),
        });
        let text_sources: Option<(Vec<Lowered>, Lowered)> = if collecting && *ty == Ty::Array(Box::new(Ty::Text)) {
            // ponytail: preallocate invariant clause products; dependent collections need
            // a growing structured builder rather than re-evaluating sources to count.
            let mut count = int(1);
            let mut sources = Vec::new();
            for (index, (_, value)) in clauses.iter().enumerate() {
                let source_ty = match value { Some(v) => v.ty.clone(), None => self.type_of(first.as_ref().ok_or_else(|| refusal("loop"))?) };
                let items = if index == 0 {
                    match value {
                        Some(v) => self.term(false, s, zone_source, env, v)?,
                        None => first.clone().ok_or_else(|| refusal("loop"))?,
                    }
                } else {
                    match value {
                        Some(v) if self.invariant(v)? && E::Residual::free_names(v).iter().all(|n| env.contains_key(n)) => {}
                        _ => return Err(refusal("text_collect_dependent_clause")),
                    }
                    let empty = match &source_ty {
                        Ty::Array(elem) => Known(V::array_init(elem, 0, &|_| unreachable!())?),
                        Ty::List(_) => Known(Value::List(Arc::new(Vec::new()))),
                        _ => return Err(refusal("text_collect_source")),
                    };
                    let nonempty = self.primitive(s, zone_source, &Ty::Bool, C::Primitive::Integer_compare(PO::Comparison::Igt), vec![("a", count.clone()), ("b", int(0))])?;
                    let value = value.unwrap();
                    self.branch(s, zone_source, &source_ty, &nonempty,
                        &mut |l, sc| l.term(false, sc, zone_source, env, value),
                        &mut |_, _| Ok(empty.clone()))?
                };
                let size = self.sequence_count(s, zone_source, &items)?;
                let zero = self.primitive(s, zone_source, &Ty::Bool, C::Primitive::Integer_compare(PO::Comparison::Ieq), vec![("a", size.clone()), ("b", int(0))])?;
                let old_count = count.clone();
                count = self.branch(s, zone_source, &Ty::Int, &zero,
                    &mut |_, _| Ok(int(0)),
                    &mut |l, sc| {
                        let limit = l.primitive(sc, zone_source, &Ty::Int, C::Primitive::Integer(PO::Integer::Idiv), vec![("a", int(MAX_ARRAY)), ("b", size.clone())])?;
                        let over = l.primitive(sc, zone_source, &Ty::Bool, C::Primitive::Integer_compare(PO::Comparison::Igt), vec![("a", old_count.clone()), ("b", limit)])?;
                        l.branch(sc, zone_source, &Ty::Int, &over,
                            &mut |l, sc| l.primitive(sc, zone_source, &Ty::Int, C::Primitive::Fail("E_ARRAY_COUNT".into(), "Array count is too large.".into()), vec![]),
                            &mut |l, sc| l.primitive(sc, zone_source, &Ty::Int, C::Primitive::Integer(PO::Integer::Imul), vec![("a", old_count.clone()), ("b", size.clone())]))
                    })?;
                sources.push(items);
            }
            Some((sources, count))
        } else {
            None
        };
        let output = if collecting {
            match &text_sources {
                Some((_, count)) => Some(self.primitive(s, zone_source, ty, C::Primitive::Sequence_build,
                    vec![("count", count.clone()), ("width", int(1)), ("type", Known(text("text")))])?),
                None => {
                    if matches!(ty, Ty::Array(_)) { return Err(refusal("packed_collect")); }
                    Some(self.primitive(s, zone_source, ty, C::Primitive::Sequence_build, vec![("count", int(4096))])?)
                }
            }
        } else {
            None
        };
        let ctx = LoopCtx {
            zone_source: zone_source.clone(), source: source.clone(), init: init.clone(), attempted, kept: kept.clone(), total: total.clone(),
            output: output.clone(), array_loop, skip: skip.to_vec(), body: body.clone(), ty: ty.clone(),
            clauses: clauses.iter().map(|(p, t)| (p.clone(), t.cloned())).collect(), first,
        };
        let sources = text_sources.as_ref().map(|(xs, _)| xs.clone()).unwrap_or_default();
        self.clauses_lower(s, env, &sources, 0, &ctx)?;
        match (output, total, init) {
            (Some(output), _, _) => {
                if *ty == Ty::Array(Box::new(Ty::Text)) {
                    self.primitive(s, zone_source, ty, C::Primitive::Array_slice, vec![("array", output), ("first", int(0)), ("count", kept)])
                } else {
                    let output = self.slice(s, zone_source, ty, &output, &int(0), &kept)?;
                    self.primitive(s, zone_source, ty, C::Primitive::Sequence_concat, vec![("list", output)])
                }
            }
            (_, Some(total), _) => Ok(total),
            (_, _, Some((_, accumulator))) => Ok(accumulator),
            _ => Err(refusal("loop")),
        }
    }

    /// A clause source that depends on nothing the loop computes (a literal-ish expression of shipped operators).
    fn invariant(&mut self, value: &Term) -> R<bool> {
        match &value.node {
            Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::RefBinding(..) => Ok(true),
            Node::Op { op, args, .. } => {
                if op == "value/rand" { return Ok(false); }
                match self.find_op(op)? {
                    Some(o) if shipped(&o) => {}
                    _ => return Ok(false),
                }
                for (_, x) in args { if !self.invariant(x)? { return Ok(false); } }
                Ok(true)
            }
            Node::Vec(xs) | Node::ListLit(xs) | Node::Str(xs) => { for x in xs { if !self.invariant(x)? { return Ok(false); } } Ok(true) }
            Node::Record(xs) => { for (_, x) in xs { if !self.invariant(x)? { return Ok(false); } } Ok(true) }
            Node::Get(x, _) | Node::Expanded { body: x, .. } => self.invariant(x),
            Node::If(a, b, c) => { for x in [a, b, c] { if !self.invariant(x)? { return Ok(false); } } Ok(true) }
            _ => Ok(false),
        }
    }

    fn clauses_lower(&mut self, s: &mut Scope, env: &Env, sources: &[Lowered], index: usize, ctx: &LoopCtx) -> R<()> {
        if index == ctx.clauses.len() {
            return self.loop_body(s, env, ctx);
        }
        let (pattern, value) = &ctx.clauses[index];
        let items = match sources.first() {
            Some(items) => items.clone(),
            None => match value {
                Some(v) => self.term(false, s, &ctx.zone_source, env, v)?,
                None => ctx.first.clone().ok_or_else(|| refusal("loop"))?,
            },
        };
        let rest: &[Lowered] = if sources.is_empty() { sources } else { &sources[1..] };
        self.iterate(s, &ctx.zone_source, &items, &mut |l, sc, _, item| {
            let env = l.bind(sc, &ctx.source, env.clone(), pattern, item.clone())?;
            l.clauses_lower(sc, &env, rest, index + 1, ctx)
        })
    }

    fn loop_body(&mut self, s: &mut Scope, env: &Env, ctx: &LoopCtx) -> R<()> {
        let zone_source = &ctx.zone_source;
        let at = self.ensure(s, zone_source, &ctx.attempted)?;
        let mut iteration = zone_source.clone();
        iteration.iter = ctx.source.iter.clone();
        iteration.iter.push(C::Iteration::Local_index(at));
        let iteration = &iteration;
        if !ctx.array_loop {
            let over = self.primitive(s, iteration, &Ty::Bool, num(PO::Binary::Ge), vec![("a", ctx.attempted.clone()), ("b", int(4096))])?;
            self.branch(s, iteration, &Ty::Int, &over,
                &mut |l, sc| l.primitive(sc, iteration, &Ty::Int, C::Primitive::Fail("E_ITER_BOUND".into(), "Loop runs more than 4,096 iterations.".into()), vec![]),
                &mut |_, _| Ok(int(0)))?;
        }
        let mut skipped = boolean(false);
        for tuple in &ctx.skip {
            if tuple.len() != iteration.iter.len() { continue; }
            let mut matches = boolean(true);
            for (index, expected) in iteration.iter.iter().zip(tuple.iter()) {
                let index = match index { C::Iteration::Static_index(n) => int(*n), C::Iteration::Local_index(n) => Computed(*n, Ty::Int) };
                let eq = self.primitive(s, iteration, &Ty::Bool, num(PO::Binary::Eq), vec![("a", index), ("b", int(*expected as i64))])?;
                matches = self.primitive(s, iteration, &Ty::Bool, num(PO::Binary::And), vec![("a", matches), ("b", eq)])?;
            }
            skipped = self.primitive(s, iteration, &Ty::Bool, num(PO::Binary::Or), vec![("a", skipped), ("b", matches)])?;
        }
        self.branch(s, iteration, &Ty::Int, &skipped,
            &mut |_, _| Ok(int(0)),
            &mut |l, sc| {
                let env = match &ctx.init {
                    None => env.clone(),
                    Some((pattern, accumulator)) => l.bind(sc, iteration, env.clone(), pattern, accumulator.clone())?,
                };
                let value = l.term(false, sc, iteration, &env, &ctx.body)?;
                let value = match &ctx.init {
                    Some((_, accumulator)) => {
                        let value = l.widen_like(sc, iteration, accumulator, &value)?;
                        l.update(sc, iteration, accumulator, &value)?;
                        accumulator.clone()
                    }
                    None => value,
                };
                if let Some(total) = &ctx.total {
                    let empty = l.primitive(sc, iteration, &Ty::Bool, num(PO::Binary::Eq), vec![("a", ctx.kept.clone()), ("b", int(0))])?;
                    let v1 = value.clone();
                    let next = l.branch(sc, iteration, &ctx.ty, &empty,
                        &mut |_, _| Ok(v1.clone()),
                        &mut |l, sc| l.primitive(sc, iteration, &ctx.ty, num(PO::Binary::Add), vec![("a", total.clone()), ("b", value.clone())]))?;
                    l.update(sc, iteration, total, &next)?;
                }
                if let Some(output) = &ctx.output {
                    l.store(sc, iteration, output, &ctx.kept, &value)?;
                }
                l.increment(sc, iteration, &ctx.kept)?;
                Ok(int(0))
            })?;
        self.increment(s, iteration, &ctx.attempted)
    }
}

impl Lowerer {
    fn operation(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, name: &str, args: &[(String, Lowered)]) -> R<Lowered> {
        let arg = |key: &str| -> R<Lowered> {
            args.iter().find(|(n, _)| n == key).map(|(_, v)| v.clone()).ok_or_else(|| Stop::Diag(V::fail("E_INTERNAL", "Not_found")))
        };
        if let Some(op) = primitive_of_name(name) {
            return self.primitive(s, source, ty, op, args.iter().map(|(n, v)| (n.as_str(), v.clone())).collect());
        }
        match name {
            "exact" => arg("value"),
            "count" => { let l = arg("list")?; self.sequence_count(s, source, &l) }
            "nth" => { let (l, i) = (arg("list")?, arg("index")?); self.sequence_get(s, source, &l, &i) }
            "range" | "take" | "drop" | "rest" | "reverse" | "first" | "last" | "array/sum" | "linspace" => self.sequence_operation(s, source, ty, name, args),
            "pointer/down" | "key/down" => {
                let (wanted, names) = if name == "key/down" { (arg("key")?, "keys") }
                    else { (args.iter().find(|(n, _)| n == "button").map(|(_, v)| v.clone()).unwrap_or_else(|| Known(Value::Text(b"left".to_vec()))), "buttons") };
                match (name, &wanted) {
                    ("pointer/down", Known(Value::Text(b))) if !["left", "right", "middle", "x1", "x2"].iter().any(|k| k.as_bytes() == b.as_slice()) => {
                        self.primitive(s, source, &Ty::Bool, C::Primitive::Fail("E_RANGE".into(), "Pointer button is left, right, middle, x1 or x2.".into()), vec![])
                    }
                    ("pointer/down", Known(Value::Text(_))) | ("key/down", _) => {
                        let input_ty = Ty::Record(vec![("buttons".into(), Ty::List(Box::new(Ty::Text))), ("keys".into(), Ty::List(Box::new(Ty::Text)))]);
                        let input = self.emit(s, source, &input_ty, C::Expr::Frame(C::FrameField::Input));
                        let held = self.field(s, source, &Ty::List(Box::new(Ty::Text)), &input, names)?;
                        let found = self.hold(s, source, None, &boolean(false))?;
                        self.iterate(s, source, &held, &mut |l, sc, _, item| {
                            let same = l.primitive(sc, source, &Ty::Bool, C::Primitive::Equal, vec![("a", item.clone()), ("b", wanted.clone())])?;
                            let or = l.primitive(sc, source, &Ty::Bool, num(PO::Binary::Or), vec![("a", found.clone()), ("b", same)])?;
                            l.update(sc, source, &found, &or)
                        })?;
                        Ok(found)
                    }
                    _ => Err(refusal(name)),
                }
            }
            "value/hsv" => self.hsv(s, source, &arg("h")?, &arg("s")?, &arg("v")?),
            "frame/dt" => Ok(self.frame(s, source, ty, C::FrameField::Dt)),
            "frame/width" => Ok(self.frame(s, source, ty, C::FrameField::Width)),
            "frame/height" => Ok(self.frame(s, source, ty, C::FrameField::Height)),
            "frame/index" => Ok(self.frame(s, source, ty, C::FrameField::Index)),
            "frame/input" => Ok(self.frame(s, source, ty, C::FrameField::Input)),
            "pointer/x" => Ok(self.frame(s, source, ty, C::FrameField::Pointer_x)),
            "pointer/y" => Ok(self.frame(s, source, ty, C::FrameField::Pointer_y)),
            "value/lerp" => {
                let u = self.to_float(s, source, &arg("u")?)?;
                let inverse = self.calc(s, source, &Ty::Float, PO::Binary::Sub, &float(1.), &u)?;
                if *ty == Ty::Vec3 {
                    let mut parts = Vec::new();
                    for name in ["x", "y", "z"] {
                        let component = |l: &mut Lowerer, sc: &mut Scope, v: &Lowered| -> R<Lowered> {
                            match v {
                                Known(Value::Vec3(..)) | Computed(_, Ty::Vec3) => l.field(sc, source, &Ty::Float, v, name),
                                _ => l.to_float(sc, source, v),
                            }
                        };
                        let a = component(self, s, &arg("a")?)?;
                        let b = component(self, s, &arg("b")?)?;
                        parts.push(self.blend(s, source, &a, &b, &u, &inverse)?);
                    }
                    self.vec(s, source, &parts)
                } else {
                    let a = self.to_float(s, source, &arg("a")?)?;
                    let b = self.to_float(s, source, &arg("b")?)?;
                    self.blend(s, source, &a, &b, &u, &inverse)
                }
            }
            "value/polar" => {
                let r = self.to_float(s, source, &arg("radius")?)?;
                let a = self.to_float(s, source, &arg("angle")?)?;
                let cosine = self.primitive(s, source, &Ty::Float, C::Primitive::Math(PO::Unary::Cos), vec![("x", a.clone())])?;
                let sine = self.primitive(s, source, &Ty::Float, C::Primitive::Math(PO::Unary::Sin), vec![("x", a)])?;
                let x = self.calc(s, source, &Ty::Float, PO::Binary::Mul, &r, &cosine)?;
                let z = self.calc(s, source, &Ty::Float, PO::Binary::Mul, &r, &sine)?;
                let height = args.iter().find(|(n, _)| n == "height").map(|(_, v)| v.clone()).unwrap_or_else(|| float(0.));
                self.vec(s, source, &[x, height, z])
            }
            "color/hsl" | "color/gradient" => self.color_operation(s, source, ty, name, args),
            "dot" => {
                let mut products = Vec::new();
                for component in ["x", "y", "z"] {
                    let a = self.field(s, source, &Ty::Float, &arg("a")?, component)?;
                    let b = self.field(s, source, &Ty::Float, &arg("b")?, component)?;
                    products.push(self.calc(s, source, &Ty::Float, PO::Binary::Mul, &a, &b)?);
                }
                let xy = self.calc(s, source, &Ty::Float, PO::Binary::Add, &products[0], &products[1])?;
                self.calc(s, source, &Ty::Float, PO::Binary::Add, &xy, &products[2])
            }
            "cross" => {
                let av = arg("a")?;
                let mut a = Vec::new();
                for c in ["x", "y", "z"] { a.push(self.field(s, source, &Ty::Float, &av, c)?); }
                let bv = arg("b")?;
                let mut b = Vec::new();
                for c in ["x", "y", "z"] { b.push(self.field(s, source, &Ty::Float, &bv, c)?); }
                let (x, y, z, u, v, w) = (&a[0], &a[1], &a[2], &b[0], &b[1], &b[2]);
                // OCaml evaluates the list literals right to left: the later operand lowers first
                let component = |l: &mut Lowerer, sc: &mut Scope, a: &Lowered, b: &Lowered, c: &Lowered, d: &Lowered| -> R<Lowered> {
                    let ab = l.calc(sc, source, &Ty::Float, PO::Binary::Mul, a, b)?;
                    let neg = l.calc(sc, source, &Ty::Float, PO::Binary::Mul, &float(-1.), c)?;
                    l.primitive(sc, source, &Ty::Float, C::Primitive::Fma, vec![("a", neg), ("b", d.clone()), ("c", ab)])
                };
                let third = component(self, s, x, v, y, u)?;
                let second = component(self, s, z, u, x, w)?;
                let first = component(self, s, y, w, z, v)?;
                self.vec(s, source, &[first, second, third])
            }
            "length" | "normalize" => {
                let vector = arg("vector")?;
                let mut components = Vec::new();
                for c in ["x", "y", "z"] { components.push(self.field(s, source, &Ty::Float, &vector, c)?); }
                let mut squares = Vec::new();
                for v in &components { squares.push(self.calc(s, source, &Ty::Float, PO::Binary::Mul, v, v)?); }
                let xy = self.calc(s, source, &Ty::Float, PO::Binary::Add, &squares[0], &squares[1])?;
                let sum = self.calc(s, source, &Ty::Float, PO::Binary::Add, &xy, &squares[2])?;
                let size = self.primitive(s, source, &Ty::Float, C::Primitive::Math(PO::Unary::Sqrt), vec![("x", sum)])?;
                if name == "length" { return Ok(size); }
                let empty = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", size.clone()), ("b", float(0.))])?;
                self.branch(s, source, &Ty::Vec3, &empty,
                    &mut |_, _| Ok(Known(Value::Vec3(0., 0., 0.))),
                    &mut |l, sc| {
                        let reciprocal = l.primitive(sc, source, &Ty::Float, num(PO::Binary::Div), vec![("a", float(1.)), ("b", size.clone())])?;
                        let mut values = Vec::new();
                        for v in &components {
                            values.push(l.primitive(sc, source, &Ty::Float, num(PO::Binary::Mul), vec![("a", v.clone()), ("b", reciprocal.clone())])?);
                        }
                        let mut ids = Vec::new();
                        for v in &values { ids.push(l.ensure(sc, source, v)?); }
                        Ok(l.emit(sc, source, &Ty::Vec3, C::Expr::Vec(ids)))
                    })
            }
            _ => Err(refusal(name)),
        }
    }

    fn to_float(&mut self, s: &mut Scope, source: &C::Source, v: &Lowered) -> R<Lowered> {
        self.primitive(s, source, &Ty::Float, C::Primitive::Float_convert, vec![("x", v.clone())])
    }

    fn vec(&mut self, s: &mut Scope, source: &C::Source, components: &[Lowered]) -> R<Lowered> {
        let mut ids = Vec::new();
        for c in components { ids.push(self.ensure(s, source, c)?); }
        Ok(self.emit(s, source, &Ty::Vec3, C::Expr::Vec(ids)))
    }

    fn blend(&mut self, s: &mut Scope, source: &C::Source, a: &Lowered, b: &Lowered, u: &Lowered, inverse: &Lowered) -> R<Lowered> {
        let left = self.calc(s, source, &Ty::Float, PO::Binary::Mul, a, inverse)?;
        let right = self.calc(s, source, &Ty::Float, PO::Binary::Mul, b, u)?;
        self.calc(s, source, &Ty::Float, PO::Binary::Add, &left, &right)
    }

    fn hsv(&mut self, s: &mut Scope, source: &C::Source, h: &Lowered, sat: &Lowered, v: &Lowered) -> R<Lowered> {
        let h = self.to_float(s, source, h)?;
        let sat = self.to_float(s, source, sat)?;
        let v = self.to_float(s, source, v)?;
        let one = float(1.);
        let fl = Ty::Float;
        let rem = self.primitive(s, source, &fl, C::Primitive::Remainder, vec![("a", h), ("b", one.clone())])?;
        let plus = self.primitive(s, source, &fl, num(PO::Binary::Add), vec![("a", rem), ("b", one.clone())])?;
        let wrapped = self.primitive(s, source, &fl, C::Primitive::Remainder, vec![("a", plus), ("b", one.clone())])?;
        let scaled = self.primitive(s, source, &fl, num(PO::Binary::Mul), vec![("a", wrapped), ("b", float(6.))])?;
        let floor = self.primitive(s, source, &fl, C::Primitive::Floor, vec![("x", scaled.clone())])?;
        let sector = self.primitive(s, source, &Ty::Int, C::Primitive::Int_convert, vec![("x", floor)])?;
        let sector_f = self.primitive(s, source, &fl, C::Primitive::Float_convert, vec![("x", sector.clone())])?;
        let f = self.primitive(s, source, &fl, num(PO::Binary::Sub), vec![("a", scaled), ("b", sector_f)])?;
        let m = self.primitive(s, source, &Ty::Int, C::Primitive::Integer(PO::Integer::Imod), vec![("a", sector), ("b", int(6))])?;
        // arm k builds one of the six sectors
        self.hsv_arm(s, source, &m, &v, &sat, &f, &one, 0)
    }

    fn hsv_arm(&mut self, s: &mut Scope, source: &C::Source, m: &Lowered, v: &Lowered, sat: &Lowered, f: &Lowered, one: &Lowered, k: i64) -> R<Lowered> {
        if k == 5 { return self.hsv_build(s, source, v, sat, f, one, k); }
        let is = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", m.clone()), ("b", int(k))])?;
        self.branch(s, source, &Ty::Vec3, &is,
            &mut |l, sc| l.hsv_build(sc, source, v, sat, f, one, k),
            &mut |l, sc| l.hsv_arm(sc, source, m, v, sat, f, one, k + 1))
    }

    fn hsv_build(&mut self, s: &mut Scope, source: &C::Source, v: &Lowered, sat: &Lowered, f: &Lowered, one: &Lowered, k: i64) -> R<Lowered> {
        let fl = Ty::Float;
        let mul = |l: &mut Lowerer, sc: &mut Scope, a: &Lowered, b: &Lowered| l.calc(sc, source, &fl, PO::Binary::Mul, a, b);
        let sub = |l: &mut Lowerer, sc: &mut Scope, a: &Lowered, b: &Lowered| l.calc(sc, source, &fl, PO::Binary::Sub, a, b);
        // low = v (1 - s); falling = v (1 - f s); rising = v (1 - (1 - f) s)
        let low = |l: &mut Lowerer, sc: &mut Scope| -> R<Lowered> { let t = sub(l, sc, one, sat)?; mul(l, sc, v, &t) };
        let falling = |l: &mut Lowerer, sc: &mut Scope| -> R<Lowered> { let fs = mul(l, sc, f, sat)?; let t = sub(l, sc, one, &fs)?; mul(l, sc, v, &t) };
        let rising = |l: &mut Lowerer, sc: &mut Scope| -> R<Lowered> { let g = sub(l, sc, one, f)?; let gs = mul(l, sc, &g, sat)?; let t = sub(l, sc, one, &gs)?; mul(l, sc, v, &t) };
        let components = match k {
            0 => { let t = rising(self, s)?; let p = low(self, s)?; vec![v.clone(), t, p] }
            1 => { let q = falling(self, s)?; let p = low(self, s)?; vec![q, v.clone(), p] }
            2 => { let p = low(self, s)?; let t = rising(self, s)?; vec![p, v.clone(), t] }
            3 => { let p = low(self, s)?; let q = falling(self, s)?; vec![p, q, v.clone()] }
            4 => { let t = rising(self, s)?; let p = low(self, s)?; vec![t, p, v.clone()] }
            _ => { let p = low(self, s)?; let q = falling(self, s)?; vec![v.clone(), p, q] }
        };
        self.vec(s, source, &components)
    }

    fn sequence_operation(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, name: &str, args: &[(String, Lowered)]) -> R<Lowered> {
        let arg = |key: &str| -> R<Lowered> {
            args.iter().find(|(n, _)| n == key).map(|(_, v)| v.clone()).ok_or_else(|| Stop::Diag(V::fail("E_INTERNAL", "Not_found")))
        };
        match name {
            "range" => {
                let (first, last) = match args.iter().find(|(n, _)| n == "end") {
                    Some((_, last)) => (arg("count")?, last.clone()),
                    None => (int(0), arg("count")?),
                };
                let first = self.ensure(s, source, &first)?;
                let last = self.ensure(s, source, &last)?;
                let count = self.local(&Ty::Int);
                let index = self.local(&Ty::Int);
                let item = self.local(&Ty::Int);
                let id = self.number();
                let mut init = s.fork();
                let result = self.primitive(&mut init, source, ty, C::Primitive::Sequence_build, vec![("count", Computed(count, Ty::Int))])?;
                let mut body = s.fork();
                self.store(&mut body, source, &result, &Computed(index, Ty::Int), &Computed(item, Ty::Int))?;
                s.st.push(statement(id, source, C::StatementNode::Counted { first, last, count, index, item, limit: 4096, stop: None, init: init.st, body: body.st }));
                Ok(result)
            }
            "take" | "drop" | "rest" => {
                let sequence = arg("list")?;
                let count = self.sequence_count(s, source, &sequence)?;
                let wanted = if name == "rest" { int(1) } else {
                    let n = arg("n")?;
                    let f = self.to_float(s, source, &n)?;
                    self.coerce(s, source, &Ty::Int, &f)?
                };
                let retained = self.primitive(s, source, &Ty::Int, num(PO::Binary::Max), vec![("a", int(0)), ("b", wanted)])?;
                let retained = self.primitive(s, source, &Ty::Int, num(PO::Binary::Min), vec![("a", retained), ("b", count.clone())])?;
                let (first, length) = if name == "take" { (int(0), retained) } else {
                    let length = self.primitive(s, source, &Ty::Int, num(PO::Binary::Sub), vec![("a", count), ("b", retained.clone())])?;
                    (retained, length)
                };
                self.primitive(s, source, ty, C::Primitive::Sequence_slice, vec![("list", sequence), ("first", first), ("count", length)])
            }
            "reverse" => {
                let sequence = arg("list")?;
                let count = self.sequence_count(s, source, &sequence)?;
                let result = self.primitive(s, source, ty, C::Primitive::Sequence_build, vec![("count", count.clone())])?;
                self.iterate(s, source, &sequence, &mut |l, sc, index, item| {
                    let last = l.primitive(sc, source, &Ty::Int, num(PO::Binary::Sub), vec![("a", count.clone()), ("b", int(1))])?;
                    let reverse = l.primitive(sc, source, &Ty::Int, num(PO::Binary::Sub), vec![("a", last), ("b", index.clone())])?;
                    l.store(sc, source, &result, &reverse, item)
                })?;
                Ok(result)
            }
            "first" | "last" => {
                let sequence = arg("list")?;
                let index = if name == "first" { int(0) } else {
                    let c = self.sequence_count(s, source, &sequence)?;
                    self.primitive(s, source, &Ty::Int, num(PO::Binary::Sub), vec![("a", c), ("b", int(1))])?
                };
                self.sequence_get(s, source, &sequence, &index)
            }
            "array/sum" => {
                let zero = match ty {
                    Ty::Int => Value::Int(0),
                    Ty::Vec2 => Value::Vec2(0., 0.),
                    Ty::Vec3 => Value::Vec3(0., 0., 0.),
                    Ty::Vec4 => Value::Vec4(0., 0., 0., 0.),
                    _ => Value::Float(0.),
                };
                let total = self.hold(s, source, Some(ty), &Known(zero))?;
                let array = arg("array")?;
                self.iterate(s, source, &array, &mut |l, sc, _, item| {
                    let op = if *ty == Ty::Int { C::Primitive::Integer(PO::Integer::Iadd) } else { num(PO::Binary::Add) };
                    let v = l.primitive(sc, source, ty, op, vec![("a", total.clone()), ("b", item.clone())])?;
                    l.update(sc, source, &total, &v)
                })?;
                Ok(total)
            }
            "linspace" => {
                let a = self.to_float(s, source, &arg("from")?)?;
                let b = self.to_float(s, source, &arg("to")?)?;
                let n = arg("count")?;
                let first = self.ensure(s, source, &int(0))?;
                let last = self.ensure(s, source, &n)?;
                let count = self.local(&Ty::Int);
                let index = self.local(&Ty::Int);
                let item = self.local(&Ty::Int);
                let id = self.number();
                let mut init = s.fork();
                let result = self.primitive(&mut init, source, ty, C::Primitive::Sequence_build, vec![("count", Computed(count, Ty::Int))])?;
                let mut body = s.fork();
                let single = self.primitive(&mut body, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", n.clone()), ("b", int(1))])?;
                let value = self.branch(&mut body, source, &Ty::Float, &single,
                    &mut |_, _| Ok(a.clone()),
                    &mut |l, sc| {
                        let k = l.primitive(sc, source, &Ty::Float, C::Primitive::Float_convert, vec![("x", Computed(item, Ty::Int))])?;
                        let m = l.primitive(sc, source, &Ty::Int, num(PO::Binary::Sub), vec![("a", n.clone()), ("b", int(1))])?;
                        let steps = l.primitive(sc, source, &Ty::Float, C::Primitive::Float_convert, vec![("x", m)])?;
                        let span = l.primitive(sc, source, &Ty::Float, num(PO::Binary::Sub), vec![("a", b.clone()), ("b", a.clone())])?;
                        let scaled = l.primitive(sc, source, &Ty::Float, num(PO::Binary::Mul), vec![("a", span), ("b", k)])?;
                        let quotient = l.primitive(sc, source, &Ty::Float, num(PO::Binary::Div), vec![("a", scaled), ("b", steps)])?;
                        l.primitive(sc, source, &Ty::Float, num(PO::Binary::Add), vec![("a", a.clone()), ("b", quotient)])
                    })?;
                self.store(&mut body, source, &result, &Computed(index, Ty::Int), &value)?;
                s.st.push(statement(id, source, C::StatementNode::Counted { first, last, count, index, item, limit: 4096, stop: None, init: init.st, body: body.st }));
                Ok(result)
            }
            _ => Err(refusal(name)),
        }
    }
}

impl Lowerer {
    /// `branch` for a non-record type whose else arm ignores the arm's own scope and lowers into the
    /// caller's (the nested `arm` of color/hsl in the OCaml): the nested comparison and branch land
    /// in the outer statement list before this branch's own statement.
    fn branch_outer(&mut self, s: &mut Scope, source: &C::Source, ty: &Ty, condition: &Lowered, yes: Lower,
                    no_outer: &mut dyn FnMut(&mut Lowerer, &mut Scope) -> R<Lowered>) -> R<Lowered> {
        let condition = self.ensure(s, source, condition)?;
        let id = self.number();
        let result = self.local(ty);
        if *ty == Ty::Float { self.mixed.insert(result); }
        let yes_body = self.arm(s, source, result, yes)?;
        let mut body = s.fork();
        let v = no_outer(self, s)?;
        let value = self.ensure(&mut body, source, &v)?;
        let aid = self.number();
        body.st.push(statement(aid, source, C::StatementNode::Assign(result, C::Expr::Local(value))));
        s.st.push(statement(id, source, C::StatementNode::If { condition, yes: yes_body, no: body.st }));
        Ok(Computed(result, ty.clone()))
    }

    fn rgb(&mut self, s: &mut Scope, source: &C::Source, r: &Lowered, g: &Lowered, b: &Lowered) -> R<Lowered> { self.vec(s, source, &[r.clone(), g.clone(), b.clone()]) }

    fn hsl_arm(&mut self, s: &mut Scope, source: &C::Source, hue: &Lowered, limit: f64, k: u8, chroma: &Lowered, x: &Lowered, zero: &Lowered) -> R<Lowered> {
        let cond = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Lt), vec![("a", hue.clone()), ("b", float(limit))])?;
        let (c, xx, z) = (chroma.clone(), x.clone(), zero.clone());
        match k {
            1 => self.branch_outer(s, source, &Ty::Vec3, &cond,
                &mut |l, sc| l.rgb(sc, source, &c, &xx, &z),
                &mut |l, sc| l.hsl_arm(sc, source, hue, 2., 2, chroma, x, zero)),
            2 => self.branch_outer(s, source, &Ty::Vec3, &cond,
                &mut |l, sc| l.rgb(sc, source, &xx, &c, &z),
                &mut |l, sc| l.hsl_arm(sc, source, hue, 3., 3, chroma, x, zero)),
            3 => self.branch_outer(s, source, &Ty::Vec3, &cond,
                &mut |l, sc| l.rgb(sc, source, &z, &c, &xx),
                &mut |l, sc| l.hsl_arm(sc, source, hue, 4., 4, chroma, x, zero)),
            4 => self.branch_outer(s, source, &Ty::Vec3, &cond,
                &mut |l, sc| l.rgb(sc, source, &z, &xx, &c),
                &mut |l, sc| l.hsl_arm(sc, source, hue, 5., 5, chroma, x, zero)),
            _ => self.branch(s, source, &Ty::Vec3, &cond,
                &mut |l, sc| l.rgb(sc, source, &xx, &z, &c),
                &mut |l, sc| l.rgb(sc, source, &c, &z, &xx)),
        }
    }

    fn color_operation(&mut self, s: &mut Scope, source: &C::Source, _ty: &Ty, name: &str, args: &[(String, Lowered)]) -> R<Lowered> {
        let arg = |key: &str| -> R<Lowered> {
            args.iter().find(|(n, _)| n == key).map(|(_, v)| v.clone()).ok_or_else(|| Stop::Diag(V::fail("E_INTERNAL", "Not_found")))
        };
        let fl = Ty::Float;
        match name {
            "color/hsl" => {
                let h = self.to_float(s, source, &arg("h")?)?;
                let sa = self.to_float(s, source, &arg("s")?)?;
                let l = self.to_float(s, source, &arg("l")?)?;
                let f = float;
                let m60 = self.calc(s, source, &fl, PO::Binary::Mod, &h, &f(360.))?;
                let hue = self.calc(s, source, &fl, PO::Binary::Div, &m60, &f(60.))?;
                let sat = self.unit(s, source, &sa)?;
                let light = self.unit(s, source, &l)?;
                let two_l = self.calc(s, source, &fl, PO::Binary::Mul, &f(2.), &light)?;
                let d = self.calc(s, source, &fl, PO::Binary::Sub, &two_l, &f(1.))?;
                let a = self.primitive(s, source, &fl, C::Primitive::Math(PO::Unary::Abs), vec![("x", d)])?;
                let e = self.calc(s, source, &fl, PO::Binary::Sub, &f(1.), &a)?;
                let chroma = self.calc(s, source, &fl, PO::Binary::Mul, &e, &sat)?;
                let hm = self.calc(s, source, &fl, PO::Binary::Mod, &hue, &f(2.))?;
                let d = self.calc(s, source, &fl, PO::Binary::Sub, &hm, &f(1.))?;
                let a = self.primitive(s, source, &fl, C::Primitive::Math(PO::Unary::Abs), vec![("x", d)])?;
                let e = self.calc(s, source, &fl, PO::Binary::Sub, &f(1.), &a)?;
                let x = self.calc(s, source, &fl, PO::Binary::Mul, &chroma, &e)?;
                let zero = f(0.);
                let triple = self.hsl_arm(s, source, &hue, 1., 1, &chroma, &x, &zero)?;
                let half = self.calc(s, source, &fl, PO::Binary::Div, &chroma, &f(2.))?;
                let m = self.calc(s, source, &fl, PO::Binary::Sub, &light, &half)?;
                let mut channels = Vec::new();
                for name in ["x", "y", "z"] {
                    let fld = self.field(s, source, &fl, &triple, name)?;
                    let sum = self.calc(s, source, &fl, PO::Binary::Add, &fld, &m)?;
                    let byte = self.primitive(s, source, &Ty::Int, C::Primitive::Color_byte(C::ByteMode::Clamped), vec![("x", sum)])?;
                    let as_float = self.primitive(s, source, &fl, C::Primitive::Float_convert, vec![("x", byte)])?;
                    channels.push(self.calc(s, source, &fl, PO::Binary::Div, &as_float, &f(255.))?);
                }
                self.vec(s, source, &channels)
            }
            "color/gradient" => {
                // Constant opaque stops only; anything else keeps the walker's diagnostics.
                let stops = arg("stops")?;
                let colors = match &stops {
                    Known(Value::List(xs)) if !xs.is_empty() => {
                        let mut out = Vec::new();
                        for x in xs.iter() { out.push(op::drawing_color(x).map_err(|_| refusal(name))?); }
                        out
                    }
                    _ => return Err(refusal(name)),
                };
                if colors.iter().any(|c| c.a != 255) { return Err(refusal(name)); }
                let n = colors.len();
                if n == 1 {
                    let (r, g, b, _) = rays_math::color::to_floats(colors[0]);
                    return Ok(Known(Value::Vec3(r, g, b)));
                }
                let f = float;
                let x = self.to_float(s, source, &arg("x")?)?;
                let u = self.unit(s, source, &x)?;
                let position = self.calc(s, source, &fl, PO::Binary::Mul, &u, &f((n - 1) as f64))?;
                let as_int = self.primitive(s, source, &Ty::Int, C::Primitive::Int_convert, vec![("x", position.clone())])?;
                let left = self.primitive(s, source, &Ty::Int, num(PO::Binary::Min), vec![("a", int(n as i64 - 2)), ("b", as_int)])?;
                let lf = self.primitive(s, source, &fl, C::Primitive::Float_convert, vec![("x", left.clone())])?;
                let diff = self.calc(s, source, &fl, PO::Binary::Sub, &position, &lf)?;
                let pct = self.unit(s, source, &diff)?;
                let inverse = self.calc(s, source, &fl, PO::Binary::Sub, &f(1.), &pct)?;
                self.gradient_arms(s, source, &colors, &left, &inverse, &pct, 0)
            }
            _ => Err(refusal(name)),
        }
    }

    fn unit(&mut self, s: &mut Scope, source: &C::Source, v: &Lowered) -> R<Lowered> {
        let lo = self.calc(s, source, &Ty::Float, PO::Binary::Min, &float(1.), v)?;
        self.calc(s, source, &Ty::Float, PO::Binary::Max, &float(0.), &lo)
    }

    fn gradient_arms(&mut self, s: &mut Scope, source: &C::Source, colors: &[rays_math::color::T], left: &Lowered, inverse: &Lowered, pct: &Lowered, k: usize) -> R<Lowered> {
        let n = colors.len();
        if k == n - 2 { return self.gradient_pick(s, source, colors, inverse, pct, k); }
        let here = self.primitive(s, source, &Ty::Bool, num(PO::Binary::Eq), vec![("a", left.clone()), ("b", int(k as i64))])?;
        self.branch(s, source, &Ty::Vec3, &here,
            &mut |l, sc| l.gradient_pick(sc, source, colors, inverse, pct, k),
            &mut |l, sc| l.gradient_arms(sc, source, colors, left, inverse, pct, k + 1))
    }

    fn gradient_pick(&mut self, s: &mut Scope, source: &C::Source, colors: &[rays_math::color::T], inverse: &Lowered, pct: &Lowered, k: usize) -> R<Lowered> {
        let (c1, c2) = (colors[k], colors[k + 1]);
        let mut out = Vec::new();
        let picks: [fn(rays_math::color::T) -> i64; 3] = [|c| c.r, |c| c.g, |c| c.b];
        for pick in picks {
            let fl = Ty::Float;
            let a = self.calc(s, source, &fl, PO::Binary::Mul, &float(pick(c1) as f64), inverse)?;
            let b = self.calc(s, source, &fl, PO::Binary::Mul, &float(pick(c2) as f64), pct)?;
            let ab = self.calc(s, source, &fl, PO::Binary::Add, &a, &b)?;
            let sum = self.calc(s, source, &fl, PO::Binary::Add, &ab, &float(0.5))?;
            let byte = self.primitive(s, source, &Ty::Int, C::Primitive::Int_convert, vec![("x", sum)])?;
            let as_float = self.primitive(s, source, &fl, C::Primitive::Float_convert, vec![("x", byte)])?;
            out.push(self.calc(s, source, &fl, PO::Binary::Div, &as_float, &float(255.))?);
        }
        self.vec(s, source, &out)
    }
}

// ---------------------------------------------------------------------------------------------

fn stop_name(stop: Stop) -> String {
    match stop { Stop::Unsupported(name) => name, Stop::Diag(_) => "constant".into() }
}

struct DrawBridge { l: Lowerer, s: Scope }
impl DrawContext for DrawBridge {
    fn value(&mut self, source: &C::Source, value: &Value) -> Result<i64, String> {
        self.l.ensure(&mut self.s, source, &Known(value.clone())).map_err(stop_name)
    }
    fn local(&mut self, ty: &Ty) -> i64 { self.l.local(ty) }
    fn emit(&mut self, source: &C::Source, node: C::StatementNode) { self.l.push(&mut self.s, source, node); }
}

fn new_lowerer(value: Option<Value>, trace: Option<Trace>, budget: Option<i64>, lowered: Option<LoweredHook>, strict: bool,
               region_allowed: Option<RegionAllowed>, has_draw: bool, root: Option<ResidualId>) -> Lowerer {
    Lowerer {
        options: packed::CompileOptions { trace, ..Default::default() }, strict,
        region_allowed: region_allowed.unwrap_or_else(|| Rc::new(|_| true)),
        has_draw, initial: value, root, root_ops: None,
        lowered_hook: lowered.unwrap_or_else(|| Rc::new(|_| ())),
        constants: Vec::new(), locals: Vec::new(), next_statement: 0,
        budget: budget.unwrap_or(if has_draw { 32768 } else { 2048 }),
        active: HashSet::new(), calls: HashMap::new(), next_call: 0, next_function: -1,
        mixed: HashSet::new(), current: None, step_nodes: HashSet::new(), regions: Vec::new(), slots: Vec::new(), next_slot: 0,
        plane: C::Plane::Frame, geometry: false,
    }
}

fn program_of(l: Lowerer, body: Vec<C::Statement>, results: Vec<i64>) -> C::Program {
    constant_storage(C::Program {
        constants: l.constants, locals: l.locals, body, results, state_slots: l.slots, regions: l.regions, call_slots: l.next_call,
    })
}

fn lower_value(l: Lowerer) -> Result<C::Program, String> {
    let mut l = l;
    let mut s = Scope::fresh();
    let run = (|| -> R<Vec<i64>> {
        let root = l.root()?;
        let view = l.view(root)?;
        let mut source = l.source_of_view(&view);
        source.span = Some(view.term.form.span.clone());
        let result = match l.initial.clone() {
            None => l.residual(&mut s, root)?,
            Some(v) => Known(v),
        };
        Ok(vec![l.ensure(&mut s, &source, &result)?])
    })();
    match run {
        Ok(results) => Ok(program_of(l, s.st, results)),
        Err(e) => Err(stop_name(e)),
    }
}

static COUNT_SCOPES: AtomicI64 = AtomicI64::new(0);

fn argument_index(residual: ResidualId, term: &Term) -> Option<i64> {
    match &term.node {
        Node::RefBinding(name, fields) if fields.is_empty() => {
            let view = E::Residual::residual_view(residual).ok()?;
            view.bindings.iter().find(|(n, _)| n == name).and_then(|(_, v)| argument_value(v))
        }
        Node::Expanded { body, .. } => argument_index(residual, body),
        _ => None,
    }
}
fn argument_value(v: &Value) -> Option<i64> {
    match v {
        Value::Source(name, _, fields) if name == "$core/argument" => match fields.as_slice() {
            [(k, Value::Int(index))] if k == "index" => Some(*index),
            _ => None,
        },
        Value::Residual(r) => argument_index(*r, &E::Residual::residual_view(*r).ok()?.term),
        _ => None,
    }
}

/// Count identities refer to single-write locals in this program only. Compaction
/// gets a new identity; a multi-source product never inherits an input's count.
fn count_origins(program: C::Program) -> C::Program {
    let scope = COUNT_SCOPES.fetch_add(1, Ordering::SeqCst);
    let n = program.locals.len();
    let mut writes = vec![0i64; n];
    let mut origins: Vec<Option<C::Region::Origin>> = vec![None; n];
    C::iter(&mut |st| for l in C::writes(st) { writes[l as usize] += 1; }, &program.body);
    let identity = |kind: &str, local: i64| -> Option<C::Region::Origin> {
        Some((vec!["@core-count".to_owned(), scope.to_string(), kind.to_owned(), local.to_string()], Vec::new()))
    };
    let count = |args: &[(String, i64)], writes: &[i64]| -> Option<C::Region::Origin> {
        args.iter().find(|(n, _)| n == "count").and_then(|(_, local)| if writes[*local as usize] == 1 { identity("scalar", *local) } else { None })
    };
    let mut regions = program.regions.clone();
    for st in &program.body {
        match &st.node {
            C::StatementNode::Assign(local, expression) if writes[*local as usize] == 1 => {
                let origin = match expression {
                    C::Expr::Local(old) => origins[*old as usize].clone(),
                    C::Expr::Prim(C::Primitive::Coerce(_), args) if args.len() == 1 => origins[args[0].1 as usize].clone(),
                    C::Expr::Prim(C::Primitive::Array_range | C::Primitive::Array_int | C::Primitive::Array_float | C::Primitive::Array_text | C::Primitive::Array_vec3 | C::Primitive::Array_vec4, args) => count(args, &writes),
                    C::Expr::Source_descriptor { name, args, .. } if name.starts_with("$core/constant-array/") => count(args, &writes),
                    _ => None,
                };
                if matches!(program.locals[*local as usize], Ty::Array(_)) {
                    origins[*local as usize] = match origin { Some(o) => Some(o), None => identity("array", *local) };
                }
            }
            C::StatementNode::Invoke { region, args, result } => {
                let packed = regions[*region as usize].clone();
                if matches!(program.locals[*result as usize], Ty::Array(_)) && writes[*result as usize] == 1 {
                    origins[*result as usize] = identity("array", *result);
                }
                if packed::elementwise(&packed) && packed.skip.is_empty() && (packed.iteration == C::Region::Iteration::Zip || packed.sources.len() == 1) {
                    let inputs: Vec<Option<C::Region::Origin>> = packed.sources.iter().map(|(residual, term)| {
                        argument_index(*residual, term).and_then(|index| {
                            if index < 0 || index as usize >= args.len() { None } else { origins[args[index as usize] as usize].clone() }
                        })
                    }).collect();
                    if !inputs.is_empty() && inputs[0].is_some() && inputs.iter().all(|i| *i == inputs[0]) {
                        let rebuilt = C::Region::rebuild(&packed, C::Region::Rebuild { origin: Some(inputs[0].clone()), ..Default::default() });
                        if let Ok(region_t) = rebuilt {
                            regions[*region as usize] = Arc::new(region_t);
                            if writes[*result as usize] == 1 { origins[*result as usize] = inputs[0].clone(); }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    C::Program { regions, ..program }
}

/// Frame lowering of a residual (see the OCaml interface for what lowers).
pub fn lower(value: Option<Value>, trace: Option<Trace>, strict: bool, budget: Option<i64>, lowered: Option<LoweredHook>, root: ResidualId) -> Result<C::Program, String> {
    let result = lower_value(new_lowerer(value, trace, budget, lowered, strict, None, false, Some(root)));
    if static_sop(root) { result.map(count_origins) } else { result }
}

/// A program whose statements `build` writes, with no result. `root` stands for the evaluation
/// (operators, definitions, state) and is read only when a residual is lowered.
pub fn lower_draw(region_allowed: Option<RegionAllowed>, root: Option<ResidualId>, build: &mut dyn FnMut(&mut dyn DrawContext) -> Result<(), String>) -> Result<C::Program, String> {
    let l = new_lowerer(None, None, None, None, false, region_allowed, true, root);
    let mut bridge = DrawBridge { l, s: Scope::fresh() };
    build(&mut bridge)?;
    let DrawBridge { l, s } = bridge;
    Ok(program_of(l, s.st, Vec::new()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use flow_check::catalog::Catalog;
    use flow_syntax::syntax;

    fn empty() -> Catalog { Catalog { version: 1, kinds: vec![] } }

    /// `(workspace w (graph g :context value BODY))`, checked.
    pub fn checked(body: &str) -> W::Workspace {
        let forms = syntax::parse(&format!("(workspace w (graph g :context value {body}))")).expect("parse");
        match W::check(&[], false, false, None, &empty(), &forms) {
            (Some(ws), ds) if ds.is_empty() => ws,
            (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
        }
    }

    fn result_g(e: &E::Eval) -> Value { e.results.iter().find(|(n, _)| n == "g").unwrap().1.clone() }

    const LOWER_EXPRESSIONS: [&str; 11] = [
        "(sum [k (range 6)] (/ (sin (* (+ t k) (+ (* 2 k) 1))) (+ (* 2 k) 1)))",
        "(let* [a 3 b (* a 2)] (+ (* t b) (mod t 0.7)))",
        "(if (> (sin t) 0.2) (pow t 2) (- 0 t))",
        "(let* [v [(* t 2) (+ t 1) (min t 3)]] (+ v.x v.y))",
        "(nth (for [i (range 4)] (* i t)) 2)",
        "(sum [x (for [i (range 4)] (+ (* i t) 1))] (* x 2))",
        "(sum [i (range 5)] (sum [j (range 3)] (* (+ t i) j)))",
        "(let* [a t a1 (+ a a) a2 (+ a1 a1) a3 (+ a2 a2)] (* a3 0.5))",
        "(let* [c (value/lerp [1 2 3] [t 0 1] 0.25)] c.z)",
        "(sum [i (range 0)] t)",
        "(/ t 0)",
    ];

    fn dump_value(out: &mut String, v: &Value) {
        match v {
            Value::Residual(r) => match lower(None, None, false, None, None, *r) {
                Ok(program) => {
                    let printed = C::pp(&program);
                    assert_eq!(printed, C::pp(&lower(None, None, false, None, None, *r).unwrap()));
                    out.push_str(&printed);
                }
                Err(name) => out.push_str(&format!("refused {name}\n")),
            },
            Value::List(xs) => xs.iter().for_each(|x| dump_value(out, x)),
            Value::Record(xs) => xs.iter().for_each(|(_, x)| dump_value(out, x)),
            v => out.push_str(&format!("constant {}\n", String::from_utf8_lossy(&E::show(v)))),
        }
    }

    #[test]
    fn lowering_matches_the_ocaml_snapshot() {
        let mut out = String::new();
        for (i, body) in LOWER_EXPRESSIONS.iter().enumerate() {
            out.push_str(&format!("expression {i}: {body}\n"));
            let evaluation = E::static_eval(&checked(&format!("{body}"))).expect("static");
            dump_value(&mut out, &result_g(&evaluation));
        }
        let expected = include_str!("../../../../lib/flow_ir/core_lower.expected").replace("\r\n", "\n");
        if out != expected {
            let first = out.lines().zip(expected.lines()).position(|(a, b)| a != b).unwrap_or(0);
            panic!("Core lowering changed at line {}:\nactual:   {:?}\nexpected: {:?}", first + 1, out.lines().nth(first), expected.lines().nth(first));
        }
    }

    fn chain(n: usize) -> String {
        format!("(let* [a0 t {}] a{n})", (0..n).map(|i| format!("a{} (+ a{} t)", i + 1, i)).collect::<Vec<_>>().join(" "))
    }

    /// Lisp input depth is bounded by stack, not by the lowering: the app runs on a 256 MB stack (plan section 9).
    fn big_stack(f: fn()) { std::thread::Builder::new().stack_size(256 << 20).spawn(f).unwrap().join().unwrap(); }

    #[test]
    fn budget_refuses_and_a_larger_budget_admits() { big_stack(budget_refuses_and_a_larger_budget_admits_body) }
    fn budget_refuses_and_a_larger_budget_admits_body() {
        let body = chain(1100);
        let evaluation = E::static_eval(&checked(&body)).expect("static");
        let Value::Residual(r) = result_g(&evaluation) else { panic!("residual") };
        assert_eq!(lower(None, None, false, None, None, r).err().as_deref(), Some("budget"));
        assert!(lower(None, None, false, Some(32768), None, r).is_ok());
    }

    #[test]
    fn a_residual_has_no_literal() {
        let evaluation = E::static_eval(&checked("(+ t 1)")).expect("static");
        let Value::Residual(r) = result_g(&evaluation) else { panic!("residual") };
        assert!(C::literal_of_value(&Value::Residual(r)).is_none());
    }

    // ---- the tests that execute lowered programs (test_core_lower.ml) ----

    use crate::core_exec::{self, Options};
    use flow_check::workspace::Paths;
    use input::Frame;
    use rays_diagnostic::Diagnostic;
    use std::sync::Mutex;

    fn exec(program: &C::Program, live: &Frame, options: Options) -> Result<Value, Diagnostic> {
        core_exec::execute(&core_exec::prepare(program, true, false, &Paths::new(), false), live, &options)
    }

    fn reference(value: &Value, live: &Frame) -> Result<Value, Diagnostic> { E::Host::force_reference(value, live, None, None, None) }

    fn lowered(r: ResidualId) -> C::Program { lower(None, None, false, None, None, r).expect("lowers") }

    fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool { format!("{a:?}") == format!("{b:?}") }

    fn observed_fold_program_agrees_with_the_walker_body() {
        let observed = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let flag = observed.clone();
        let handlers = E::ResidualHandlers {
            observe: Some(Arc::new(move |r| {
                let view = E::Residual::residual_view(r).unwrap();
                if matches!(view.term.node, Node::Loop { kind: LoopKind::Fold, .. }) {
                    flag.store(true, Ordering::SeqCst);
                    let program = lowered(r);
                    for time in [0., 0.37, 2.] {
                        let live = input::at_time(time);
                        assert!(same(&exec(&program, &live, Options::default()), &reference(&Value::Residual(r), &live)));
                    }
                }
            })),
            ..Default::default()
        };
        let ws = checked("(fold [s 0.] [i (range 2)] (+ s (+ t i)))");
        E::Residual::handle(handlers, &mut || E::Host::static_with_requests(None, false, None, &ws)).expect("static");
        assert!(observed.load(Ordering::SeqCst));
    }

    #[test]
    fn a_fold_lowered_while_observed_agrees_with_the_walker() { observed_fold_program_agrees_with_the_walker_body() }

    fn marker(name: &str, ty: Ty, id: i64) -> op::Op {
        let base = op::find(&[], "frame/dt", &context::value()).expect("frame/dt");
        let out_ty = ty.clone();
        op::Op {
            name: name.into(),
            signature: op::Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] },
            out: Arc::new(move |_| Ok(out_ty.clone())),
            out_args: None,
            live: false,
            arithmetic: None,
            packed_extension: None,
            body: Arc::new(move |_, _, _| Ok(Value::Deferred(ty.clone(), id))),
            ..base
        }
    }

    fn check_with(ops: &[op::Op], source: &str) -> W::Workspace {
        let forms = syntax::parse(source).expect("parse");
        match W::check(ops, false, false, None, &empty(), &forms) {
            (Some(ws), ds) if ds.is_empty() => ws,
            (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
        }
    }

    fn recorded(e: &E::Eval, path: &[&str]) -> Value {
        let p: Vec<String> = path.iter().map(|s| s.to_string()).collect();
        e.records.iter().find(|(q, _)| *q == p).expect("record").1[0].1.clone()
    }

    #[test]
    fn cooked_handles_have_exact_types_lazy_failures_and_fresh_validation() {
        let geometry = ty::geometry();
        let image = ty::image();
        let ops = vec![marker("handle_geometry", geometry.clone(), 11), marker("handle_image", image.clone(), 11)];
        let compiled = |body: &str| -> core_exec::Prepared {
            let ws = check_with(&ops, &format!(
                "(workspace handles (graph g :context value [(selected : bool false) (geo : geometry (handle_geometry)) (img : image (handle_image)) (bad : geometry (handle_geometry))] (let* [tested (if selected {body} {body})] 0.)))"));
            let inputs = vec![("g".to_owned(), vec![
                ("selected".to_owned(), Value::Source("test/selected".into(), Ty::Bool, vec![])),
                ("geo".to_owned(), Value::Deferred(geometry.clone(), 11)),
                ("img".to_owned(), Value::Deferred(image.clone(), 11)),
                ("bad".to_owned(), Value::Deferred(geometry.clone(), 99)),
            ])];
            let evaluated = E::r#static(&ws, true, Some(&inputs)).expect("static");
            let Value::Residual(r) = recorded(&evaluated, &["g", "tested"]) else { panic!("Expected cooked residual: {body}") };
            core_exec::prepare(&lower(None, None, true, None, None, r).expect("strict lowering"), false, false, &Paths::new(), false)
        };
        let reason = {
            let mut d = V::fail("E_TEST_HANDLE", "Unknown cooked handle.");
            d.cause = Some("unknown handle cause".into());
            d
        };
        let run = |prepared: &core_exec::Prepared, time: f64, allow_bad: bool| -> (Result<Value, Diagnostic>, Vec<(Ty, i64)>) {
            let calls: Arc<Mutex<Vec<(Ty, i64)>>> = Arc::new(Mutex::new(Vec::new()));
            let log = calls.clone();
            let (geometry, image, reason) = (geometry.clone(), image.clone(), reason.clone());
            let resolve: core_exec::ResolveFn = Arc::new(move |v: &Value| match v {
                Value::Source(name, Ty::Bool, f) if name == "test/selected" && f.is_empty() => {
                    log.lock().unwrap().push((Ty::Bool, 0));
                    Ok(Value::Bool(time > 0.))
                }
                Value::Source(name, ty, f) if name == "$core/cooked" => {
                    let Some((_, Value::Int(id))) = f.first() else { panic!("cooked id") };
                    log.lock().unwrap().push((ty.clone(), *id));
                    if (*ty == geometry || *ty == image) && (*id == 11 || (allow_bad && *id == 99)) { Ok(Value::Deferred(ty.clone(), *id)) } else { Err(reason.clone()) }
                }
                _ => panic!("unexpected source"),
            });
            let result = core_exec::execute(prepared, &input::at_time(time), &Options { resolve: Some(resolve), ..Default::default() });
            let out = calls.lock().unwrap().clone();
            (result, out)
        };
        let count = |calls: &[(Ty, i64)], key: &(Ty, i64)| calls.iter().filter(|c| *c == key).count();
        let mixed = compiled("(if selected {:a geo :b geo :picture img} {:a geo :b geo :picture img})");
        for time in [-1., 1., -1., 1.] {
            let (result, calls) = run(&mixed, time, false);
            assert_eq!(result, Ok(Value::Record(vec![
                ("a".into(), Value::Deferred(geometry.clone(), 11)),
                ("b".into(), Value::Deferred(geometry.clone(), 11)),
                ("picture".into(), Value::Deferred(image.clone(), 11)),
            ])));
            assert_eq!(count(&calls, &(geometry.clone(), 11)), 1);
            assert_eq!(count(&calls, &(image.clone(), 11)), 1);
            assert_eq!(count(&calls, &(Ty::Bool, 0)), 2);
        }
        let lazy_bad = compiled("(if selected {:a bad :b bad} {:a geo :b geo})");
        for time in [-1., 1., -1., 1.] {
            let (result, calls) = run(&lazy_bad, time, false);
            if time > 0. {
                let d = result.expect_err("bad handle");
                assert!(d.code == reason.code && d.message == reason.message && d.cause == reason.cause);
            } else {
                assert_eq!(result, Ok(Value::Record(vec![("a".into(), Value::Deferred(geometry.clone(), 11)), ("b".into(), Value::Deferred(geometry.clone(), 11))])));
                assert!(!calls.contains(&(geometry.clone(), 99)));
            }
        }
        let crossing = compiled("(let* [chosen (if selected geo bad)] {:first chosen :after geo})");
        for time in [1., -1., 1., -1.] {
            let (result, calls) = run(&crossing, time, true);
            assert_eq!(result, Ok(Value::Record(vec![
                ("first".into(), Value::Deferred(geometry.clone(), if time > 0. { 11 } else { 99 })),
                ("after".into(), Value::Deferred(geometry.clone(), 11)),
            ])));
            assert!(calls.contains(&(geometry.clone(), 11)));
        }
    }

    #[test]
    fn constant_arrays_keep_original_site_validation_and_cooked_storage_aliases() {
        let base = op::find(&[], "frame/dt", &context::value()).expect("frame/dt");
        let storage = op::Op {
            name: "test/storage".into(),
            signature: op::Signature { pos: vec![("values".into(), Ty::Array(Box::new(Ty::Vec4)))], opt: vec![], rest: None, kw: vec![("later".into(), Ty::Float)] },
            out: Arc::new(|_| Ok(ty::geometry())),
            out_args: None,
            live: false,
            arithmetic: None,
            packed_extension: None,
            body: Arc::new(|_, _, args| Ok(Value::Source("test/storage".into(), ty::geometry(), args.to_vec()))),
            ..base
        };
        let program = |body: &str| -> C::Program {
            let ws = check_with(&[storage.clone()], &format!("(workspace constants (graph g :context value (let* [tested {body}] 0.)))"));
            let evaluated = E::r#static(&ws, true, None).expect("static");
            let Value::Residual(r) = recorded(&evaluated, &["g", "tested"]) else { panic!("Expected constructor residual") };
            lower(None, None, true, None, None, r).expect("strict lowering")
        };
        let count_descriptors = |program: &C::Program| {
            let mut count = 0;
            C::iter(&mut |s| {
                if let C::StatementNode::Assign(_, C::Expr::Source_descriptor { name, .. }) = &s.node {
                    if name.starts_with("$core/constant-array/") { count += 1; }
                }
            }, &program.body);
            count
        };
        let run = |program: &C::Program, time: f64| -> (Result<Value, Diagnostic>, Option<Value>) {
            let received: Arc<Mutex<Option<Value>>> = Arc::new(Mutex::new(None));
            let log = received.clone();
            let resolve: core_exec::ResolveFn = Arc::new(move |v: &Value| match v {
                Value::Source(name, _, args) if name == "$core/operator" => {
                    *log.lock().unwrap() = args.iter().find(|(k, _)| k == "values").map(|(_, v)| v.clone());
                    Ok(Value::Deferred(ty::geometry(), 1))
                }
                _ => panic!("unexpected source"),
            });
            let result = exec(program, &input::at_time(time), Options { resolve: Some(resolve), ..Default::default() });
            let out = received.lock().unwrap().clone();
            (result, out)
        };
        let only = program("(let* [colors (array/vec4 (int t) [-0.0 1.0 2.0 3.0]) alias colors] (test/storage (exact alias)))");
        assert_eq!(count_descriptors(&only), 1);
        for time in [0., 1., 32769.] {
            let (result, received) = run(&only, time);
            assert_eq!(result, Ok(Value::Deferred(ty::geometry(), 1)));
            match received {
                Some(Value::Source(name, Ty::Array(e), f)) if *e == Ty::Vec4 && matches!(f.as_slice(), [(c, Value::Int(_)), (v, _)] if c == "count" && v == "value") => {
                    let Value::Int(n) = f[0].1 else { unreachable!() };
                    assert!(name == "$core/constant-array/array/vec4" && n == time as i64);
                }
                other => panic!("Unexpected constant descriptor: {other:?}\n{}", C::pp(&only)),
            }
        }
        let mixed = program("(let* [colors (array/vec4 (int t) [-0.0 1.0 2.0 3.0])] (test/storage colors :later (float (array/count colors))))");
        assert_eq!(count_descriptors(&mixed), 0);
        match run(&mixed, 3.).1 {
            Some(Value::Vec4_array(data)) => assert_eq!(data.len(), 12),
            other => panic!("{other:?}"),
        }
        let ambiguous = program("(let* [colors (array/vec4 (int t) [-0.0 1.0 2.0 3.0]) alias (if (> t 1.) colors colors)] (test/storage alias))");
        assert_eq!(count_descriptors(&ambiguous), 0);
        let early = program("(test/storage (array/vec4 (int t) [-0.0 1.0 2.0 3.0]) :later (if (> t -100.) (error \"E_LATER\" \"later argument\") 0.))");
        assert_eq!(run(&early, -1.).0.expect_err("range").code, "E_ARRAY_RANGE");
        assert_eq!(run(&early, 1.).0.expect_err("later").code, "E_LATER");
    }

    #[test]
    fn frame_facts_are_shared_per_scope_and_agree_with_the_walker() {
        let cases = [
            ("(+ (* (sin t) 0.5) (* t t))", 1),
            ("(+ (+ t t) (+ (+ (frame/dt) (frame/dt)) (+ (pointer/x) (pointer/x))))", 1),
            ("(+ (if (> (frame/index) 0) t 0.) t)", 2),
            ("(let* [total (fold [a 0.] [i (range (frame/index))] (+ a t))] (+ total t))", 2),
        ];
        for (body, expected_time) in cases {
            let evaluation = E::static_eval(&checked(body)).expect("static");
            let value = result_g(&evaluation);
            let Value::Residual(r) = value.clone() else { panic!("Expected live frame facts: {body}") };
            let program = lowered(r);
            let mut times = 0;
            C::iter(&mut |s| { if matches!(s.node, C::StatementNode::Assign(_, C::Expr::Frame(C::FrameField::Time))) { times += 1; } }, &program.body);
            assert_eq!(times, expected_time, "{body}");
            let prepared = core_exec::prepare(&program, false, false, &Paths::new(), false);
            for (time, frame) in [(-1., 0), (0., 1), (0.37, 2), (2., 0)] {
                let mut live = input::at_time(time);
                live.frame = frame;
                live.dt = 0.125;
                live.pointer = (3., 4.);
                let expected = reference(&value, &live);
                let actual = core_exec::execute(&prepared, &live, &Options::default());
                assert!(same(&actual, &expected), "{body} at {time}");
            }
        }
    }

    #[test]
    fn the_scalar_path_matches_the_walker_bit_for_bit() {
        for body in ["(+ (* (sin t) 0.5) (* t t))", "(- (cos t) (* t (* t 1e10)))"] {
            let evaluation = E::static_eval(&checked(body)).expect("static");
            let Value::Residual(r) = result_g(&evaluation) else { panic!("residual") };
            let program = lowered(r);
            for time in [0., 0.37, 1e154, 1e200] {
                let live = input::at_time(time);
                assert!(same(&exec(&program, &live, Options::default()), &reference(&Value::Residual(r), &live)), "{body} at {time}");
            }
        }
    }

    #[test]
    fn a_capture_inside_a_host_source_lowers_to_plain_host_source_arguments() {
        let evaluation = E::static_eval(&checked("(let* [s (+ t 1.)] (+ s 1.))")).expect("static");
        let Value::Residual(r) = result_g(&evaluation) else { panic!("residual") };
        let view = E::Residual::residual_view(r).unwrap();
        let Some((_, Value::Residual(inner))) = view.bindings.iter().find(|(n, _)| n == "s").cloned() else { panic!("inner") };
        let r = E::Residual::with_bindings(r, &view.term, &[("s".to_owned(), Value::Source("test/source".into(), Ty::Float, vec![("value".into(), Value::Residual(inner))]))]).unwrap();
        let program = lowered(r);
        let mut host = false;
        C::iter(&mut |s| match &s.node {
            C::StatementNode::Assign(_, C::Expr::Host_source { name, .. }) if name == "test/source" => host = true,
            C::StatementNode::Assign(_, C::Expr::Source(_)) => panic!("a captured source stays plain data"),
            _ => {}
        }, &program.body);
        assert!(host);
        let prepared = core_exec::prepare(&program, false, false, &Paths::new(), false);
        let live = input::at_time(2.);
        let resolve: core_exec::ResolveFn = Arc::new(|v: &Value| match v {
            Value::Source(name, _, f) if name == "test/source" => Ok(f[0].1.clone()),
            _ => panic!("unexpected source"),
        });
        let with = |offer: Result<Option<Value>, Diagnostic>| {
            let host_source: core_exec::HostSourceFn = Arc::new(move |_| offer.clone());
            core_exec::execute(&prepared, &live, &Options { resolve: Some(resolve.clone()), host_source: Some(host_source), ..Default::default() })
        };
        assert_eq!(with(Ok(None)), Ok(Value::Float(4.)));
        assert_eq!(with(Ok(Some(Value::Float(8.)))), Ok(Value::Float(9.)));
        assert_eq!(with(Ok(Some(Value::Int(8)))).expect_err("wrong declared type").code, "E_DATA_SOURCE");
        assert_eq!(with(Ok(Some(Value::Source("unresolved".into(), Ty::Float, vec![])))).expect_err("unresolved").code, "E_DATA_SOURCE");
    }
}
