//! Port of lib/flow_ir/core_region.ml: lowers a checked packed form (map, reduce, numeric loop,
//! array/sum) to a core region. It reads terms; everything after it reads the region only.
//!
//! OCaml exceptions: `Unsupported` / `Compile_refused d` / `Diagnostic.Error d` are the variants of
//! [`Stop`]; `Invalid_argument` (an out-of-bounds register read, `Option.get`) is `Stop::Invalid`.
//! OCaml evaluates function arguments, tuples, constructor and array-literal fields right to left;
//! every place where that order changes the numbering of emitted registers is written out as
//! explicit lets in OCaml's actual order.
use crate::core_ir::Region::{
    self as R, CountSource, Instruction as I, Iteration, LiveFloat, LiveInt, Loop, Parts, Projection, RegionResult, Stage,
};
use crate::core_ir::RegionT;
use flow_check::op::{self, PackedKind};
use flow_check::workspace::{self as W, HofKind, LoopKind, Node, Path, Pattern, Term};
use flow_eval::Residual;
use flow_syntax::context;
use flow_syntax::packed_ops::{self, Binary, Comparison, Conversion, Integer, Unary};
use flow_syntax::syntax::Node as SNode;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{self as V, ResidualId, Value};
use flow_syntax::{lisp, Diagnostic};
use rays_diagnostic::Span;
use std::cell::RefCell;
use std::collections::HashMap;

const MAX_INT: i64 = 4_611_686_018_427_387_903;

#[derive(Clone, Debug)]
pub enum Stop {
    Unsupported,
    /// Compile-time refusals bypass the lazy runtime `Diagnostic.Error` handler.
    Refused(Diagnostic),
    /// OCaml `Diagnostic.Error`.
    Error(Diagnostic),
    /// OCaml `Invalid_argument`.
    Invalid(String),
}
impl From<Diagnostic> for Stop {
    fn from(d: Diagnostic) -> Stop { Stop::Error(d) }
}
type Res<X> = Result<X, Stop>;

#[derive(Clone, Debug)]
struct Expression {
    registers: Vec<i64>,
    constant: Option<Value>,
    fields: Vec<(String, Expression)>,
}
fn regs(registers: Vec<i64>) -> Expression { Expression { registers, constant: None, fields: Vec::new() } }
fn one(register: i64) -> Expression { regs(vec![register]) }
fn fields_of(fields: Vec<(String, Expression)>) -> Expression { Expression { registers: Vec::new(), constant: None, fields } }

fn diag(span: Option<Span>, code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), span, None, None, Vec::new(), Vec::new())
}
fn refusal(at: &Term, code: &str, message: &str) -> Stop {
    Stop::Refused(diag(Some(at.form.span.clone()), code, message))
}
fn invalid(message: &str) -> Stop { Stop::Invalid(message.to_owned()) }
/// `e.registers.(i)`.
fn reg(e: &Expression, i: usize) -> Res<i64> {
    e.registers.get(i).copied().ok_or_else(|| invalid("index out of bounds"))
}
fn array_of(ty: Ty) -> Ty { Ty::Array(Box::new(ty)) }

#[derive(Clone, Debug, PartialEq)]
pub enum InstructionKey {
    Constant(u64),
    GuardedConstant(Vec<(i64, bool)>, u64),
    Instruction(I),
}

/// Sharing preserves the binary64 bits of constants, including signed zero.
pub fn instruction_key(instruction: &I) -> InstructionKey {
    match instruction {
        I::Const(value) => InstructionKey::Constant(value.to_bits()),
        I::Guarded(guards, body) => match body.as_ref() {
            I::Const(value) => InstructionKey::GuardedConstant(guards.clone(), value.to_bits()),
            _ => InstructionKey::Instruction(instruction.clone()),
        },
        other => InstructionKey::Instruction(other.clone()),
    }
}

fn width_opt(ty: &Ty) -> Option<i64> { packed_ops::scalar_width(ty).map(|w| w as i64) }
fn width_res(ty: &Ty) -> Res<i64> { width_opt(ty).ok_or(Stop::Unsupported) }
/// The scalar width of a type; 0 where OCaml raises `Unsupported`.
pub fn width_of(ty: &Ty) -> i64 { width_opt(ty).unwrap_or(0) }

fn packed_list(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) if !items.is_empty() => {
            let ty = V::ty_of(&items[0]);
            if matches!(ty, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) && items.iter().all(|item| V::ty_of(item) == ty) {
                V::array_init(&ty, items.len() as i64, &|i| items[i].clone()).ok()
            } else {
                None
            }
        }
        _ => None,
    }
}

/// The float components of a scalar or vector value.
pub fn components(value: &Value) -> Result<Vec<f64>, Diagnostic> {
    Ok(match value {
        Value::Vec2(x, y) => vec![*x, *y],
        Value::Vec3(x, y, z) => vec![*x, *y, *z],
        Value::Vec4(x, y, z, w) => vec![*x, *y, *z, *w],
        other => vec![V::num(other)?],
    })
}

fn field(at: &Term, e: &Expression, name: &str) -> Res<Expression> {
    if let Some((_, value)) = e.fields.iter().find(|(n, _)| n == name) {
        return Ok(value.clone());
    }
    let component: i64 = match name { "x" => 0, "y" => 1, "z" => 2, "w" => 3, _ => -1 };
    if e.registers.len() >= 2 && component >= 0 && (component as usize) < e.registers.len() {
        Ok(one(e.registers[component as usize]))
    } else {
        Err(refusal(at, "E_PACKED_TYPE", &format!("Component {} is unavailable on a {}-component packed value.", name, e.registers.len())))
    }
}

fn same_carry_type(a: &Ty, b: &Ty) -> bool {
    match (a, b) {
        (Ty::Record(a), Ty::Record(b)) => {
            a.len() == b.len() && a.iter().all(|(name, ty)| b.iter().find(|(n, _)| n == name).map(|(_, t2)| same_carry_type(ty, t2)).unwrap_or(false))
        }
        _ => a == b,
    }
}

/// Fill each loop header's closing register after its body has been emitted.
pub fn finish_loops(finishes: &HashMap<i64, i64>, code: Vec<I>) -> Vec<I> {
    fn patch(instruction: I, finish: i64) -> I {
        match instruction {
            I::Loop_begin(loop_) => I::Loop_begin(Loop { finish, ..loop_ }),
            I::Guarded(guards, body) => I::Guarded(guards, Box::new(patch(*body, finish))),
            other => other,
        }
    }
    code.into_iter()
        .enumerate()
        .map(|(slot, instruction)| match finishes.get(&(slot as i64)) {
            Some(finish) => patch(instruction, *finish),
            // OCaml's Hashtbl.find raises Not_found for a slot without a finish: only headers have one.
            None => instruction,
        })
        .collect()
}

fn integer_work(instruction: &I) -> bool {
    match instruction {
        I::Int_const(_) => false,
        I::Int_input(_) | I::Int_uniform(_) | I::Int_frame(_) | I::Int_accumulator | I::Int_binary(..) | I::Int_compare(..) | I::Int_abs(_)
        | I::Convert(..) | I::Int_select(..) | I::Rand_at(..) | I::Trace(..) | I::Raise(..) | I::Array_prepare(..) | I::Gather(..)
        | I::Int_gather(..) | I::Text_const(_) | I::Text_input(_) | I::Text_uniform(_) | I::Text_gather(..) | I::Text_select(..)
        | I::Loop_begin(_) | I::Loop_carry(..) | I::Loop_end(..) | I::Loop_result(..) => true,
        I::Guarded(_, body) => integer_work(body),
        _ => false,
    }
}

fn paths(view: &Residual::View, sites: &mut Vec<(i64, Path, Vec<i64>)>, t: &Term) {
    // `sites` is the OCaml list reversed: its head is the last element.
    if let Some(path) = &t.path {
        sites.push((view.instance, path.clone(), view.iter.clone()));
    }
    let children: Vec<Term> = match &t.node {
        Node::Bypass(call) => W::bypass_input(call).into_iter().collect(),
        _ => W::children(t),
    };
    for child in &children {
        paths(view, sites, child);
    }
}

fn iteration_skips(iter: &[i64], skip: &[Vec<usize>]) -> Vec<i64> {
    let mut out: Vec<i64> = skip
        .iter()
        .filter_map(|tuple| {
            let (k, prefix) = tuple.split_last()?;
            let prefix: Vec<i64> = prefix.iter().map(|x| *x as i64).collect();
            if prefix == iter { Some(*k as i64) } else { None }
        })
        .collect();
    out.sort();
    out.dedup();
    out
}

pub(crate) fn same_op(a: &op::Op, b: &op::Op) -> bool {
    // OCaml compares physically; a clone shares every Arc.
    a.name == b.name && a.ctx == b.ctx && a.signature == b.signature && a.live == b.live && a.category == b.category
        && std::sync::Arc::ptr_eq(&a.out, &b.out) && std::sync::Arc::ptr_eq(&a.check, &b.check) && std::sync::Arc::ptr_eq(&a.body, &b.body)
}

fn canonical_operator(residual: ResidualId, name: &str) -> bool {
    let extra = Residual::residual_ops(residual);
    match (op::find(&extra, name, &context::value()), op::find(&[], name, &context::value())) {
        (Some(actual), Some(builtin)) => same_op(&actual, &builtin),
        _ => false,
    }
}

/// A field of a live record (a matrix chosen per frame): the field of the one residual; a live
/// vector's component stays a component of its one uniform.
fn field_residual(r: ResidualId, value: &Value, field: &str) -> Res<Value> {
    let view = Residual::residual_view(r)?;
    let ty = match &view.term.ty {
        Ty::Record(fs) => match fs.iter().find(|(n, _)| n == field) {
            Some((_, ty)) => ty.clone(),
            None => return Err(Stop::Unsupported),
        },
        _ => return Err(Stop::Unsupported),
    };
    let term = Term { path: None, ty, node: Node::RefBinding("$field".to_owned(), vec![field.to_owned()]), form: view.term.form.clone() };
    let bound = Residual::with_bindings(r, &term, &[("$field".to_owned(), value.clone())])?;
    Ok(Value::Residual(bound))
}

fn captured_value(bindings: &[(String, Value)], name: &str, fields: &[String]) -> Res<Value> {
    let mut value = bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()).ok_or(Stop::Unsupported)?;
    for field in fields {
        value = match (&value, field.as_str()) {
            (Value::Residual(r), f) => field_residual(*r, &value, f)?,
            (Value::Record(fs), f) | (Value::Struct(_, _, fs), f) => fs.iter().find(|(n, _)| n == f).map(|(_, v)| v.clone()).ok_or(Stop::Unsupported)?,
            (Value::Fn(function), f) => Residual::function_bindings(*function).into_iter().find(|(n, _)| n == f).map(|(_, v)| v).ok_or(Stop::Unsupported)?,
            (Value::Vec3(x, _, _), "x") | (Value::Vec3(_, x, _), "y") | (Value::Vec3(_, _, x), "z") => Value::Float(*x),
            (Value::Vec2(x, _), "x") | (Value::Vec2(_, x), "y") => Value::Float(*x),
            (Value::Vec4(x, _, _, _), "x") | (Value::Vec4(_, x, _, _), "y") | (Value::Vec4(_, _, x, _), "z") | (Value::Vec4(_, _, _, x), "w") => Value::Float(*x),
            _ => return Err(Stop::Unsupported),
        };
    }
    Ok(value)
}

type Params = Vec<(Pattern, Option<Ty>)>;
type Scope = Option<(String, Vec<String>)>;

fn definition_body(at: &Term, residual: ResidualId, name: &str) -> Res<(Params, Term, Scope)> {
    match Residual::residual_definition(residual, name) {
        Some(definition) => Ok((definition.inputs.iter().map(|(n, ty, _)| (Pattern::Name(n.clone()), Some(ty.clone()))).collect(), definition.body.clone(), None)),
        None => Err(refusal(at, "E_PACKED_FUNCTION", &format!("Packed helper {} is unavailable.", name))),
    }
}

fn helper_body(at: &Term, residual: ResidualId, bindings: &[(String, Value)], function: &str, name: &str, path: &[String]) -> Res<(Params, Term, Scope)> {
    match captured_value(bindings, name, path) {
        Ok(Value::Fn(callable)) => match Residual::function_body(callable) {
            Some((params, body)) => Ok((params, body, Some((name.to_owned(), path.to_vec())))),
            None => match Residual::function_name(callable) {
                Some(name) => definition_body(at, residual, &name),
                None => Err(refusal(at, "E_PACKED_FUNCTION", "Captured helper has no checked body.")),
            },
        },
        _ => definition_body(at, residual, function),
    }
}

fn literal_function(f: &Term) -> bool {
    match &f.node {
        Node::Fn { capture: Some(value), .. } => literal_function(value),
        Node::Expanded { body, .. } => literal_function(body),
        Node::Fn { capture: None, .. } => true,
        _ => false,
    }
}

/// The element count of a source known before running.
pub fn count_of(residual: ResidualId, s: &Term) -> Option<i64> {
    match &s.node {
        Node::RefBinding(name, fields) => {
            let view = Residual::residual_view(residual).ok()?;
            match captured_value(&view.bindings, name, fields) {
                Ok(v @ (Value::Int_array(_) | Value::Text_array(_) | Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_))) => {
                    V::array_length(&v).ok().map(|n| n as i64)
                }
                _ => None,
            }
        }
        Node::Op { op, args, .. } if matches!(op.as_str(), "array/int" | "array/text" | "array/float" | "array/vec3" | "array/vec4" | "array/range") => {
            match args.iter().find(|(n, _)| n == "count") {
                Some((_, Term { node: Node::Lit(param::Value::Int(n)), .. })) if *n >= 0 => Some(*n),
                _ => None,
            }
        }
        _ => None,
    }
}

fn count_of_sources(iteration: Iteration, counts: &[Option<i64>]) -> Res<Option<i64>> {
    if counts.is_empty() || (iteration == Iteration::Product && counts.iter().any(|c| *c == Some(0))) {
        return Ok(Some(0));
    }
    if counts.iter().all(|c| c.is_some()) {
        let mut n = if iteration == Iteration::Zip { MAX_INT } else { 1 };
        for c in counts {
            let b = c.unwrap();
            n = match iteration {
                Iteration::Zip => n.min(b),
                Iteration::Product => {
                    if b != 0 && n > MAX_INT / b { return Err(Stop::Unsupported); }
                    n * b
                }
            };
        }
        Ok(Some(n))
    } else {
        Ok(None)
    }
}

fn pattern_names(p: &Pattern) -> Vec<String> {
    match p {
        Pattern::Name(name) => vec![name.clone()],
        Pattern::Keys(names) => names.clone(),
        Pattern::Seq(ps) => ps.iter().flat_map(pattern_names).collect(),
    }
}

fn reads_names(names: &[String], term: &Term) -> bool {
    let any = |ts: &[&Term]| ts.iter().any(|t| reads_names(names, t));
    fn of_fields(fs: &[(String, Term)]) -> Vec<&Term> { fs.iter().map(|(_, t)| t).collect() }
    match &term.node {
        Node::RefBinding(name, _) => names.contains(name),
        Node::Fn { params, body, .. } => {
            let bound: Vec<String> = params.iter().flat_map(|(p, _)| pattern_names(p)).collect();
            let remaining: Vec<String> = names.iter().filter(|n| !bound.contains(n)).cloned().collect();
            reads_names(&remaining, body)
        }
        Node::Let(bindings, body) => {
            let mut names: Vec<String> = names.to_vec();
            for (p, t) in bindings {
                if reads_names(&names, t) { return true; }
                let bound = pattern_names(p);
                names.retain(|n| !bound.contains(n));
            }
            reads_names(&names, body)
        }
        Node::Op { args, .. } | Node::Call { args, .. } | Node::Record(args) => any(&of_fields(args)),
        Node::CallFn { args, .. } => any(&args.iter().collect::<Vec<_>>()),
        Node::GraphRef { inputs, .. } => any(&of_fields(inputs)),
        Node::Vec(ts) | Node::ListLit(ts) | Node::Str(ts) | Node::ListOp(_, ts) | Node::Hof(_, ts) => any(&ts.iter().collect::<Vec<_>>()),
        Node::Loop { accs, clauses, body, .. } => {
            let mut all: Vec<&Term> = vec![body];
            all.extend(accs.iter().chain(clauses.iter()).map(|(_, t)| t));
            any(&all)
        }
        Node::State { init, step, .. } => any(&[init, step]),
        Node::WithError(a, b) => any(&[a, b]),
        Node::ErrorRows { rows, mapper, .. } => any(&[rows, mapper]),
        Node::If(c, a, b) => any(&[c, a, b]),
        Node::Cond(arms, d) => {
            let mut all: Vec<&Term> = vec![d];
            for (a, b) in arms { all.push(a); all.push(b); }
            any(&all)
        }
        Node::Case(s, arms, d) => {
            let mut all: Vec<&Term> = vec![s, d];
            all.extend(arms.iter().map(|(_, t)| t));
            any(&all)
        }
        Node::Get(body, _) | Node::Expanded { body, .. } => reads_names(names, body),
        Node::Bypass(call) => W::bypass_input(call).map(|t| reads_names(names, &t)).unwrap_or(false),
        Node::Assoc(r, args) => {
            let mut all: Vec<&Term> = vec![r];
            all.extend(args.iter().map(|(_, t)| t));
            any(&all)
        }
        Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::FnRef(_) => false,
    }
}

fn function_body(
    residual: ResidualId,
    bindings: &[(String, Value)],
    result_ty: &Ty,
    reduction: Option<(&Term, &Term)>,
    f: &Term,
) -> Res<(Params, Term, Vec<(String, Value)>)> {
    let captured_body = |f: &Term| -> Res<(Params, Term, Vec<(String, Value)>)> {
        match Residual::eval_term(residual, f, &input::at_time(0.0), None, None, None) {
            Ok(Value::Fn(function)) => match Residual::function_body(function) {
                Some((params, body)) => Ok((params, body, Residual::function_bindings(function))),
                None => Err(refusal(f, "E_PACKED_FUNCTION", "Captured function has no instantiated packed body.")),
            },
            Err(d) => Err(Stop::Refused(d)),
            Ok(_) => Err(refusal(f, "E_PACKED_FUNCTION", "Captured callable did not resolve to a function.")),
        }
    };
    match &f.node {
        Node::Fn { capture: Some(_), .. } => captured_body(f),
        Node::Fn { params, body, .. } => Ok((params.clone(), (**body).clone(), bindings.to_vec())),
        Node::RefBinding(name, fields) if fields.is_empty() => match bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v) {
            Some(Value::Fn(function)) => match Residual::function_body(*function) {
                Some((params, body)) => Ok((params, body, Residual::function_bindings(*function))),
                None => Err(refusal(f, "E_PACKED_FUNCTION", &format!("Function {} has no instantiated packed body.", name))),
            },
            _ => Err(refusal(f, "E_PACKED_FUNCTION", &format!("Function binding {} is unavailable.", name))),
        },
        Node::FnRef(name) if name == "+" && reduction.is_some() => {
            let extra = Residual::residual_ops(residual);
            match op::find(&extra, "+", &context::value()) {
                Some(declaration) if op::packed_kind(&declaration) == Some(PackedKind::Binary(Binary::Add)) => {
                    let (seed, source) = reduction.unwrap();
                    let item_ty = match &source.ty {
                        Ty::Array(ty) => (**ty).clone(),
                        _ => return Err(refusal(source, "E_PACKED_TYPE", "Packed reduce requires a numeric array.")),
                    };
                    let arg = |name: &str, ty: Ty| Term { path: None, ty, node: Node::RefBinding(name.to_owned(), Vec::new()), form: f.form.clone() };
                    let body = Term {
                        path: f.path.clone(),
                        ty: result_ty.clone(),
                        node: Node::Op {
                            op: declaration.name.clone(),
                            args: vec![("a".to_owned(), arg("@accumulator", seed.ty.clone())), ("b".to_owned(), arg("@item", item_ty))],
                            skip: Vec::new(),
                        },
                        form: f.form.clone(),
                    };
                    Ok((vec![(Pattern::Name("@accumulator".to_owned()), None), (Pattern::Name("@item".to_owned()), None)], body, bindings.to_vec()))
                }
                _ => Err(refusal(f, "E_PACKED_FUNCTION", "Packed named reduce requires canonical addition.")),
            }
        }
        Node::FnRef(name) => {
            let (params, body, _) = definition_body(f, residual, name)?;
            Ok((params, body, bindings.to_vec()))
        }
        Node::Op { op, .. } if op == "first" => captured_body(f),
        _ => Err(refusal(f, "E_PACKED_FUNCTION", "Packed kernels require a function with an instantiated body.")),
    }
}

fn record_outputs(lower: &mut Lower, e: &Expression, ty: &Ty) -> Res<Vec<Vec<i64>>> {
    match ty {
        Ty::Record(fields) => {
            let mut out = Vec::new();
            for (name, ty) in fields {
                match e.fields.iter().find(|(n, _)| n == name) {
                    Some((_, f)) => match ty {
                        Ty::Record(_) => out.extend(record_outputs(lower, f, ty)?),
                        _ => out.push(lower.as_float(f)?.registers),
                    },
                    None => return Err(Stop::Refused(diag(None, "E_PACKED_TYPE", &format!("Packed record result lacks {}.", name)))),
                }
            }
            Ok(out)
        }
        _ => Ok(Vec::new()),
    }
}

/// `shape ty next`: a fresh expression whose leaves come from `next`.
fn shape(ty: &Ty, next: &mut dyn FnMut(Ty) -> Res<i64>) -> Res<Expression> {
    match ty {
        Ty::Record(fields) => {
            let mut out = Vec::new();
            for (name, ty) in fields {
                out.push((name.clone(), shape(ty, next)?));
            }
            Ok(fields_of(out))
        }
        _ => {
            let (scalar, width) = match ty {
                Ty::Text => (Ty::Text, 1),
                Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => (Ty::Float, width_res(ty)?),
                other => (other.clone(), 1),
            };
            let mut registers = Vec::new();
            for _ in 0..width {
                registers.push(next(scalar.clone())?);
            }
            Ok(regs(registers))
        }
    }
}

type Env = Vec<(String, Expression)>;
fn env_find<'a>(env: &'a Env, name: &str) -> Option<&'a Expression> {
    env.iter().rev().find(|(n, _)| n == name).map(|(_, e)| e)
}

type CaptureKey = (String, Vec<String>, Vec<(i64, bool)>, Vec<i64>);

struct Lower {
    residual: ResidualId,
    term: std::sync::Arc<Term>,
    dynamic: bool,
    bindings: Vec<(String, Value)>,
    sources: Vec<Term>,
    code: Vec<I>,
    uniforms: Vec<Value>,
    uniform_names: Vec<(String, Vec<String>)>,
    shared: HashMap<String, i64>,
    loop_finishes: HashMap<i64, i64>,
    control_scope: Vec<i64>,
    guards: Vec<(i64, bool)>,
    integer_registers: Vec<bool>,
    text_registers: Vec<bool>,
    bottom_registers: Vec<bool>,
    array_registers: HashMap<i64, Ty>,
    array_values: HashMap<i64, Value>,
    table_uniforms: HashMap<(String, Vec<String>), i64>,
    captures: HashMap<CaptureKey, Expression>,
    capture_scope: Scope,
    call_depth: i64,
}

impl Lower {
    fn refuse(&self, code: &str, message: &str) -> Stop { refusal(&self.term, code, message) }

    fn emit(&mut self, instruction: I) -> Res<i64> {
        let control = matches!(instruction, I::Loop_begin(_) | I::Loop_carry(..) | I::Loop_end(..));
        let instruction = if self.guards.is_empty() { instruction } else { I::Guarded(self.guards.clone(), Box::new(instruction)) };
        // As in fusion, compare constant bits too: +0 and -0 remain distinct.
        // ponytail: the sharing key is the Debug text of (scope, key); a typed hash only if emit shows hot.
        let key = format!("{:?}|{:?}", self.control_scope, instruction_key(&instruction));
        if !control {
            if let Some(id) = self.shared.get(&key) { return Ok(*id); }
        }
        // ponytail: cap scratch at 512 KiB per block; measure larger bodies before raising it.
        if self.code.len() >= packed_ops::REGISTER_LIMIT {
            return Err(self.refuse("E_PACKED_LIMIT", &format!("Packed kernel exceeds {} registers.", packed_ops::REGISTER_LIMIT)));
        }
        let id = self.code.len() as i64;
        self.integer_registers.push(R::integer_register(&instruction));
        self.text_registers.push(R::text_register(&instruction));
        fn bottom(i: &I) -> bool {
            match i {
                I::Raise(Ty::Any, ..) => true,
                I::Guarded(_, body) => bottom(body),
                _ => false,
            }
        }
        self.bottom_registers.push(bottom(&instruction));
        match &instruction {
            I::Array_prepare(_, ty) => { self.array_registers.insert(id, ty.clone()); }
            I::Guarded(_, body) => {
                if let I::Array_prepare(_, ty) = body.as_ref() { self.array_registers.insert(id, ty.clone()); }
            }
            _ => {}
        }
        self.code.push(instruction);
        if !control { self.shared.insert(key, id); }
        Ok(id)
    }

    fn k(&mut self, x: f64) -> Res<i64> { self.emit(I::Const(x)) }
    fn bin(&mut self, op: Binary, a: i64, b: i64) -> Res<i64> { self.emit(I::Binary(op, a, b)) }

    fn array_type(&self, value: &Value) -> Res<Ty> {
        match value {
            Value::Residual(r) => Ok(Residual::residual_view(*r)?.term.ty.clone()),
            other => Ok(V::ty_of(other)),
        }
    }

    fn table(&mut self, at: &Term, name: (String, Vec<String>), value: &Value) -> Res<Expression> {
        let ty = self.array_type(value)?;
        match &ty {
            Ty::Array(e) if matches!(**e, Ty::Int | Ty::Text | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) => {}
            _ => return Err(refusal(at, "E_PACKED_CAPTURE", "Kernel lookup tables require integer, float or vector arrays.")),
        }
        let existing = if name.0.is_empty() { None } else { self.table_uniforms.get(&name).copied() };
        let i = match existing {
            Some(i) => i,
            None => {
                let i = self.uniforms.len() as i64;
                self.uniforms.push(value.clone());
                self.uniform_names.push(name.clone());
                if !name.0.is_empty() { self.table_uniforms.insert(name, i); }
                i
            }
        };
        let prepared = self.emit(I::Array_prepare(i, ty))?;
        self.array_values.insert(prepared, value.clone());
        Ok(one(prepared))
    }

    fn is_table(&self, e: &Expression) -> bool {
        e.registers.len() == 1 && self.array_registers.contains_key(&e.registers[0])
    }

    fn literal(&mut self, at: &Term, v: &Value) -> Res<Expression> {
        match v {
            Value::Int_array(_) | Value::Text_array(_) | Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_) => {
                self.table(at, (String::new(), Vec::new()), v)
            }
            Value::Record(fs) => {
                let mut fields = Vec::new();
                for (name, value) in fs {
                    fields.push((name.clone(), self.literal(at, value)?));
                }
                Ok(Expression { registers: Vec::new(), constant: Some(v.clone()), fields })
            }
            _ => {
                let registers = match v {
                    // Emit a constant only after its integer/float use is known.
                    Value::Int(_) => Vec::new(),
                    Value::Text(text) => vec![self.emit(I::Text_const(text.clone()))?],
                    Value::Float(_) | Value::Bool(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..) => {
                        let mut out = Vec::new();
                        for f in components(v)? { out.push(self.emit(I::Const(f))?); }
                        out
                    }
                    _ => return Err(refusal(at, "E_PACKED_TYPE", &format!("Packed constant has unsupported type {}.", ty::to_string(&V::ty_of(v))))),
                };
                Ok(Expression { registers, constant: Some(v.clone()), fields: Vec::new() })
            }
        }
    }

    fn is_text(&self, e: &Expression) -> bool { e.registers.iter().any(|r| self.text_registers[*r as usize]) }

    fn as_float(&mut self, e: &Expression) -> Res<Expression> {
        if self.is_table(e) { return Err(self.refuse("E_PACKED_TYPE", "An array handle cannot be used as a number.")); }
        if self.is_text(e) { return Err(self.refuse("E_PACKED_TYPE", "A Text leaf cannot be used as a number.")); }
        if let Some(Value::Int(n)) = &e.constant {
            let term = self.term.clone();
            return self.literal(&term, &Value::Float(*n as f64));
        }
        let mut registers = Vec::new();
        for register in &e.registers {
            if self.integer_registers[*register as usize] {
                registers.push(self.emit(I::Convert(Conversion::ToFloat, *register))?);
            } else {
                registers.push(*register);
            }
        }
        Ok(Expression { registers, ..e.clone() })
    }

    fn as_int(&mut self, e: &Expression) -> Res<Expression> {
        if self.is_table(e) { return Err(self.refuse("E_PACKED_TYPE", "An array handle cannot be used as an integer.")); }
        if self.is_text(e) { return Err(self.refuse("E_PACKED_TYPE", "A Text leaf cannot be used as an integer.")); }
        match &e.constant {
            Some(value) => {
                let value = V::coerce_to(&Ty::Int, value)?;
                match value {
                    Value::Int(n) => Ok(Expression { registers: vec![self.emit(I::Int_const(n))?], constant: Some(value), fields: Vec::new() }),
                    _ => Err(invalid("assert false")),
                }
            }
            None => {
                let mut registers = Vec::new();
                for register in &e.registers {
                    if self.integer_registers[*register as usize] {
                        registers.push(*register);
                    } else {
                        registers.push(self.emit(I::Convert(Conversion::CoerceInt, *register))?);
                    }
                }
                Ok(Expression { registers, ..e.clone() })
            }
        }
    }

    fn as_text(&mut self, e: &Expression) -> Res<Expression> {
        if e.registers.len() == 1 && self.bottom_registers[e.registers[0] as usize] {
            // The guarded Raise has already run before this unreachable value. A typed placeholder
            // keeps bottom branches out of numeric banks.
            Ok(one(self.emit(I::Text_const(Vec::new()))?))
        } else {
            if e.registers.len() != 1 || !self.text_registers[e.registers[0] as usize] {
                return Err(self.refuse("E_PACKED_TYPE", "Packed Text leaves require text values."));
            }
            Ok(e.clone())
        }
    }

    fn input(&mut self, i: usize, width: i64) -> Res<Expression> {
        let source_ty = self.sources[i].ty.clone();
        let mut registers = Vec::new();
        for j in 0..width {
            registers.push(if source_ty == array_of(Ty::Int) {
                self.emit(I::Int_input(i as i64))?
            } else if source_ty == array_of(Ty::Text) {
                self.emit(I::Text_input(i as i64))?
            } else {
                self.emit(I::Input(i as i64, width, j))?
            });
        }
        Ok(regs(registers))
    }

    fn name_text(name: &(String, Vec<String>)) -> String {
        let mut parts = vec![name.0.clone()];
        parts.extend(name.1.iter().cloned());
        parts.join(".")
    }

    fn uniform(&mut self, at: &Term, name: (String, Vec<String>), v: &Value) -> Res<Expression> {
        let ty = self.array_type(v)?;
        if let Value::List(_) = v {
            return match packed_list(v) {
                Some(packed) => {
                    let table = self.table(at, name, &packed)?;
                    Ok(Expression { constant: Some(v.clone()), ..table })
                }
                None => Err(refusal(at, "E_PACKED_CAPTURE", "Kernel list tables require a nonempty homogeneous list of int, float or vec2\u{2013}4 values.")),
            };
        }
        if matches!(ty, Ty::Array(_)) { return self.table(at, name, v); }
        let simple = matches!(v, Value::Residual(_) | Value::Source(..) | Value::Int(_) | Value::Text(_) | Value::Float(_) | Value::Bool(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..));
        let scalar = matches!(v, Value::Int(_) | Value::Text(_) | Value::Float(_) | Value::Bool(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..));
        if simple && (self.dynamic || flow_eval::is_live(v) || matches!(v, Value::Source(..))) {
            let ty = match v {
                Value::Residual(r) => Residual::residual_view(*r)?.term.ty.clone(),
                other => V::ty_of(other),
            };
            let width = if ty == Ty::Text {
                1
            } else {
                match width_opt(&ty) {
                    Some(w) => w,
                    None => return Err(refusal(at, "E_PACKED_CAPTURE", &format!("Capture {} has unsupported type {}.", Lower::name_text(&name), ty::to_string(&ty)))),
                }
            };
            let i = self.uniforms.len() as i64;
            self.uniforms.push(v.clone());
            self.uniform_names.push(name);
            let mut registers = Vec::new();
            for j in 0..width {
                registers.push(if ty == Ty::Int {
                    self.emit(I::Int_uniform(i))?
                } else if ty == Ty::Text {
                    self.emit(I::Text_uniform(i))?
                } else {
                    self.emit(I::Uniform(i, j))?
                });
            }
            Ok(regs(registers))
        } else if scalar {
            self.literal(at, v)
        } else {
            Err(refusal(at, "E_PACKED_CAPTURE", &format!("Capture {} has unsupported type {}.", Lower::name_text(&name), ty::to_string(&V::ty_of(v)))))
        }
    }

    fn capture_path(&self, name: &str, fields: &[String]) -> (String, Vec<String>) {
        match &self.capture_scope {
            None => (name.to_owned(), fields.to_vec()),
            Some((root, path)) => {
                let mut p = path.clone();
                p.push(name.to_owned());
                p.extend(fields.iter().cloned());
                (root.clone(), p)
            }
        }
    }

    fn capture(&mut self, at: &Term, name: &str, fields: &[String]) -> Res<Expression> {
        let key: CaptureKey = (name.to_owned(), fields.to_vec(), self.guards.clone(), self.control_scope.clone());
        if let Some(e) = self.captures.get(&key) { return Ok(e.clone()); }
        let value = match captured_value(&self.bindings, name, fields) {
            Ok(v) => v,
            Err(Stop::Unsupported) => {
                return Err(refusal(at, "E_PACKED_CAPTURE", &format!("Captured binding or field {} is unavailable.", {
                    let mut p = vec![name.to_owned()];
                    p.extend(fields.iter().cloned());
                    p.join(".")
                })))
            }
            Err(other) => return Err(other),
        };
        let e = match &value {
            Value::Record(fs) => {
                let mut out = Vec::new();
                for (key, _) in fs {
                    let mut f = fields.to_vec();
                    f.push(key.clone());
                    out.push((key.clone(), self.capture(at, name, &f)?));
                }
                fields_of(out)
            }
            Value::Residual(r) => {
                // a live record (a matrix chosen per frame) is a uniform per field
                match Residual::residual_view(*r)?.term.ty.clone() {
                    Ty::Record(fs) => {
                        let mut out = Vec::new();
                        for (key, _) in &fs {
                            let mut f = fields.to_vec();
                            f.push(key.clone());
                            out.push((key.clone(), self.capture(at, name, &f)?));
                        }
                        fields_of(out)
                    }
                    _ => self.uniform(at, (name.to_owned(), fields.to_vec()), &value)?,
                }
            }
            _ => self.uniform(at, (name.to_owned(), fields.to_vec()), &value)?,
        };
        self.captures.insert(key, e.clone());
        Ok(e)
    }

    fn captured(&mut self, at: &Term, name: &str, fields: &[String]) -> Res<Expression> {
        let (name, fields) = self.capture_path(name, fields);
        self.capture(at, &name, &fields)
    }

    fn invariant_value(&mut self, at: &Term) -> Res<Expression> {
        let bindings = match &self.capture_scope {
            None => self.bindings.clone(),
            Some((name, fields)) => match captured_value(&self.bindings, name, fields)? {
                Value::Fn(function) => Residual::function_bindings(function),
                _ => return Err(Stop::Unsupported),
            },
        };
        let bound = Residual::with_bindings(self.residual, at, &bindings)?;
        self.uniform(at, (String::new(), Vec::new()), &Value::Residual(bound))
    }

    fn component(&self, e: &Expression, i: usize) -> Res<i64> {
        reg(e, if e.registers.len() == 1 { 0 } else { i })
    }

    fn with_guard(&mut self, condition: i64, selected: bool, f: &mut dyn FnMut(&mut Lower) -> Res<Expression>) -> Res<Expression> {
        let previous = self.guards.clone();
        self.guards.push((condition, selected));
        let result = f(self);
        self.guards = previous;
        result
    }

    /// `under condition selected instruction`: emit one instruction under one more guard.
    fn under(&mut self, condition: i64, selected: bool, instruction: I) -> Res<i64> {
        let previous = self.guards.clone();
        self.guards.push((condition, selected));
        let result = self.emit(instruction);
        self.guards = previous;
        result
    }

    fn coerce(&mut self, at: &Term, ty: &Ty, value: &Expression) -> Res<Expression> {
        let table_ty = if self.is_table(value) { self.array_registers.get(&value.registers[0]).cloned() } else { None };
        if let Ty::List(element) = ty {
            if let Some(t) = &table_ty {
                if **element == Ty::Any || Some((**element).clone()) == ty::elem(t) { return Ok(value.clone()); }
            }
        }
        if let Ty::Array(_) = ty {
            if table_ty.as_ref() == Some(ty) { return Ok(value.clone()); }
        }
        match (ty, &value.constant) {
            (Ty::Int, Some(_)) => self.as_int(value),
            (_, Some(constant)) => {
                let coerced = V::coerce_to(ty, constant)?;
                self.literal(at, &coerced)
            }
            (Ty::Record(fields), _) => {
                let mut out = Vec::new();
                for (name, ty) in fields {
                    let f = field(at, value, name)?;
                    out.push((name.clone(), self.coerce(at, ty, &f)?));
                }
                Ok(Expression { fields: out, ..value.clone() })
            }
            (Ty::Int, _) => self.as_int(value),
            (Ty::Text, _) => self.as_text(value),
            (Ty::Float, _) => self.as_float(value),
            (Ty::Bool, _) => {
                let value = self.as_float(value)?;
                if value.registers.len() != 1 { return Err(refusal(at, "E_PACKED_TYPE", "Packed boolean argument requires a scalar.")); }
                let inner = self.emit(I::Unary(Unary::Not, value.registers[0]))?;
                let outer = self.emit(I::Unary(Unary::Not, inner))?;
                Ok(Expression { registers: vec![outer], ..value })
            }
            (Ty::Vec2 | Ty::Vec3 | Ty::Vec4, _) => {
                let value = self.as_float(value)?;
                let width = width_res(ty)? as usize;
                if value.registers.len() == width {
                    Ok(value)
                } else if value.registers.len() == 1 {
                    Ok(Expression { registers: vec![value.registers[0]; width], ..value })
                } else {
                    Err(refusal(at, "E_PACKED_TYPE", "Packed function argument has the wrong vector width."))
                }
            }
            _ => Err(refusal(at, "E_PACKED_TYPE", &format!("Packed function argument has unsupported type {}.", ty::to_string(ty)))),
        }
    }

    fn leaves(&mut self, at: &Term, ty: &Ty, value: &Expression) -> Res<Vec<(i64, Ty)>> {
        match ty {
            Ty::Record(fields) => {
                let mut out = Vec::new();
                for (name, ty) in fields {
                    let f = field(at, value, name)?;
                    out.extend(self.leaves(at, ty, &f)?);
                }
                Ok(out)
            }
            Ty::Int | Ty::Text | Ty::Float | Ty::Bool | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => {
                let value = self.coerce(at, ty, value)?;
                let scalar = match ty { Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => Ty::Float, other => other.clone() };
                Ok(value.registers.iter().map(|r| (*r, scalar.clone())).collect())
            }
            _ => Err(refusal(at, "E_PACKED_TYPE", "Carried loop values require scalar, vector or record leaves.")),
        }
    }

    fn carried_initial(&mut self, seed: &Term, ty: &Ty, value: &Expression) -> Res<Vec<i64>> {
        // ponytail: mixed carries stay in Core until Region retains their initial and later numeric kinds.
        if !same_carry_type(&seed.ty, ty) {
            return Err(refusal(seed, "E_PACKED_TYPE", "Carried numeric widening requires the structured Core representation."));
        }
        Ok(self.leaves(seed, ty, value)?.into_iter().map(|(r, _)| r).collect())
    }

    fn bind_parameters(&mut self, at: &Term, params: &Params, values: &[Expression], env: &Env) -> Res<Env> {
        if params.len() != values.len() {
            return Err(refusal(at, "E_PACKED_FUNCTION", "Packed helper arguments do not match its checked parameters."));
        }
        let mut env = env.clone();
        for ((pattern, ty), value) in params.iter().zip(values.iter()) {
            match pattern {
                Pattern::Name(name) => {
                    let bound = match ty { Some(ty) => self.coerce(at, ty, value)?, None => value.clone() };
                    env.push((name.clone(), bound));
                }
                _ => return Err(refusal(at, "E_PACKED_FORM", "Packed helper parameters require name patterns.")),
            }
        }
        Ok(env)
    }

    fn carried_loop(
        &mut self,
        ty: &Ty,
        control: Loop,
        step: &mut dyn FnMut(&mut Lower, i64, &Expression) -> Res<(Vec<i64>, Option<i64>)>,
    ) -> Res<Expression> {
        if control.initial.is_empty() {
            return Err(Stop::Refused(diag(Some(control.span.clone()), "E_PACKED_TYPE", "A carried loop requires at least one scalar leaf.")));
        }
        let header = self.emit(I::Loop_begin(control))?;
        let previous = self.control_scope.clone();
        self.control_scope.insert(0, header);
        // `scope := header :: previous`: the head is the most recent; keep the OCaml order.
        let body = (|| -> Res<()> {
            let mut component = 0;
            let carried = {
                let me: &mut Lower = self;
                shape(ty, &mut |ty| {
                    let index = component;
                    component += 1;
                    me.emit(I::Loop_carry(header, index, ty))
                })?
            };
            let (next, stop) = step(self, header, &carried)?;
            let finish = self.emit(I::Loop_end(header, next, stop))?;
            self.loop_finishes.insert(header, finish);
            Ok(())
        })();
        self.control_scope = previous;
        body?;
        let mut component = 0;
        let me: &mut Lower = self;
        shape(ty, &mut |ty| {
            let index = component;
            component += 1;
            me.emit(I::Loop_result(header, index, ty))
        })
    }

    fn hsl(&mut self, env: &Env, args: &[(String, Term)]) -> Res<Expression> {
        // Flow.Op.hsl_channels, step for step, with a select per hue sector. Emission order is
        // OCaml's right-to-left argument order.
        let arg = |me: &mut Lower, name: &str| -> Res<i64> {
            let term = args.iter().find(|(n, _)| n == name).map(|(_, t)| t).ok_or_else(|| invalid("Not_found"))?;
            let e = me.expression(env, term)?;
            let f = me.as_float(&e)?;
            reg(&f, 0)
        };
        let h = arg(self, "h")?;
        let s = arg(self, "s")?;
        let l = arg(self, "l")?;
        // unit v = bin Max (k 0.) (bin Min (k 1.) v)
        fn unit(me: &mut Lower, v: i64) -> Res<i64> {
            let c1 = me.k(1.0)?;
            let lo = me.bin(Binary::Min, c1, v)?;
            let c0 = me.k(0.0)?;
            me.bin(Binary::Max, c0, lo)
        }
        // hue = bin Div (bin Mod h (k 360.)) (k 60.)
        let c60 = self.k(60.0)?;
        let c360 = self.k(360.0)?;
        let modulo = self.bin(Binary::Mod, h, c360)?;
        let hue = self.bin(Binary::Div, modulo, c60)?;
        let s = unit(self, s)?;
        let l = unit(self, l)?;
        // chroma = bin Mul (bin Sub (k 1.) (abs (bin Sub (bin Mul (k 2.) l) (k 1.)))) s
        let c1 = self.k(1.0)?;
        let c2 = self.k(2.0)?;
        let twice = self.bin(Binary::Mul, c2, l)?;
        let diff = self.bin(Binary::Sub, twice, c1)?;
        let abs1 = self.emit(I::Unary(Unary::Abs, diff))?;
        let c1b = self.k(1.0)?;
        let rest = self.bin(Binary::Sub, c1b, abs1)?;
        let chroma = self.bin(Binary::Mul, rest, s)?;
        // x = bin Mul chroma (bin Sub (k 1.) (abs (bin Sub (bin Mod hue (k 2.)) (k 1.))))
        let c1c = self.k(1.0)?;
        let c2b = self.k(2.0)?;
        let hm = self.bin(Binary::Mod, hue, c2b)?;
        let hd = self.bin(Binary::Sub, hm, c1c)?;
        let abs2 = self.emit(I::Unary(Unary::Abs, hd))?;
        let c1d = self.k(1.0)?;
        let inv = self.bin(Binary::Sub, c1d, abs2)?;
        let x = self.bin(Binary::Mul, chroma, inv)?;
        let zero = self.k(0.0)?;
        let sector = |me: &mut Lower, c: [i64; 6]| -> Res<i64> {
            // pick limit yes no = emit (Select (bin Lt hue (k limit), yes, no)), nested to the right.
            fn pick(me: &mut Lower, hue: i64, limit: f64, yes: i64, no: i64) -> Res<i64> {
                let limit_register = me.k(limit)?;
                let condition = me.bin(Binary::Lt, hue, limit_register)?;
                me.emit(I::Select(condition, yes, no))
            }
            let p5 = pick(me, hue, 5.0, c[4], c[5])?;
            let p4 = pick(me, hue, 4.0, c[3], p5)?;
            let p3 = pick(me, hue, 3.0, c[2], p4)?;
            let p2 = pick(me, hue, 2.0, c[1], p3)?;
            pick(me, hue, 1.0, c[0], p2)
        };
        // m = bin Sub l (bin Div chroma (k 2.))
        let c2c = self.k(2.0)?;
        let half = self.bin(Binary::Div, chroma, c2c)?;
        let m = self.bin(Binary::Sub, l, half)?;
        let byte = |me: &mut Lower, v: i64| -> Res<i64> {
            // bin Div (float (int (bin Add (bin Mul (unit (bin Add v m)) (k 255.)) (k 0.5)))) (k 255.)
            let c255 = me.k(255.0)?;
            let c05 = me.k(0.5)?;
            let c255b = me.k(255.0)?;
            let sum = me.bin(Binary::Add, v, m)?;
            let clamped = unit(me, sum)?;
            let scaled = me.bin(Binary::Mul, clamped, c255b)?;
            let rounded = me.bin(Binary::Add, scaled, c05)?;
            let truncated = me.emit(I::Convert(Conversion::Truncate, rounded))?;
            let as_float = me.emit(I::Convert(Conversion::ToFloat, truncated))?;
            me.bin(Binary::Div, as_float, c255)
        };
        // An array literal evaluates its elements right to left.
        let s2 = sector(self, [zero, zero, x, chroma, chroma, x])?;
        let b2 = byte(self, s2)?;
        let s1 = sector(self, [x, chroma, chroma, x, zero, zero])?;
        let b1 = byte(self, s1)?;
        let s0 = sector(self, [chroma, x, zero, zero, x, chroma])?;
        let b0 = byte(self, s0)?;
        Ok(regs(vec![b0, b1, b2]))
    }

    #[allow(clippy::too_many_lines)]
    fn operation(&mut self, env: &Env, t: &Term, op_name: &str, args: &[(String, Term)]) -> Res<Expression> {
        let extra = Residual::residual_ops(self.residual);
        let declaration = match op::find(&extra, op_name, &context::value()) {
            Some(o) => match op::packed_kind(&o) {
                Some(kind) => (o, kind),
                None => {
                    return Err(refusal(t, "E_PACKED_OPERATOR", &format!("Operator {} is not a canonical scalar declaration or validated packed intrinsic.", op_name)));
                }
            },
            None => return Err(refusal(t, "E_PACKED_OPERATOR", &format!("Operator {} is unavailable.", op_name))),
        };
        let (declaration, kind) = declaration;
        let mut expressions = Vec::new();
        for (_, a) in args {
            expressions.push(self.expression(env, a)?);
        }
        if expressions.iter().all(|e| e.constant.is_some()) && !declaration.live {
            let folded_args: Vec<(String, Value)> = args.iter().zip(expressions.iter()).map(|((name, _), e)| (name.clone(), e.constant.clone().unwrap())).collect();
            let refused: RefCell<Option<Diagnostic>> = RefCell::new(None);
            let node = |name: &str, _: &op::Args| -> Result<Value, Diagnostic> {
                let d = diag(Some(t.form.span.clone()), "E_PACKED_CONSTANT", &format!("Constant operator {} attempted to create node {}.", op_name, name));
                *refused.borrow_mut() = Some(d.clone());
                Err(d)
            };
            let result = (declaration.body)(&input::at_time(0.0), &node, &folded_args);
            if let Some(d) = refused.into_inner() { return Err(Stop::Refused(d)); }
            let value = result?;
            return self.literal(t, &value);
        }
        if declaration.live && args.is_empty() && matches!(t.ty, Ty::Float | Ty::Int | Ty::Bool) {
            let register = self.emit(if t.ty == Ty::Int { I::Int_frame(declaration.name.clone()) } else { I::Frame(declaration.name.clone()) })?;
            return Ok(one(register));
        }
        match kind {
            PackedKind::Gather => self.gather(t, op_name, &expressions),
            PackedKind::Trace => {
                let mut registers = Vec::new();
                for (i, e) in expressions.iter().enumerate() {
                    let converted = if i == 0 { self.as_int(e)? } else { self.as_float(e)? };
                    registers.extend(converted.registers);
                }
                if registers.len() != 10 {
                    return Err(refusal(t, "E_PACKED_TYPE", "scene/trace needs an integer scene and scalar or vec3 arguments."));
                }
                let f0 = one(self.emit(I::Trace(0, registers.clone()))?);
                let f1 = self.emit(I::Trace(1, registers.clone()))?;
                let prim = one(self.emit(I::Convert(Conversion::Truncate, f1))?);
                let u = one(self.emit(I::Trace(2, registers.clone()))?);
                let v = one(self.emit(I::Trace(3, registers.clone()))?);
                // record fields and array elements evaluate right to left in OCaml
                Ok(self.trace_record(&registers, f0, prim, u, v)?)
            }
            PackedKind::Stage => self.stage(t, op_name, args, &expressions),
            PackedKind::CheckedMul | PackedKind::CheckedFma => {
                // ponytail: one destination per instruction computes both projections
                // separately; co-compute pairs only if duplicate arithmetic measures hot.
                let mut arguments = Vec::new();
                for e in &expressions { arguments.push(self.as_float(e)?); }
                let instruction = |me: &mut Lower, projection: Projection| -> Res<Expression> {
                    let ins = match (&kind, arguments.as_slice()) {
                        (PackedKind::CheckedMul, [a, b]) => I::Checked_mul(projection, reg(a, 0)?, reg(b, 0)?),
                        (PackedKind::CheckedFma, [a, b, c]) => I::Checked_fma(projection, reg(a, 0)?, reg(b, 0)?, reg(c, 0)?),
                        _ => return Err(refusal(t, "E_PACKED_OPERATOR", "Checked arithmetic needs scalar arguments.")),
                    };
                    Ok(one(me.emit(ins)?))
                };
                // `[ ("value", field Value); ("valid", field Valid) ]`: right to left
                let valid = instruction(self, Projection::Valid)?;
                let value = instruction(self, Projection::Value)?;
                Ok(fields_of(vec![("value".to_owned(), value), ("valid".to_owned(), valid)]))
            }
            _ => self.arithmetic(t, op_name, &declaration, &kind, &expressions, args),
        }
    }

    fn trace_record(&mut self, registers: &[i64], f0: Expression, prim: Expression, u: Expression, v: Expression) -> Res<Expression> {
        // The list literal evaluates last to first: instance, hit, normal, v, u, prim, t. The
        // callers above built t, prim, u, v in field order, so rebuild in OCaml's order here.
        let _ = (f0, prim, u, v);
        let at = |me: &mut Lower, k: i64| -> Res<Expression> { Ok(one(me.emit(I::Trace(k, registers.to_vec()))?)) };
        let conv = |me: &mut Lower, k: i64| -> Res<Expression> {
            let f = me.emit(I::Trace(k, registers.to_vec()))?;
            Ok(one(me.emit(I::Convert(Conversion::Truncate, f))?))
        };
        let instance = conv(self, 8)?;
        let hit = at(self, 7)?;
        let n4 = self.emit(I::Trace(4, registers.to_vec()))?;
        let n5 = self.emit(I::Trace(5, registers.to_vec()))?;
        let n6 = self.emit(I::Trace(6, registers.to_vec()))?;
        let normal = regs(vec![n4, n5, n6]);
        let v = at(self, 3)?;
        let u = at(self, 2)?;
        let prim = conv(self, 1)?;
        let t0 = at(self, 0)?;
        Ok(fields_of(vec![
            ("t".to_owned(), t0),
            ("prim".to_owned(), prim),
            ("u".to_owned(), u),
            ("v".to_owned(), v),
            ("normal".to_owned(), normal),
            ("hit".to_owned(), hit),
            ("instance".to_owned(), instance),
        ]))
    }

    fn gather(&mut self, t: &Term, op_name: &str, expressions: &[Expression]) -> Res<Expression> {
        let (array, index) = match expressions {
            [array, index] if self.is_table(array) => (array.clone(), index.clone()),
            _ => return Err(refusal(t, "E_PACKED_CAPTURE", "Array lookup requires an invariant numeric table.")),
        };
        let (index, valid) = if op_name != "nth" {
            (self.as_int(&index)?, None)
        } else {
            let count = match &array.constant {
                Some(Value::List(items)) => items.len() as i64,
                _ => return Err(refusal(t, "E_PACKED_CAPTURE", "nth requires an immutable numeric list table.")),
            };
            let is_integer = match &index.constant {
                Some(Value::Int(_)) => true,
                _ => index.registers.len() == 1 && self.integer_registers[index.registers[0] as usize],
            };
            let (converted, valid) = if is_integer {
                let converted = self.as_int(&index)?;
                let r = reg(&converted, 0)?;
                let zero = self.emit(I::Int_const(0))?;
                let lo = self.emit(I::Int_compare(Comparison::Ige, r, zero))?;
                let bound = self.emit(I::Int_const(count))?;
                let hi = self.emit(I::Int_compare(Comparison::Ilt, r, bound))?;
                // Binary (And, lo, hi) after both operands: matches OCaml's let-bound order.
                let valid = self.emit(I::Binary(Binary::And, lo, hi))?;
                (converted, valid)
            } else {
                let index = self.as_float(&index)?;
                if index.registers.len() != 1 { return Err(refusal(t, "E_PACKED_TYPE", "List indices require a scalar value.")); }
                let r = index.registers[0];
                let zero = self.emit(I::Const(0.0))?;
                let lo = self.emit(I::Binary(Binary::Ge, r, zero))?;
                // hi = Binary (Lt, r, emit (Const (float count)))
                let limit = self.emit(I::Const(count as f64))?;
                let hi = self.emit(I::Binary(Binary::Lt, r, limit))?;
                // whole = Binary (Eq, emit (Binary (Mod, r, emit (Const 1.))), zero)
                let one_c = self.emit(I::Const(1.0))?;
                let modulo = self.emit(I::Binary(Binary::Mod, r, one_c))?;
                let whole = self.emit(I::Binary(Binary::Eq, modulo, zero))?;
                let both = self.emit(I::Binary(Binary::And, lo, hi))?;
                let valid = self.emit(I::Binary(Binary::And, both, whole))?;
                let truncated = self.under(valid, true, I::Convert(Conversion::Truncate, r))?;
                (one(truncated), valid)
            };
            let failure = self.under(valid, false, I::Raise(Ty::Int, "E_LIST_RANGE".to_owned(), "nth index is out of range or is not a whole number.".to_owned(), None))?;
            let selected = self.emit(I::Int_select(valid, reg(&converted, 0)?, failure))?;
            (Expression { registers: vec![selected], ..converted }, Some(valid))
        };
        if index.registers.len() != 1 { return Err(refusal(t, "E_PACKED_TYPE", "Array indices require a scalar integer.")); }
        let prepared = reg(&array, 0)?;
        let index = index.registers[0];
        let table_ty = self.array_registers.get(&prepared).cloned().ok_or_else(|| invalid("Not_found"))?;
        let element = ty::elem(&table_ty).ok_or_else(|| invalid("option is None"))?;
        if t.ty != Ty::Any && t.ty != element {
            return Err(refusal(t, "E_PACKED_TYPE", &format!("{} has checked type {}, but its table contains {}.", lisp::flat(&t.form), ty::to_string(&t.ty), ty::to_string(&element))));
        }
        let element = if t.ty == Ty::Any { element } else { t.ty.clone() };
        let gather = |me: &mut Lower, instruction: I| -> Res<i64> {
            match valid { None => me.emit(instruction), Some(valid) => me.under(valid, true, instruction) }
        };
        let registers = if element == Ty::Int {
            vec![gather(self, I::Int_gather(prepared, index))?]
        } else if element == Ty::Text {
            vec![gather(self, I::Text_gather(prepared, index))?]
        } else {
            let width = width_res(&element)?;
            let mut out = Vec::new();
            for component in 0..width {
                out.push(gather(self, I::Gather(prepared, index, width, component))?);
            }
            out
        };
        Ok(regs(registers))
    }

    fn stage(&mut self, t: &Term, op_name: &str, args: &[(String, Term)], expressions: &[Expression]) -> Res<Expression> {
        let mut operands: Vec<i64> = Vec::new();
        for ((name, a), e) in args.iter().zip(expressions.iter()) {
            if name == "texture" { continue; }
            match &a.ty {
                Ty::Array(_) => operands.extend(e.registers.iter().copied()),
                Ty::Int => operands.extend(self.as_int(e)?.registers),
                _ => operands.extend(self.as_float(e)?.registers),
            }
        }
        let texture = || -> Res<i64> {
            match args.iter().find(|(n, _)| n == "texture") {
                Some((_, Term { node: Node::Lit(param::Value::Int(n)), .. })) => Ok(*n),
                _ => Err(refusal(t, "E_PACKED_TYPE", &format!("{} names its texture slot with an integer literal.", op_name))),
            }
        };
        let stage = |me: &mut Lower, s: Stage| -> Res<Expression> { Ok(one(me.emit(I::Stage(s, operands.clone()))?)) };
        match op_name {
            "gpu/fwidth" => stage(self, Stage::Fwidth),
            "gpu/float-bits" => stage(self, Stage::Float_bits),
            "gpu/trace-any" => stage(self, Stage::Trace_any),
            "gpu/trace-curves-any" => stage(self, Stage::Trace_curves_any),
            "gpu/trace-triangles" | "gpu/trace-curves" => {
                let curves = op_name == "gpu/trace-curves";
                let names: &[&str] = if curves { &["t", "prim", "u", "hit"] } else { &["t", "prim", "u", "v", "hit", "instance"] };
                // List.mapi, left to right
                let mut fields = Vec::new();
                for (k, name) in names.iter().enumerate() {
                    let s = if curves { Stage::Trace_curves(k as i64) } else { Stage::Trace_triangles(k as i64) };
                    fields.push(((*name).to_owned(), stage(self, s)?));
                }
                Ok(fields_of(fields))
            }
            "gpu/scene-vertex" | "gpu/scene-vertex-end" => {
                let end = op_name == "gpu/scene-vertex-end";
                let mut registers = Vec::new();
                for c in 0..3 { registers.push(self.emit(I::Stage(Stage::Scene_vertex(end, c), operands.clone()))?); }
                Ok(regs(registers))
            }
            "gpu/scene-instance" => stage(self, Stage::Scene_instance),
            "gpu/group-scan" => {
                // [ ("before", stage (Group_scan 0)); ("total", stage (Group_scan 1)) ]: right to left
                let total = stage(self, Stage::Group_scan(1))?;
                let before = stage(self, Stage::Group_scan(0))?;
                Ok(fields_of(vec![("before".to_owned(), before), ("total".to_owned(), total)]))
            }
            "gpu/store" | "gpu/store-int" => stage(self, Stage::Store),
            "gpu/scene-sphere" | "gpu/scene-strand" => {
                let sphere = op_name == "gpu/scene-sphere";
                let mut registers = Vec::new();
                for c in 0..4 {
                    registers.push(self.emit(I::Stage(if sphere { Stage::Scene_sphere(c) } else { Stage::Scene_strand(c) }, operands.clone()))?);
                }
                Ok(regs(registers))
            }
            "gpu/sample" | "gpu/sample-level" | "gpu/read" => {
                let tex = texture()?;
                let mut registers = Vec::new();
                for c in 0..4 {
                    let s = match op_name { "gpu/sample" => Stage::Sample(tex, c), "gpu/read" => Stage::Read(tex, c), _ => Stage::Sample_level(tex, c) };
                    registers.push(self.emit(I::Stage(s, operands.clone()))?);
                }
                Ok(regs(registers))
            }
            _ => {
                let tex = texture()?;
                let mut registers = Vec::new();
                for c in 0..2 { registers.push(self.emit(I::Stage(Stage::Texture_size(tex, c), operands.clone()))?); }
                Ok(regs(registers))
            }
        }
    }

    fn arithmetic(&mut self, t: &Term, op_name: &str, declaration: &op::Op, kind: &PackedKind, expressions: &[Expression], args: &[(String, Term)]) -> Res<Expression> {
        let width = expressions.iter().fold(1, |n, e| n.max(e.registers.len()));
        let registers: Vec<i64> = match (kind, expressions) {
            (PackedKind::Integer(operation), [a, b]) => {
                let a = self.as_int(a)?;
                let b = self.as_int(b)?;
                vec![self.emit(I::Int_binary(*operation, reg(&a, 0)?, reg(&b, 0)?))?]
            }
            (PackedKind::IntegerCompare(operation), [a, b]) => {
                let a = self.as_int(a)?;
                let b = self.as_int(b)?;
                vec![self.emit(I::Int_compare(*operation, reg(&a, 0)?, reg(&b, 0)?))?]
            }
            (PackedKind::RandAt, [a, b]) => {
                let a = self.as_int(a)?;
                let b = self.as_int(b)?;
                vec![self.emit(I::Rand_at(reg(&a, 0)?, reg(&b, 0)?))?]
            }
            (PackedKind::Fma, [a, b, c]) => {
                let a = self.as_float(a)?;
                let b = self.as_float(b)?;
                let c = self.as_float(c)?;
                vec![self.emit(I::Fma(reg(&a, 0)?, reg(&b, 0)?, reg(&c, 0)?))?]
            }
            (PackedKind::Convert(conversion), [a]) => vec![self.emit(I::Convert(conversion.clone(), reg(a, 0)?))?],
            (PackedKind::Noise3, [a, rest @ ..]) if a.registers.len() == 3 => {
                // a live octave count is an Int register, a live lacunarity or gain a float register
                // (uniforms: the kernel stays one compiled program), checked where the noise runs; the
                // seed picks a table, so it is constant
                let mut live: Vec<(String, i64)> = Vec::new();
                let mut configuration: Vec<(String, Value)> = Vec::new();
                for ((name, _), expression) in args.iter().skip(1).zip(rest.iter()) {
                    let value = match &expression.constant {
                        Some(value) => value.clone(),
                        None if name == "octaves" => {
                            let r = reg(&self.as_int(expression)?, 0)?;
                            live.push((name.clone(), r));
                            Value::Int(1)
                        }
                        None if name == "lacunarity" || name == "gain" => {
                            let r = reg(&self.as_float(expression)?, 0)?;
                            live.push((name.clone(), r));
                            Value::Float(if name == "lacunarity" { 2.0 } else { 0.5 })
                        }
                        None => return Err(refusal(t, "E_PACKED_CONSTANT", &format!("Noise configuration {} must be constant.", name))),
                    };
                    configuration.push((name.clone(), value));
                }
                let (seed, octaves, lacunarity, gain) = match op::noise_settings(&configuration) {
                    Ok(settings) => settings,
                    Err(d) => return Err(refusal(t, "E_PACKED_CONSTANT", &d.message)),
                };
                let find = |name: &str| live.iter().rev().find(|(n, _)| n == name).map(|(_, r)| *r);
                // `live` was consed (latest first): the first match in OCaml's list is the latest push.
                let lacunarity = match find("lacunarity") { Some(r) => LiveFloat::Live(r), None => LiveFloat::Fixed(lacunarity) };
                let gain = match find("gain") { Some(r) => LiveFloat::Live(r), None => LiveFloat::Fixed(gain) };
                if !packed_ops::supported_noise_octaves(octaves) {
                    return Err(refusal(t, "E_PACKED_CONSTANT", &format!("Packed noise octaves must be 1 to 64, got {}.", octaves)));
                }
                let octaves = match find("octaves") { Some(r) => LiveInt::Live(r), None => LiveInt::Fixed(octaves) };
                vec![self.emit(I::Noise3(reg(a, 0)?, reg(a, 1)?, reg(a, 2)?, seed, octaves, lacunarity, gain))?]
            }
            (PackedKind::Exact, [a]) => a.registers.clone(),
            (PackedKind::Length, [a]) if a.registers.len() == 3 => {
                let xx = self.square(a, 0)?;
                let yy = self.square(a, 1)?;
                let zz = self.square(a, 2)?;
                let xy = self.emit(I::Binary(Binary::Add, xx, yy))?;
                let sum = self.emit(I::Binary(Binary::Add, xy, zz))?;
                vec![self.emit(I::Unary(Unary::Sqrt, sum))?]
            }
            (PackedKind::Dot, [a, b]) if a.registers.len() == 3 && b.registers.len() == 3 => {
                let x = self.product(a, b, 0)?;
                let y = self.product(a, b, 1)?;
                let z = self.product(a, b, 2)?;
                let xy = self.emit(I::Binary(Binary::Add, x, y))?;
                vec![self.emit(I::Binary(Binary::Add, xy, z))?]
            }
            (PackedKind::Cross, [a, b]) if a.registers.len() == 3 && b.registers.len() == 3 => {
                let negative = self.emit(I::Const(-1.0))?;
                let component = |me: &mut Lower, i: usize, j: usize| -> Res<i64> {
                    // let p = ... and q = ...: sequential
                    let p = me.emit(I::Binary(Binary::Mul, a.registers[i], b.registers[j]))?;
                    let q = me.emit(I::Binary(Binary::Mul, negative, a.registers[j]))?;
                    me.emit(I::Fma(q, b.registers[i], p))
                };
                // `[| component 1 2; component 2 0; component 0 1 |]`: right to left
                let c2 = component(self, 0, 1)?;
                let c1 = component(self, 2, 0)?;
                let c0 = component(self, 1, 2)?;
                vec![c0, c1, c2]
            }
            (PackedKind::Normalize, [a]) if a.registers.len() == 3 => {
                let x = self.square(a, 0)?;
                let y = self.square(a, 1)?;
                let z = self.square(a, 2)?;
                let xy = self.emit(I::Binary(Binary::Add, x, y))?;
                let sum = self.emit(I::Binary(Binary::Add, xy, z))?;
                let norm = self.emit(I::Unary(Unary::Sqrt, sum))?;
                let zero = self.emit(I::Const(0.0))?;
                let empty = self.emit(I::Binary(Binary::Eq, norm, zero))?;
                let previous = self.guards.clone();
                self.guards.push((empty, false));
                let scaled = (|| -> Res<Vec<i64>> {
                    let c1 = self.emit(I::Const(1.0))?;
                    let inverse = self.emit(I::Binary(Binary::Div, c1, norm))?;
                    let mut out = Vec::new();
                    for r in &a.registers { out.push(self.emit(I::Binary(Binary::Mul, *r, inverse))?); }
                    Ok(out)
                })();
                self.guards = previous;
                let scaled = scaled?;
                let mut out = Vec::new();
                for r in scaled { out.push(self.emit(I::Select(empty, zero, r))?); }
                out
            }
            (PackedKind::Binary(operation), [a, b]) => {
                let a = self.as_float(a)?;
                let b = self.as_float(b)?;
                let mut out = Vec::new();
                for i in 0..width {
                    let register = self.emit(I::Binary(*operation, self.component(&a, i)?, self.component(&b, i)?))?;
                    out.push(if t.ty == Ty::Int { self.emit(I::Convert(Conversion::IntegerResult(declaration.name.clone()), register))? } else { register });
                }
                out
            }
            (PackedKind::Unary(Unary::Abs), [a]) if t.ty == Ty::Int => vec![self.emit(I::Int_abs(reg(a, 0)?))?],
            (PackedKind::Unary(operation), [a]) if width == 1 => {
                let a = self.as_float(a)?;
                vec![self.emit(I::Unary(*operation, reg(&a, 0)?))?]
            }
            _ => {
                return Err(refusal(t, "E_PACKED_OPERATOR", &format!(
                    "Dynamic operator {} has no packed instruction for {} arguments at width {}.", op_name, expressions.len(), width)));
            }
        };
        Ok(regs(registers))
    }

    fn square(&mut self, a: &Expression, i: usize) -> Res<i64> {
        self.emit(I::Binary(Binary::Mul, reg(a, i)?, reg(a, i)?))
    }
    fn product(&mut self, a: &Expression, b: &Expression, i: usize) -> Res<i64> {
        self.emit(I::Binary(Binary::Mul, reg(a, i)?, reg(b, i)?))
    }

    fn expression(&mut self, env: &Env, t: &Term) -> Res<Expression> {
        match self.expression_node(env, t) {
            Err(Stop::Unsupported) => Err(self.term_unsupported(t)),
            Err(Stop::Error(d)) => {
                if !self.guards.is_empty() || !self.control_scope.is_empty() {
                    let register = self.emit(I::Raise(t.ty.clone(), d.code.clone(), d.message.clone(), d.cause.clone()))?;
                    Ok(one(register))
                } else {
                    let span = d.span.clone().or_else(|| Some(t.form.span.clone()));
                    Err(Stop::Error(Diagnostic { span, ..d }))
                }
            }
            other => other,
        }
    }

    fn term_unsupported(&self, t: &Term) -> Stop {
        refusal(t, "E_PACKED_FORM", &format!("Packed term {} has unsupported type {}.", lisp::flat(&t.form), ty::to_string(&t.ty)))
    }

    fn expression_node(&mut self, env: &Env, t: &Term) -> Res<Expression> {
        match &t.node {
            Node::Lit(param::Value::Int(n)) => self.literal(t, &Value::Int(*n)),
            Node::Lit(param::Value::Float(f)) => self.literal(t, &Value::Float(*f)),
            Node::Lit(param::Value::Bool(b)) => self.literal(t, &Value::Bool(*b)),
            Node::Text(text) => self.literal(t, &Value::Text(text.clone())),
            Node::RefBinding(name, fields) => match env_find(env, name) {
                Some(e) => {
                    let mut e = e.clone();
                    for f in fields { e = field(t, &e, f)?; }
                    Ok(e)
                }
                None => {
                    // A record may contain a deferred vector. Bind that vector as a uniform before
                    // projecting its component.
                    let (cname, cfields) = self.capture_path(name, fields);
                    let direct = captured_value(&self.bindings, &cname, &cfields).is_ok();
                    if direct {
                        self.captured(t, name, fields)
                    } else {
                        match fields.split_last() {
                            Some((component, prefix)) => {
                                let base = self.captured(t, name, prefix)?;
                                field(t, &base, component)
                            }
                            None => Err(refusal(t, "E_PACKED_CAPTURE", &format!("Captured binding {} is unavailable.", name))),
                        }
                    }
                }
            },
            Node::Time => Ok(one(self.emit(I::Frame("t".to_owned()))?)),
            Node::Vec(ts) if matches!(ts.len(), 2 | 3 | 4) => {
                let mut components = Vec::new();
                for term in ts {
                    let e = self.expression(env, term)?;
                    components.push(self.as_float(&e)?);
                }
                if !components.iter().all(|e| e.registers.len() == 1) {
                    return Err(refusal(t, "E_PACKED_TYPE", "Packed vector components must be scalar values."));
                }
                Ok(regs(components.iter().map(|e| e.registers[0]).collect()))
            }
            Node::Record(fs) => {
                let mut out = Vec::new();
                for (name, value) in fs { out.push((name.clone(), self.expression(env, value)?)); }
                Ok(fields_of(out))
            }
            Node::Assoc(value, updates) => {
                let value = self.expression(env, value)?;
                let mut ups = Vec::new();
                for (name, term) in updates { ups.push((name.clone(), self.expression(env, term)?)); }
                let mut fields: Vec<(String, Expression)> = value
                    .fields
                    .iter()
                    .map(|(name, old)| (name.clone(), ups.iter().find(|(n, _)| n == name).map(|(_, e)| e.clone()).unwrap_or_else(|| old.clone())))
                    .collect();
                for (name, e) in &ups {
                    if !fields.iter().any(|(n, _)| n == name) { fields.push((name.clone(), e.clone())); }
                }
                Ok(Expression { fields, ..value })
            }
            Node::Get(value, f) => {
                let e = self.expression(env, value)?;
                field(t, &e, f)
            }
            Node::Loop { kind: LoopKind::Fold, accs, clauses, body, skip, .. }
                if accs.len() == 1 && clauses.len() == 1 && skip.is_empty()
                    && matches!(&accs[0].0, Pattern::Name(_))
                    && matches!(&clauses[0], (Pattern::Name(_), Term { node: Node::Op { op, skip, .. }, .. }) if op == "range" && skip.is_empty())
                    && canonical_operator(self.residual, "range") =>
            {
                let (Pattern::Name(accumulator), seed) = &accs[0] else { return Err(invalid("unreachable")) };
                let (Pattern::Name(index_name), source) = &clauses[0] else { return Err(invalid("unreachable")) };
                let Node::Op { args, .. } = &source.node else { return Err(invalid("unreachable")) };
                let seed_e = self.expression(env, seed)?;
                let initial = self.carried_initial(seed, &t.ty, &seed_e)?;
                let bound = |me: &mut Lower, term: &Term| -> Res<i64> {
                    let e = me.expression(env, term)?;
                    reg(&me.as_int(&e)?, 0)
                };
                let (first, last) = match args.as_slice() {
                    [(_, last)] => {
                        let first = self.emit(I::Int_const(0))?;
                        (first, bound(self, last)?)
                    }
                    [(_, first), (_, last)] => {
                        let first = bound(self, first)?;
                        (first, bound(self, last)?)
                    }
                    _ => return Err(refusal(source, "E_PACKED_FORM", "A counted loop requires one or two range bounds.")),
                };
                let control = Loop { first, last, finish: -1, initial, limit: Some(op::max_iterations() as i64), span: source.form.span.clone() };
                let ty = t.ty.clone();
                self.carried_loop(&ty, control, &mut |me, header, carried| {
                    let index = one(header);
                    let mut env2 = env.clone();
                    env2.push((accumulator.clone(), carried.clone()));
                    env2.push((index_name.clone(), index));
                    // `(index_name, index) :: (accumulator, carried) :: env`: index_name shadows
                    let value = me.expression(&env2, body)?;
                    let next = me.leaves(body, &ty, &value)?.into_iter().map(|(r, _)| r).collect();
                    Ok((next, None))
                })
            }
            Node::Hof(HofKind::Iterate, parts) if parts.len() >= 3 => {
                let (fn_term, seed, count, until) = (&parts[0], &parts[1], &parts[2], &parts[3..]);
                let seed_e = self.expression(env, seed)?;
                let initial = self.carried_initial(seed, &t.ty, &seed_e)?;
                let count_e = self.expression(env, count)?;
                let last = reg(&self.as_int(&count_e)?, 0)?;
                let first = self.emit(I::Int_const(0))?;
                let control = Loop { first, last, finish: -1, initial, limit: None, span: count.form.span.clone() };
                let ty = t.ty.clone();
                self.carried_loop(&ty, control, &mut |me, _header, carried| {
                    let inlined = me.inline(env, t, fn_term, &[carried.clone()])?;
                    let value = me.coerce(t, &ty, &inlined)?;
                    let stop = match until.first() {
                        None => None,
                        Some(predicate) => {
                            let r = me.inline(env, t, predicate, &[carried.clone(), value.clone()])?;
                            let r = me.coerce(predicate, &Ty::Bool, &r)?;
                            Some(reg(&r, 0)?)
                        }
                    };
                    let next = me.leaves(t, &ty, &value)?.into_iter().map(|(r, _)| r).collect();
                    Ok((next, stop))
                })
            }
            Node::Cond(arms, default) => {
                let mut folded: Term = (**default).clone();
                for (condition, yes) in arms.iter().rev() {
                    folded = Term { node: Node::If(Box::new(condition.clone()), Box::new(yes.clone()), Box::new(folded)), ..t.clone() };
                }
                self.expression(env, &folded)
            }
            Node::Case(scrutinee, arms, default) => {
                let boolean = |value: &Term| -> Term {
                    let inner = Term { ty: Ty::Bool, node: Node::Op { op: "not".to_owned(), args: vec![("x".to_owned(), value.clone())], skip: Vec::new() }, ..value.clone() };
                    Term { ty: Ty::Bool, node: Node::Op { op: "not".to_owned(), args: vec![("x".to_owned(), inner)], skip: Vec::new() }, ..value.clone() }
                };
                let mut folded: Term = (**default).clone();
                for (literal, yes) in arms.iter().rev() {
                    let (value, ty) = match &literal.node {
                        SNode::Num(s) => (param::Value::Float(cfmt::parse_ocaml_float(s).ok_or_else(|| invalid("float_of_string"))?), Ty::Float),
                        SNode::Sym(s) if s == "true" || s == "false" => (param::Value::Bool(s == "true"), Ty::Bool),
                        _ => return Err(refusal(t, "E_PACKED_FORM", "Packed case arms require numeric or boolean literals.")),
                    };
                    let right = Term { path: None, ty: ty.clone(), node: Node::Lit(value), form: std::sync::Arc::new(literal.clone()) };
                    let (left, right) = if scrutinee.ty == Ty::Bool || ty == Ty::Bool { (boolean(scrutinee), boolean(&right)) } else { ((**scrutinee).clone(), right) };
                    let condition = Term { ty: Ty::Bool, node: Node::Op { op: "=".to_owned(), args: vec![("a".to_owned(), left), ("b".to_owned(), right)], skip: Vec::new() }, ..t.clone() };
                    folded = Term { node: Node::If(Box::new(condition), Box::new(yes.clone()), Box::new(folded)), ..t.clone() };
                }
                self.expression(env, &folded)
            }
            Node::Op { op: name, args, .. } if name == "array/sum" && args.len() == 1 && canonical_operator(self.residual, "array/sum") => {
                self.small_sum(env, t, &args[0].1)
            }
            Node::Op { op: name, .. }
                if matches!(name.as_str(), "array/concat" | "array/slice" | "array/count")
                    && match (name.as_str(), &t.ty) {
                        ("array/count", Ty::Int) => true,
                        ("array/concat" | "array/slice", Ty::Array(_)) => true,
                        _ => false,
                    }
                    && !Residual::free_names(t).iter().any(|n| env_find(env, n).is_some())
                    && self.constant_only(name) =>
            {
                // Preserve checked argument order and copying. Invariant composition and its count
                // use the existing selected table/integer captures.
                self.invariant_value(t)
            }
            Node::If(..)
                if matches!(t.ty, Ty::Array(_)) && !Residual::free_names(t).iter().any(|n| env_find(env, n).is_some()) =>
            {
                // An invariant array choice is a lazy table preparation, not a numeric select.
                self.invariant_value(t)
            }
            Node::If(condition, yes, no) => {
                let condition_e = self.expression(env, condition)?;
                let condition = self.as_float(&condition_e)?;
                match &condition.constant {
                    Some(v) => {
                        let chosen = if V::truthy(v)? { yes } else { no };
                        self.expression(env, chosen)
                    }
                    None => {
                        let c = reg(&condition, 0);
                        let branch = |me: &mut Lower, selected: bool, body: &Term| -> Res<Expression> {
                            let cond_reg = *condition.registers.first().ok_or_else(|| invalid("index out of bounds"))?;
                            me.with_guard(cond_reg, selected, &mut |me| me.expression(env, body))
                        };
                        let _ = c;
                        let yes_e = branch(self, true, yes)?;
                        let no_e = branch(self, false, no)?;
                        if condition.registers.len() != 1 {
                            return Err(refusal(t, "E_PACKED_TYPE", "Packed conditions require a scalar value."));
                        }
                        self.select(t, &t.ty.clone(), condition.registers[0], &yes_e, &no_e)
                    }
                }
            }
            Node::CallFn { fn_name, args, body: specialized } => {
                if self.call_depth >= 64 { return Err(refusal(t, "E_PACKED_LIMIT", "Packed helper call depth exceeds 64.")); }
                let mut values = Vec::new();
                for a in args { values.push(self.expression(env, a)?); }
                let (cname, cpath) = self.capture_path(fn_name, &[]);
                let (params, body, scope) = helper_body(t, self.residual, &self.bindings, fn_name, &cname, &cpath)?;
                let env2 = self.bind_parameters(t, &params, &values, &Vec::new())?;
                let previous = self.capture_scope.clone();
                self.capture_scope = scope;
                self.call_depth += 1;
                let chosen: Term = match specialized { Some(b) => (**b).clone(), None => body };
                let result = self.expression(&env2, &chosen);
                self.capture_scope = previous;
                self.call_depth -= 1;
                result
            }
            Node::Op { op: name, args, .. } if name == "error" && self.constant_only("error") => {
                let text = |me: &Lower, arg: &Term| -> Res<String> {
                    let bytes = match &arg.node {
                        Node::Text(text) => text.clone(),
                        Node::RefBinding(n, fields) => match captured_value(&me.bindings, n, fields) {
                            Ok(Value::Text(text)) => text,
                            _ => return Err(refusal(arg, "E_PACKED_CONSTANT", "Packed error code and message must be constant text.")),
                        },
                        _ => return Err(refusal(arg, "E_PACKED_CONSTANT", "Packed error code and message must be constant text.")),
                    };
                    Ok(String::from_utf8_lossy(&bytes).into_owned())
                };
                let find = |key: &str| args.iter().find(|(n, _)| n == key).map(|(_, t)| t);
                // `let code = ... and message = ...`: in order
                let code = text(self, find("code").ok_or_else(|| invalid("Not_found"))?)?;
                let message = text(self, find("message").ok_or_else(|| invalid("Not_found"))?)?;
                let cause = match find("cause") { Some(c) => Some(text(self, c)?), None => None };
                Ok(one(self.emit(I::Raise(t.ty.clone(), code, message, cause))?))
            }
            Node::Op { op: name, args, .. } if name == "color/hsl" && args.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>() == ["h", "s", "l"] => {
                self.hsl(env, args)
            }
            Node::Op { op: name, args, .. } => self.operation(env, t, name, args),
            Node::Let(bindings, result) => {
                let mut env2 = env.clone();
                for (p, term) in bindings {
                    match p {
                        Pattern::Name(n) => {
                            let e = self.expression(&env2, term)?;
                            env2.push((n.clone(), e));
                        }
                        _ => return Err(refusal(term, "E_PACKED_FORM", "Packed let bindings require name patterns.")),
                    }
                }
                self.expression(&env2, result)
            }
            Node::Expanded { body, .. } => self.expression(env, body),
            Node::Bypass(call) => match W::bypass_input(call) {
                Some(input) if input.ty == t.ty => self.expression(env, &input),
                Some(input) if t.ty == Ty::Float && matches!(input.ty, Ty::Int | Ty::Bool) => {
                    let e = self.expression(env, &input)?;
                    self.as_float(&e)
                }
                Some(input) if matches!(t.ty, Ty::Vec2 | Ty::Vec3 | Ty::Vec4) && matches!(input.ty, Ty::Int | Ty::Float) => {
                    let e = self.expression(env, &input)?;
                    let e = self.as_float(&e)?;
                    Ok(regs(vec![reg(&e, 0)?; width_res(&t.ty)? as usize]))
                }
                _ => Err(refusal(t, "E_PACKED_TYPE", "Bypass coercion requires reference evaluation.")),
            },
            _ => Err(refusal(t, "E_PACKED_FORM", &format!("Unsupported packed body form {}.", flow_syntax::syntax::head(&t.form).map(str::to_owned).unwrap_or_else(|| ty::to_string(&t.ty))))),
        }
    }

    fn constant_only(&self, name: &str) -> bool {
        let extra = Residual::residual_ops(self.residual);
        op::find(&extra, name, &context::value()).and_then(|o| op::packed_kind(&o)) == Some(PackedKind::ConstantOnly)
    }

    fn select(&mut self, t: &Term, ty: &Ty, condition: i64, yes: &Expression, no: &Expression) -> Res<Expression> {
        match ty {
            Ty::Record(fields) => {
                let mut out = Vec::new();
                for (name, ty) in fields {
                    let y = field(t, yes, name)?;
                    let n = field(t, no, name)?;
                    out.push((name.clone(), self.select(t, ty, condition, &y, &n)?));
                }
                Ok(fields_of(out))
            }
            _ => {
                let (yes, no) = if *ty == Ty::Int || *ty == Ty::Text {
                    // OCaml evaluates the tuple `(coerce yes, coerce no)` right to left
                    let n = self.coerce(t, ty, no)?;
                    let y = self.coerce(t, ty, yes)?;
                    (y, n)
                } else {
                    let n = self.as_float(no)?;
                    let y = self.as_float(yes)?;
                    (y, n)
                };
                let width = yes.registers.len().max(no.registers.len());
                let mut registers = Vec::new();
                for i in 0..width {
                    let (c, a, b) = (condition, self.component(&yes, i)?, self.component(&no, i)?);
                    registers.push(self.emit(if *ty == Ty::Int {
                        I::Int_select(c, a, b)
                    } else if *ty == Ty::Text {
                        I::Text_select(c, a, b)
                    } else {
                        I::Select(c, a, b)
                    })?);
                }
                Ok(regs(registers))
            }
        }
    }

    fn small_sum(&mut self, env: &Env, at: &Term, source: &Term) -> Res<Expression> {
        fn unwrap(term: &Term) -> &Term {
            match &term.node { Node::Expanded { body, .. } => unwrap(body), _ => term }
        }
        let source = unwrap(source);
        let ty = match &source.ty { Ty::Array(ty) => (**ty).clone(), _ => Ty::Any };
        if !matches!(ty, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) {
            return Err(refusal(source, "E_PACKED_TYPE", "An inner sum requires a numeric array."));
        }
        let count = |me: &mut Lower, term: &Term| -> Res<i64> {
            match me.expression(env, term)?.constant {
                Some(Value::Int(n)) if (0..=64).contains(&n) => Ok(n),
                _ => Err(refusal(term, "E_PACKED_CONSTANT", "An inner map/slice sum requires a constant count from 0 to 64.")),
            }
        };
        let zero = if ty == Ty::Int {
            self.literal(&self.term.clone(), &Value::Int(0))?
        } else {
            let width = width_res(&ty)? as usize;
            let z = self.emit(I::Const(0.0))?;
            regs(vec![z; width])
        };
        let (seed, items): (Expression, Vec<Expression>) = match &source.node {
            Node::Hof(HofKind::Map, parts) if parts.len() == 2 => {
                let range = unwrap(&parts[1]);
                match &range.node {
                    Node::Op { op: name, args, .. } if name == "array/range" && canonical_operator(self.residual, "array/range") => {
                        let count_term = args.iter().find(|(n, _)| n == "count").map(|(_, t)| t).ok_or_else(|| invalid("Not_found"))?;
                        let n = count(self, count_term)?;
                        // Materialise every mapped expression before adding any of them: a later
                        // map error precedes overflow in the sum.
                        let mut items = Vec::new();
                        for i in 0..n {
                            let index = if range.ty == array_of(Ty::Int) { Value::Int(i) } else { Value::Float(i as f64) };
                            let lit = self.literal(&self.term.clone(), &index)?;
                            let inlined = self.inline_expressions(env, at, &parts[0], &[lit])?;
                            items.push(self.coerce(at, &ty, &inlined)?);
                        }
                        (zero, items)
                    }
                    _ => return Err(refusal(range, "E_PACKED_FORM", "An inner map sum requires a constant-count array/range.")),
                }
            }
            Node::Op { op: name, args, .. } if name == "array/slice" && canonical_operator(self.residual, "array/slice") => {
                let find = |key: &str| args.iter().find(|(n, _)| n == key).map(|(_, t)| t).ok_or_else(|| invalid("Not_found"));
                let table = self.expression(env, find("array")?)?;
                if !self.is_table(&table) {
                    return Err(refusal(source, "E_PACKED_CAPTURE", "An inner slice sum requires a numeric table."));
                }
                let first_term = find("first")?;
                let first = self.expression(env, first_term)?;
                if first_term.ty != Ty::Int && !matches!(first.constant, Some(Value::Int(_))) {
                    return Err(refusal(first_term, "E_PACKED_TYPE", "An inner slice sum requires an integer first index."));
                }
                let first = reg(&self.as_int(&first)?, 0)?;
                let n = count(self, find("count")?)?;
                let prepared = reg(&table, 0)?;
                let input = Term { ty: array_of(ty.clone()), node: Node::RefBinding("$inner-sum-table".to_owned(), Vec::new()), ..source.clone() };
                let length_term = Term { ty: Ty::Int, node: Node::Op { op: "array/count".to_owned(), args: vec![("array".to_owned(), input)], skip: Vec::new() }, ..source.clone() };
                let table_value = self.array_values.get(&prepared).cloned().ok_or_else(|| invalid("Not_found"))?;
                let bound = Residual::with_bindings(self.residual, &length_term, &[("$inner-sum-table".to_owned(), table_value)])?;
                let uniform = self.uniform(source, (String::new(), Vec::new()), &Value::Residual(bound))?;
                let length = reg(&self.as_int(&uniform)?, 0)?;
                let constant = |me: &mut Lower, n: i64| me.emit(I::Int_const(n));
                // require condition message value
                let require = |me: &mut Lower, condition: i64, message: &str, value: i64| -> Res<i64> {
                    let failed = me.under(condition, false, I::Raise(Ty::Int, "E_ARRAY_RANGE".to_owned(), message.to_owned(), None))?;
                    me.emit(I::Int_select(condition, value, failed))
                };
                // both (compare Ige first (constant 0)) (compare Ile first (constant max_floatarray_length)):
                // arguments right to left
                let max_len = constant(self, MAX_FLOATARRAY_LENGTH)?;
                let le = self.emit(I::Int_compare(Comparison::Ile, first, max_len))?;
                let c0 = constant(self, 0)?;
                let ge = self.emit(I::Int_compare(Comparison::Ige, first, c0))?;
                let ok = self.emit(I::Binary(Binary::And, ge, le))?;
                let first = require(self, ok, "array/slice needs a non-negative whole length.", first)?;
                // valid = both (compare Ile first length) (compare Ile (constant n) (emit (Int_binary (Isub, length, first))))
                let diff = self.emit(I::Int_binary(Integer::Isub, length, first))?;
                let cn = constant(self, n)?;
                let fit = self.emit(I::Int_compare(Comparison::Ile, cn, diff))?;
                let within = self.emit(I::Int_compare(Comparison::Ile, first, length))?;
                let valid = self.emit(I::Binary(Binary::And, within, fit))?;
                // Keep the guard reachable even when n=0, and guard device gathers: a status error
                // does not stop the rest of a device invocation.
                let c0b = constant(self, 0)?;
                let seed = require(self, valid, "An array slice must be within its source.", c0b)?;
                let seed = if ty == Ty::Int { seed } else { self.emit(I::Convert(Conversion::ToFloat, seed))? };
                let width = if ty == Ty::Int { 1 } else { width_res(&ty)? as usize };
                let seed = regs(vec![seed; width]);
                let mut items = Vec::new();
                for i in 0..n {
                    let ci = constant(self, i)?;
                    let index = self.emit(I::Int_binary(Integer::Iadd, first, ci))?;
                    let mut registers = Vec::new();
                    for component in 0..width {
                        let instruction = if ty == Ty::Int { I::Int_gather(prepared, index) } else { I::Gather(prepared, index, width as i64, component as i64) };
                        registers.push(self.under(valid, true, instruction)?);
                    }
                    items.push(regs(registers));
                }
                (seed, items)
            }
            _ => return Err(refusal(source, "E_PACKED_FORM", "An inner sum requires map over array/range or array/slice with a constant count.")),
        };
        let mut sum = seed;
        for item in &items {
            let (s, item) = if ty == Ty::Int { (self.as_int(&sum)?, self.as_int(item)?) } else { (self.as_float(&sum)?, self.as_float(item)?) };
            let mut registers = Vec::new();
            for (component, register) in s.registers.iter().enumerate() {
                let other = reg(&item, component)?;
                registers.push(self.emit(if ty == Ty::Int { I::Int_binary(Integer::Iadd, *register, other) } else { I::Binary(Binary::Add, *register, other) })?);
            }
            sum = regs(registers);
        }
        Ok(sum)
    }

    fn inline_expressions(&mut self, env: &Env, at: &Term, f: &Term, values: &[Expression]) -> Res<Expression> {
        self.inline(env, at, f, values)
    }

    fn inline(&mut self, env: &Env, at: &Term, f: &Term, values: &[Expression]) -> Res<Expression> {
        if self.call_depth >= 64 { return Err(refusal(at, "E_PACKED_LIMIT", "Packed helper call depth exceeds 64.")); }
        let result_ty = self.term.ty.clone();
        let (params, body, _) = function_body(self.residual, &self.bindings, &result_ty, None, f)?;
        let base: Env = if literal_function(f) { env.clone() } else { Vec::new() };
        let env2 = self.bind_parameters(at, &params, values, &base)?;
        fn scope(me: &Lower, f: &Term) -> Scope {
            match &f.node {
                Node::RefBinding(name, fields) => Some(me.capture_path(name, fields)),
                Node::Fn { capture: Some(value), .. } => scope(me, value),
                Node::FnRef(_) => None,
                _ => me.capture_scope.clone(),
            }
        }
        let previous = self.capture_scope.clone();
        self.capture_scope = scope(self, f);
        self.call_depth += 1;
        let result = self.expression(&env2, &body);
        self.capture_scope = previous;
        self.call_depth -= 1;
        result
    }
}

/// OCaml `Sys.max_floatarray_length` on 64-bit: `max_wosize`.
const MAX_FLOATARRAY_LENGTH: i64 = (1 << 54) - 1;

/// The unfused region of one form; `fusion` and `dynamic` are recorded on it.
pub fn lower(fusion: bool, dynamic: bool, count_source: &CountSource, residual: ResidualId, term: &Term) -> Result<RegionT, Vec<Diagnostic>> {
    match lower_inner(fusion, dynamic, count_source, residual, term) {
        Ok(region) => Ok(region),
        Err(Stop::Refused(d)) | Err(Stop::Error(d)) => {
            let span = d.span.clone().or_else(|| Some(term.form.span.clone()));
            Err(vec![Diagnostic { span, ..d }])
        }
        Err(Stop::Invalid(message)) => Err(vec![diag(Some(term.form.span.clone()), "E_PACKED_LAYOUT", &format!("Packed layout is unsupported: {}", message))]),
        Err(Stop::Unsupported) => Err(vec![diag(
            Some(term.form.span.clone()),
            "E_PACKED_FORM",
            &format!("Packed term {} has unsupported type {}.", lisp::flat(&term.form), ty::to_string(&term.ty)),
        )]),
    }
}

fn lower_inner(fusion: bool, dynamic: bool, count_source: &CountSource, residual: ResidualId, term: &Term) -> Res<RegionT> {
    let view = Residual::residual_view(residual)?;
    let refuse = |code: &str, message: &str| -> Stop { refusal(term, code, message) };
    let result_ty = term.ty.clone();
    // params, accumulator, body, sources, bindings, iteration, result, skip
    let (params, accumulator, body, sources, bindings, iteration, result, skip): (Params, Option<(Pattern, Option<Ty>)>, Term, Vec<Term>, Vec<(String, Value)>, Iteration, RegionResult, Vec<Vec<usize>>) =
        match &term.node {
            Node::Hof(HofKind::Map, parts) if !parts.is_empty() => {
                let (params, body, bindings) = function_body(residual, &view.bindings, &result_ty, None, &parts[0])?;
                (params, None, body, parts[1..].to_vec(), bindings, Iteration::Zip, RegionResult::Collect, Vec::new())
            }
            Node::Hof(HofKind::Reduce, parts) if parts.len() == 3 => {
                let (params, body, bindings) = function_body(residual, &view.bindings, &result_ty, Some((&parts[1], &parts[2])), &parts[0])?;
                let mut params = params;
                if params.is_empty() { return Err(refuse("E_PACKED_FORM", "Packed reduce requires an accumulator parameter.")); }
                let acc = params.remove(0);
                (params, Some(acc), body, vec![parts[2].clone()], bindings, Iteration::Zip, RegionResult::Accumulate(std::sync::Arc::new(parts[1].clone()), false), Vec::new())
            }
            Node::Loop { kind, accs, clauses, body, skip, .. } => {
                let (accumulator, result) = match (kind, accs.as_slice()) {
                    (LoopKind::For, []) => (None, RegionResult::Collect),
                    (LoopKind::Sum, []) => (None, RegionResult::Sum(false)),
                    (LoopKind::Fold | LoopKind::Scan, [(p, seed)]) => (Some((p.clone(), None)), RegionResult::Accumulate(std::sync::Arc::new(seed.clone()), *kind == LoopKind::Scan)),
                    _ => return Err(refuse("E_PACKED_FORM", "Packed fold/scan requires exactly one accumulator.")),
                };
                (
                    clauses.iter().map(|(p, _)| (p.clone(), None)).collect(),
                    accumulator,
                    (**body).clone(),
                    clauses.iter().map(|(_, t)| t.clone()).collect(),
                    view.bindings.clone(),
                    Iteration::Product,
                    result,
                    skip.clone(),
                )
            }
            Node::Op { op, args, .. } if op == "array/sum" && args.len() == 1 => {
                let source = &args[0].1;
                let ty = match &source.ty {
                    Ty::Array(ty) => (**ty).clone(),
                    _ => return Err(refusal(source, "E_PACKED_TYPE", &format!("array/sum source is {}, not an array.", ty::to_string(&source.ty)))),
                };
                let body = Term { ty, node: Node::RefBinding("@item".to_owned(), Vec::new()), ..source.clone() };
                (vec![(Pattern::Name("@item".to_owned()), None)], None, body, vec![source.clone()], view.bindings.clone(), Iteration::Zip, RegionResult::Sum(true), Vec::new())
            }
            _ => return Err(refuse("E_PACKED_FORM", "Packed kernels require map, reduce, a numeric loop or array/sum.")),
        };
    let binding_fn: Option<String> = match &term.node {
        Node::Hof(_, parts) => match parts.first() {
            Some(Term { node: Node::RefBinding(name, fields), .. }) if fields.is_empty() => Some(name.clone()),
            _ => None,
        },
        _ => None,
    };
    let mut names: Vec<String> = Vec::new();
    for (p, _) in params.iter().chain(accumulator.iter()) {
        if let Pattern::Name(name) = p { names.push(name.clone()); }
    }
    // ponytail: correlated clauses stay interpreted; lower their changing source counts before tiling them.
    if iteration == Iteration::Product && sources.iter().any(|s| reads_names(&names, s)) {
        return Err(refuse("E_PACKED_FORM", "Correlated product sources require reference evaluation."));
    }
    let mut widths: Vec<i64> = Vec::new();
    for s in &sources {
        let width = if s.ty == array_of(Ty::Text) { Some(1) } else { packed_ops::array_width(&s.ty).map(|w| w as i64) };
        match width {
            Some(width) => widths.push(width),
            None => return Err(refusal(s, "E_PACKED_TYPE", &format!("Packed source must be a float/vector array, got {}.", ty::to_string(&s.ty)))),
        }
    }
    if sources.len() != params.len() {
        return Err(refuse("E_PACKED_FORM", &format!("Packed kernel has {} sources but {} parameters.", sources.len(), params.len())));
    }
    let arc_term = std::sync::Arc::new(term.clone());
    let mut lower = Lower {
        residual,
        term: arc_term.clone(),
        dynamic,
        bindings: bindings,
        sources: sources.clone(),
        code: Vec::new(),
        uniforms: Vec::new(),
        uniform_names: Vec::new(),
        shared: HashMap::new(),
        loop_finishes: HashMap::new(),
        control_scope: Vec::new(),
        guards: Vec::new(),
        integer_registers: Vec::new(),
        text_registers: Vec::new(),
        bottom_registers: Vec::new(),
        array_registers: HashMap::new(),
        array_values: HashMap::new(),
        table_uniforms: HashMap::new(),
        captures: HashMap::new(),
        capture_scope: None,
        call_depth: 0,
    };
    let mut env: Env = Vec::new();
    for (i, (pattern, annotation)) in params.iter().enumerate() {
        match pattern {
            Pattern::Name(name) => {
                let element = ty::elem(&sources[i].ty).ok_or_else(|| invalid("option is None"))?;
                if annotation.is_none() || annotation.as_ref() == Some(&element) {
                    env.push((name.clone(), lower.input(i, widths[i])?));
                } else {
                    return Err(refuse(
                        "E_PACKED_TYPE",
                        &format!(
                            "Packed parameter {} annotation {} does not match its {}-component source.",
                            name,
                            annotation.as_ref().map(ty::to_string).unwrap_or_else(|| "none".to_owned()),
                            widths[i]
                        ),
                    ));
                }
            }
            _ => return Err(refuse("E_PACKED_FORM", "Packed parameters require name patterns.")),
        }
    }
    // The accumulator comes first in the OCaml list (`name, ... :: env`), so it shadows nothing here
    // but is searched first: push it last.
    match (&accumulator, &result) {
        (Some((Pattern::Name(name), annotation)), RegionResult::Accumulate(seed, _)) => {
            if !matches!(seed.ty, Ty::Float | Ty::Int | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) {
                return Err(refusal(seed, "E_PACKED_TYPE", &format!("Packed accumulator {} has unsupported seed type {}.", name, ty::to_string(&seed.ty))));
            }
            let width = width_res(&seed.ty)?;
            if annotation.is_some() && annotation.as_ref() != Some(&seed.ty) && !(seed.ty == Ty::Int && annotation.as_ref() == Some(&Ty::Float)) {
                return Err(refusal(
                    seed,
                    "E_PACKED_TYPE",
                    &format!(
                        "Packed accumulator {} annotation {} does not match seed type {}.",
                        name,
                        annotation.as_ref().map(ty::to_string).unwrap_or_else(|| "none".to_owned()),
                        ty::to_string(&seed.ty)
                    ),
                ));
            }
            let integer = seed.ty == Ty::Int && body.ty == Ty::Int;
            let mut registers = Vec::new();
            for i in 0..width {
                registers.push(lower.emit(if integer { I::Int_accumulator } else { I::Accumulator(i) })?);
            }
            env.push((name.clone(), regs(registers)));
        }
        (None, _) => {}
        _ => return Err(refuse("E_PACKED_FORM", "Packed accumulator requires a name pattern.")),
    }

    let result_elem = match &term.ty { Ty::Array(ty) => (**ty).clone(), ty => ty.clone() };
    let output_expression = lower.expression(&env, &body)?;
    let outputs = record_outputs(&mut lower, &output_expression, &result_elem)?;
    let output_e = if result_elem == Ty::Text {
        lower.as_text(&output_expression)?
    } else if result_elem == Ty::Int && body.ty == Ty::Int {
        lower.as_int(&output_expression)?
    } else if (body.ty == Ty::Float && term.ty == Ty::Int) || !outputs.is_empty() {
        output_expression.clone()
    } else {
        lower.as_float(&output_expression)?
    };
    let mut output = output_e.registers;
    if outputs.is_empty() && !matches!(result_elem, Ty::Int | Ty::Text | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) {
        return Err(refuse("E_PACKED_TYPE", &format!("Packed result must be float or vector, got {}.", ty::to_string(&result_elem))));
    }
    let width = if result_elem == Ty::Text { 1 } else if !outputs.is_empty() { 0 } else { width_res(&result_elem)? as usize };
    if matches!(result, RegionResult::Sum(_) | RegionResult::Accumulate(..)) && !matches!(body.ty, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) {
        return Err(refusal(&body, "E_PACKED_TYPE", &format!("Packed reduction body has unsupported type {}.", ty::to_string(&body.ty))));
    }
    // array_init also broadcasts a scalar to the requested vector width.
    if output.len() == 1 && width > 1 { output = vec![output[0]; width]; }
    if output.len() != width {
        return Err(refuse("E_PACKED_TYPE", &format!("Packed result has {} components but its type requires {}.", output.len(), width)));
    }
    let counts: Vec<Option<i64>> = sources.iter().map(|s| count_of(residual, s)).collect();
    let count = match count_of_sources(iteration, &counts) {
        Ok(c) => c,
        Err(Stop::Unsupported) => return Err(refuse("E_PACKED_LIMIT", "Packed product source count exceeds the integer limit.")),
        Err(other) => return Err(other),
    };
    let mut sites: Vec<(i64, Path, Vec<i64>)> = vec![(view.instance, term.path.clone().unwrap_or_else(|| view.site.clone()), view.iter.clone())];
    paths(&view, &mut sites, &body);
    match &term.node {
        Node::Hof(_, parts) => match parts.first() {
            Some(Term { node: Node::Fn { zone, .. }, .. }) => sites.push((view.instance, zone.clone(), view.iter.clone())),
            Some(Term { node: Node::RefBinding(name, fields), .. }) if fields.is_empty() => {
                if let Some(Value::Fn(f)) = view.bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v) {
                    if let Some((_, Term { path: Some(p), .. })) = Residual::function_body(*f) {
                        let keep = p.len().saturating_sub(1);
                        sites.push((view.instance, p[..keep.min(p.len())].to_vec(), view.iter.clone()));
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }
    sites.reverse();
    let skip = iteration_skips(&view.iter, &skip);
    let code = finish_loops(&lower.loop_finishes, std::mem::take(&mut lower.code));
    let code: Vec<I> = if code.iter().any(integer_work) {
        code
    } else {
        code.into_iter()
            .map(|i| match i {
                I::Guarded(guards, body) => match *body {
                    I::Int_const(n) => I::Guarded(guards, Box::new(I::Const(n as f64))),
                    other => I::Guarded(guards, Box::new(other)),
                },
                I::Int_const(n) => I::Const(n as f64),
                other => other,
            })
            .collect()
    };
    let region = R::make(Parts {
        residual,
        term: arc_term.clone(),
        sources: sources.iter().map(|t| (residual, std::sync::Arc::new(t.clone()))).collect(),
        widths,
        uniforms: lower.uniforms,
        uniform_names: lower.uniform_names,
        code,
        outputs: if outputs.is_empty() { vec![output] } else { outputs },
        count,
        origin: None,
        iteration,
        result,
        skip,
        stages: vec![(residual, arc_term)],
        fusion,
        dynamic,
        count_source: count_source.clone(),
        body: std::sync::Arc::new(body),
        binding_fn,
        sites,
    });
    match region {
        Ok(r) => Ok(r),
        Err(mut ds) => Err(Stop::Error(ds.remove(0))),
    }
}

/// A frame-fold template keeps its captures as uniforms: rebinding reads the next step's bindings.
pub fn rebind(t: &RegionT, residual: ResidualId) -> Option<RegionT> {
    let view = Residual::residual_view(residual).ok()?;
    if !t.dynamic
        || t.stages.len() != 1
        || *t.term != *view.term
        || view.previous
        || flow_eval::state_dependent(&Value::Residual(residual))
    {
        return None;
    }
    let attempt = || -> Res<Option<RegionT>> {
        let captured: Option<Term> = match &t.term.node {
            Node::Hof(_, parts) => match parts.first() {
                Some(f @ Term { node: Node::Fn { capture: Some(_), .. }, .. }) => Some(f.clone()),
                _ => None,
            },
            _ => None,
        };
        let bindings_of = |residual: ResidualId| -> Res<Vec<(String, Value)>> {
            let view = Residual::residual_view(residual)?;
            match (&captured, &t.binding_fn) {
                (Some(f), _) => match Residual::eval_term(residual, f, &input::at_time(0.0), None, None, None) {
                    Ok(Value::Fn(function)) if Residual::function_body(function).map(|(_, body)| body == *t.body).unwrap_or(false) => Ok(Residual::function_bindings(function)),
                    _ => Err(Stop::Unsupported),
                },
                (None, None) => Ok(view.bindings),
                (None, Some(name)) => match view.bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v) {
                    Some(Value::Fn(f)) if Residual::function_body(*f).map(|(_, body)| body == *t.body).unwrap_or(false) => Ok(Residual::function_bindings(*f)),
                    _ => Err(Stop::Unsupported),
                },
            }
        };
        let bindings = bindings_of(residual)?;
        let mut previous: Option<Vec<(String, Value)>> = None;
        let mut uniforms = Vec::new();
        for (i, (name, fields)) in t.uniform_names.iter().enumerate() {
            if name.is_empty() {
                uniforms.push(t.uniforms[i].clone());
                continue;
            }
            let value = captured_value(&bindings, name, fields)?;
            if previous.is_none() { previous = Some(bindings_of(t.residual)?); }
            let before = captured_value(previous.as_ref().unwrap(), name, fields)?;
            uniforms.push(match (&before, &value) {
                (Value::List(b), Value::List(a)) if b.len() == a.len() => packed_list(&value).ok_or(Stop::Unsupported)?,
                (Value::List(_), _) => return Err(Stop::Unsupported),
                (_, Value::List(_)) => return Err(Stop::Unsupported),
                _ => value,
            });
        }
        let mut sources = Vec::new();
        for (r, term) in &t.sources {
            if *r != t.residual { return Err(Stop::Unsupported); }
            sources.push((residual, term.clone()));
        }
        let counts: Vec<Option<i64>> = sources.iter().map(|(r, s)| count_of(*r, s)).collect();
        let count = count_of_sources(t.iteration, &counts)?;
        match R::Private::with_captures(t, residual, uniforms, sources, vec![(residual, t.term.clone())], count) {
            Ok(region) => Ok(Some(region)),
            Err(_) => Ok(None),
        }
    };
    attempt().ok().flatten()
}

#[allow(dead_code)]
fn _unused(_: Stage) {}
