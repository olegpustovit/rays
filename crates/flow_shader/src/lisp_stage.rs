//! A Lisp entry compiled as a stage: one Packed program of the entry over its element parameters, an
//! output per leaf of its result record. Its array parameters are buffers (Raster's bindings).
use flow_syntax::symbol::Symbol;
use flow_check::catalog::Catalog;
use flow_check::op::{self, Op, Shape, Signature};
use flow_check::workspace;
use flow_eval as E;
use flow_ir::packed::{self, CompileOptions};
use flow_syntax::lisp;
use flow_syntax::syntax::{self, Node, Syntax};
use flow_syntax::ty::{self, FnSignature, Ty};
use flow_syntax::value::{self, Value};
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

pub fn fail<X>(message: &str) -> Result<X, Diagnostic> {
    Err(value::fail("E_RASTER", message))
}

/// The entry's parameters, read from its authored `defn`.
pub fn parameters(forms: &[Syntax], entry: &str) -> Result<Vec<(String, Ty)>, Diagnostic> {
    fn find(form: &Syntax, entry: &str) -> Option<Syntax> {
        let Node::List(items) = form.node else { return None };
        if let [first, second, rest @ ..] = form.range(items) {
            let first = form.child(*first);
            let second = form.child(*second);
            if let (Node::Sym(head), Node::Sym(name)) = (first.node, second.node) {
                if first.name(head) == "defn" && second.name(name) == entry {
                    return rest.iter().find_map(|id| {
                        let child = form.child(*id);
                        matches!(child.node, Node::Vec(_)).then_some(child)
                    });
                }
            }
        }
        form.range(items).iter().find_map(|id| find(&form.child(*id), entry))
    }
    let Some(params) = forms.iter().find_map(|form| find(form, entry)) else { return fail(&format!("no defn {entry}")) };
    let mut out = Vec::new();
    for id in params.children() {
        let p = params.child(*id);
        if let Node::List(items) = p.node {
            if let [a, b, c] = p.range(items) {
                let a = p.child(*a);
                let b = p.child(*b);
                let c = p.child(*c);
                if let (Node::Sym(name), Node::Sym(colon)) = (a.node, b.node) {
                    let name = a.name(name);
                    if b.name(colon) == ":" {
                        match ty::of_syntax(&c) {
                            Some(ty) => {
                                out.push((name.to_owned(), ty));
                                continue;
                            }
                            None => return fail(&format!("{entry}: parameter {name} has no stage type")),
                        }
                    }
                }
            }
        }
        return fail(&format!("{entry}: every stage parameter is typed: {}", lisp::text(std::slice::from_ref(&p))));
    }
    Ok(out)
}

/// The `index`th array parameter is a host buffer bound at `buffer(index)`. Before compiling, its
/// placeholder becomes a distinct (index + 1)-element array.
fn dummy(index: i64, ty: &Ty) -> Result<Value, Diagnostic> {
    match ty {
        Ty::Array(t) if **t == Ty::Int => Ok(Value::IntArray(Arc::new(vec![0; index as usize + 1]))),
        Ty::Array(t) if **t == Ty::Float => Ok(Value::Floats(1, Arc::new(vec![0.0; index as usize + 1]))),
        ty => fail(&format!("a stage buffer is (array int) or (array float), not {}", ty::to_string(ty))),
    }
}

fn column(ty: &Ty) -> Value {
    match ty {
        Ty::Int => Value::IntArray(Arc::new(vec![0])),
        Ty::Float => Value::Floats(1, Arc::new(vec![0.0])),
        Ty::Vec2 => Value::Floats(2, Arc::new(vec![0.0; 2])),
        Ty::Vec3 => Value::Floats(3, Arc::new(vec![0.0; 3])),
        Ty::Vec4 => Value::Floats(4, Arc::new(vec![0.0; 4])),
        _ => panic!("Raster.column"),
    }
}

pub fn is_buffer(p: &(String, Ty)) -> bool {
    matches!(p.1, Ty::Array(_))
}

/// A stage output: a leaf of the entry's result, in field order (the program's outputs), `flat` when
/// it sits under `:flat`.
#[derive(Clone, Debug, PartialEq)]
pub struct Leaf {
    pub path: flow_check::workspace::Path,
    pub ty: Ty,
    pub flat: bool,
}

#[derive(Clone)]
pub struct Stage {
    pub params: Vec<(String, Ty)>,
    pub leaves: Vec<Leaf>,
    pub program: packed::T,
    pub bindings: Vec<(String, i64)>,
}

fn leaves(flat: bool, path: &[Symbol], ty: &Ty) -> Vec<Leaf> {
    match ty {
        Ty::Record(fields) => fields
            .iter()
            .flat_map(|(name, ty)| {
                let mut p = path.to_vec();
                p.push((*name).into());
                leaves(flat || name == "flat", &p, ty)
            })
            .collect(),
        ty => vec![Leaf { path: path.to_vec(), ty: ty.clone(), flat }],
    }
}

/// A vertex stage's buffers bind in their order; a fragment buffer named like a vertex one shares its
/// binding, any other follows the vertex stage's.
pub fn compile(shared: &[(String, i64)], declarations: &str, entry: &str) -> Result<Stage, Diagnostic> {
    let forms = syntax::parse(declarations)?;
    let params = parameters(&forms, entry)?;
    let buffers: Vec<(String, Ty)> = params.iter().filter(|p| is_buffer(p)).cloned().collect();
    let elements: Vec<(String, Ty)> = params.iter().filter(|p| !is_buffer(p)).cloned().collect();
    let mut next = shared.len() as i64;
    let mut bindings = Vec::new();
    for (name, _) in &buffers {
        let slot = match shared.iter().find(|(n, _)| n == name) {
            Some((_, slot)) => *slot,
            None => {
                let slot = next;
                next += 1;
                slot
            }
        };
        bindings.push((name.clone(), slot));
    }
    let noise3 = op::all().iter().find(|o| o.name == "noise3").expect("noise3 is a builtin").clone();
    let ops: Vec<Op> = buffers
        .iter()
        .enumerate()
        .map(|(index, (_, ty))| {
            let name = format!("raster/buffer{index}");
            let (out_ty, body_ty, body_name) = (ty.clone(), ty.clone(), name.clone());
            Op {
                name,
                packed_extension: None,
                signature: Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] },
                out: Arc::new(move |_| Ok(out_ty.clone())),
                shape: Shape::Struct { splice: false },
                check: Arc::new(|_| Ok(())),
                body: Arc::new(move |_, _, _| Ok(flow_syntax::value::source_value(body_name.clone(), body_ty.clone(), vec![].into()))),
                ..noise3.clone()
            }
        })
        .collect();
    let text = format!(
        "(workspace raster {} (graph g :context value (let* [{} kernel (fn [{}] ({} {}))] 0.0)))",
        declarations,
        buffers
            .iter()
            .enumerate()
            .map(|(index, (name, _))| format!("{name} (raster/buffer{index})"))
            .collect::<Vec<_>>()
            .join(" "),
        elements.iter().map(|(name, ty)| format!("({name} : {})", ty::to_string(ty))).collect::<Vec<_>>().join(" "),
        entry,
        params.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>().join(" "),
    );
    let forms = syntax::parse(&text)?;
    let catalog = Catalog { version: 1, kinds: vec![] };
    let ws = match workspace::check(&ops, false, false, None, &catalog, &forms) {
        (Some(ws), d) if d.is_empty() => ws,
        (_, mut d) if !d.is_empty() => return Err(d.remove(0)),
        _ => return fail(&format!("{entry}: unchecked")),
    };
    let evaluated = E::static_with(&ws, true, None, None)?;
    let key = vec!["g".to_owned(), "kernel".to_owned()];
    let function = match evaluated.records.iter().find(|(p, _)| *p == key).and_then(|(_, v)| v.first()) {
        Some((_, Value::Fn(f))) => *f,
        _ => return fail(&format!("{entry}: the stage is not a function")),
    };
    let result = match E::residual::function_body(function) {
        Some((_, body)) => body.ty().clone(),
        None => Ty::Any,
    };
    let mut bound = Vec::new();
    for (name, ty) in &buffers {
        let slot = bindings.iter().find(|(n, _)| n == name).expect("bound").1;
        bound.push(dummy(slot, ty)?);
    }
    let function = E::bind_captures(function, &|v: &Value| match v {
        Value::Source(g) if g.fields.is_empty() && g.name.starts_with("raster/buffer") => {
            bound[g.name[13..].parse::<usize>().expect("buffer index")].clone()
        }
        other => other.clone(),
    });
    let signature = FnSignature { params: elements.iter().map(|(_, t)| t.clone()).collect(), result: result.clone() };
    let columns: Vec<Value> = elements.iter().map(|(_, t)| column(t)).collect();
    let residual = E::map_kernel(None, None, &signature, function, &columns)?;
    match residual {
        Value::Residual(r) => {
            let term = E::residual::residual_view(r)?.term;
            let options = CompileOptions::default();
            match packed::compile_template(&options, r, &term) {
                Some(program) => Ok(Stage { params, leaves: leaves(false, &[], &result), program, bindings }),
                None => match packed::compile_result(&options, r, &term) {
                    Err(mut d) if !d.is_empty() => Err(d.remove(0)),
                    _ => fail(&format!("{entry} does not compile to a packed program")),
                },
            }
        }
        _ => fail(&format!("{entry}: the stage did not defer")),
    }
}
