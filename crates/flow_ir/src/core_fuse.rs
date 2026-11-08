//! Port of lib/flow_ir/core_fuse.ml: fusion on regions. A map-like region absorbs the Collect + Zip
//! regions that feed it when their counts or origins agree, sharing equal instructions, within the
//! register limit. Declining leaves the region unfused; it never fails.
//!
//! OCaml physical equality (`==`) on terms and values is structural equality here.
use crate::core_ir::Region::{self as R, CountSource, Instruction as I, Iteration, RegionResult};
use crate::core_ir::RegionT;
use crate::core_region::{count_of, finish_loops, instruction_key, lower};
use flow_check::workspace::{Node, Term};
use flow_eval::Residual;
use flow_syntax::packed_ops::{self, Conversion};
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as V, ResidualId, Value};
use std::collections::HashMap;
use std::sync::Arc;

const MAX_INT: i64 = 4_611_686_018_427_387_903;

/// OCaml `Core_region.Unsupported`: the register limit declined, or a producer cannot be spliced.
#[derive(Debug)]
pub struct Unsupported;
type Res<X> = Result<X, Unsupported>;

type Source = (ResidualId, Arc<Term>);

fn bare(i: &I) -> &I {
    match i { I::Guarded(_, body) => bare(body), other => other }
}

fn source_input(term: &Term, source: i64, width: i64, component: i64) -> I {
    match term.ty() {
        Ty::Array(e) if **e == Ty::Int => I::Int_input(source),
        Ty::Array(e) if **e == Ty::Text => I::Text_input(source),
        _ => I::Input(source, width, component),
    }
}

/// An accumulating register program: instructions equal up to registers share one register, and
/// exceeding the register limit is [`Unsupported`].
struct Builder {
    sources: Vec<Source>,
    widths: Vec<i64>,
    uniforms: Vec<Value>,
    code: Vec<I>,
    shared: HashMap<String, i64>,
    scope: Vec<i64>,
    finishes: HashMap<i64, i64>,
}

type InputFn<'a> = &'a mut dyn FnMut(&mut Builder, i64, i64, i64) -> Res<i64>;

impl Builder {
    fn new() -> Builder {
        Builder { sources: Vec::new(), widths: Vec::new(), uniforms: Vec::new(), code: Vec::new(), shared: HashMap::new(), scope: Vec::new(), finishes: HashMap::new() }
    }

    fn emit(&mut self, instruction: I) -> Res<i64> {
        // ponytail: the sharing key is the Debug text of (scope, key); a typed hash only if fusion shows hot.
        let key = format!("{:?}|{:?}", self.scope, instruction_key(&instruction));
        let control = matches!(bare(&instruction), I::Loop_begin(_) | I::Loop_carry(..) | I::Loop_end(..));
        if !control {
            if let Some(id) = self.shared.get(&key) { return Ok(*id); }
        }
        if self.code.len() >= packed_ops::REGISTER_LIMIT { return Err(Unsupported); }
        let id = self.code.len() as i64;
        self.code.push(instruction);
        if !control { self.shared.insert(key, id); }
        Ok(id)
    }

    fn append_source(&mut self, source: Source, width: i64) -> i64 {
        let i = self.sources.len() as i64;
        self.sources.push(source);
        self.widths.push(width);
        i
    }

    fn append_uniforms(&mut self, values: &[Value]) -> i64 {
        let offset = self.uniforms.len() as i64;
        self.uniforms.extend(values.iter().cloned());
        offset
    }

    fn append_code(&mut self, p: &RegionT, input: InputFn, uniform: i64) -> Res<Vec<Vec<i64>>> {
        let mut registers = vec![0_i64; p.code.len()];
        for (i, instruction) in p.code.iter().enumerate() {
            let produced = match bare(instruction) {
                I::Input(source, width, component) => {
                    let register = input(self, *source, *width, *component)?;
                    Some(if R::integer_register(&self.code[register as usize]) { self.emit(I::Convert(Conversion::ToFloat, register))? } else { register })
                }
                I::Int_input(source) => {
                    let register = input(self, *source, 1, 0)?;
                    if !R::integer_register(&self.code[register as usize]) { return Err(Unsupported); }
                    Some(register)
                }
                I::Text_input(source) => {
                    let register = input(self, *source, 1, 0)?;
                    if !R::text_register(&self.code[register as usize]) { return Err(Unsupported); }
                    Some(register)
                }
                _ => None,
            };
            // The producer already ran eagerly. Alias its result, including a guarded input, rather
            // than copying an instruction across a loop boundary.
            registers[i] = match produced {
                Some(register) => register,
                None => {
                    let remapped = remap(instruction, &registers, uniform);
                    self.emit(remapped)?
                }
            };
            match bare(instruction) {
                I::Loop_begin(_) => self.scope.push(registers[i]),
                I::Loop_end(owner, _, _) => {
                    self.finishes.insert(registers[*owner as usize], registers[i]);
                    self.scope.pop();
                }
                _ => {}
            }
        }
        Ok(p.outputs.iter().map(|o| o.iter().map(|r| registers[*r as usize]).collect()).collect())
    }

    #[allow(clippy::type_complexity)]
    fn finish(self) -> (Vec<Source>, Vec<i64>, Vec<Value>, Vec<I>) {
        (self.sources, self.widths, self.uniforms, finish_loops(&self.finishes, self.code))
    }
}

fn remap(instruction: &I, registers: &[i64], uniform: i64) -> I {
    let at = |n: i64| registers[n as usize];
    match instruction {
        I::Uniform(n, component) => I::Uniform(uniform + n, *component),
        I::Array_prepare(n, ty) => I::Array_prepare(uniform + n, ty.clone()),
        I::Text_uniform(n) => I::Text_uniform(uniform + n),
        I::Int_uniform(n) => I::Int_uniform(uniform + n),
        I::Loop_begin(loop_) => I::Loop_begin(R::Loop {
            first: at(loop_.first),
            last: at(loop_.last),
            initial: loop_.initial.iter().map(|r| at(*r)).collect(),
            finish: -1,
            ..loop_.clone()
        }),
        I::Loop_carry(owner, component, ty) => I::Loop_carry(at(*owner), *component, ty.clone()),
        I::Loop_end(owner, next, stop) => I::Loop_end(at(*owner), next.iter().map(|r| at(*r)).collect(), stop.map(at)),
        I::Loop_result(owner, component, ty) => I::Loop_result(at(*owner), *component, ty.clone()),
        I::Guarded(guards, body) => I::Guarded(guards.iter().map(|(c, yes)| (at(*c), *yes)).collect(), Box::new(remap(body, registers, uniform))),
        // the prepared array is a register too: `map_registers` leaves it as it is
        I::Gather(a, i, w, c) => I::Gather(at(*a), at(*i), *w, *c),
        I::Int_gather(a, i) => I::Int_gather(at(*a), at(*i)),
        I::Text_gather(a, i) => I::Text_gather(at(*a), at(*i)),
        other => R::map_registers(&at, other),
    }
}

fn source_region(through_exact: bool, dynamic: bool, count_source: &CountSource, residual: ResidualId, term: &Term) -> Option<RegionT> {
    let arena = term.arena();
    match term.node {
        Node::RefBinding(id) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => {
            let name = term.name(arena.ref_binding(id).name);
            let view = Residual::residual_view(residual).ok()?;
            match view.bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v) {
                Some(Value::Residual(r)) => {
                    let inner = Residual::residual_view(*r).ok()?;
                    source_region(through_exact, dynamic, count_source, *r, &inner.term)
                }
                _ => None,
            }
        }
        Node::Op(id) if term.name(arena.op(id).op) == "exact" && arena.fields(arena.op(id).args).len() == 1 && through_exact => {
            source_region(through_exact, dynamic, count_source, residual, &term.child(arena.fields(arena.op(id).args)[0].term))
        }
        _ => lower(true, dynamic, count_source, residual, term).ok().map(|region| fuse(through_exact, region)),
    }
}

/// Absorb the `Collect` regions of equal count or origin that feed this one. `through_exact` also
/// absorbs a producer an `(exact x)` wraps, for a device region read back as a whole (a fused SOP
/// write's); otherwise `exact` is a boundary.
pub fn fuse(through_exact: bool, t: RegionT) -> RegionT {
    if !t.skip.is_empty() || (t.iteration == Iteration::Product && t.sources.len() != 1) { return t; }
    let children: Vec<Option<RegionT>> = t.sources.iter().map(|(r, term)| source_region(through_exact, t.dynamic, &t.count_source, *r, term)).collect();
    let origins: Vec<Option<R::Origin>> = children
        .iter()
        .enumerate()
        .map(|(i, c)| match c {
            Some(p) => p.origin.clone(),
            None => {
                let (r, s) = &t.sources[i];
                t.count_source.call(*r, s)
            }
        })
        .collect();
    let origin = if !origins.is_empty() && origins[0].is_some() && origins.iter().all(|o| *o == origins[0]) { origins[0].clone() } else { None };
    let counts: Vec<Option<i64>> = children
        .iter()
        .enumerate()
        .map(|(i, c)| match c {
            Some(p) => p.count,
            None => {
                let (r, s) = &t.sources[i];
                count_of(*r, s)
            }
        })
        .collect();
    let count = if counts.iter().all(|c| c.is_some()) && !counts.is_empty() { Some(counts.iter().fold(MAX_INT, |n, c| n.min(c.unwrap()))) } else { t.count };
    let origin_for_t = if t.result == RegionResult::Collect { origin.clone() } else { None };
    let t = match R::rebuild(&t, R::Rebuild { count: Some(count), origin: Some(origin_for_t), ..Default::default() }) {
        Ok(rebuilt) => rebuilt,
        Err(_) => t,
    };
    let can_fuse = |child: &Option<RegionT>| -> bool {
        match child {
            Some(p) if p.result == RegionResult::Collect && p.iteration == Iteration::Zip && p.skip.is_empty() && p.outputs.len() == 1 => {
                t.sources.len() == 1 || (p.count.is_some() && p.count == count) || (origin.is_some() && p.origin == origin)
            }
            _ => false,
        }
    };
    if !children.iter().any(can_fuse) { return t; }
    let attempt = || -> Res<RegionT> {
        let mut b = Builder::new();
        let mut stages = t.stages.clone();
        let mut sites: Vec<(i64, flow_check::workspace::Path, Vec<i64>)> = t.sites.to_vec();
        let mut inputs: Vec<Vec<i64>> = Vec::new();
        for (i, child) in children.iter().enumerate() {
            if can_fuse(child) {
                let p = child.as_ref().unwrap();
                let mut source_inputs = Vec::new();
                for (j, source) in p.sources.iter().enumerate() { source_inputs.push(b.append_source(source.clone(), p.widths[j])); }
                let uniform = b.append_uniforms(&p.uniforms);
                stages.extend(p.stages.iter().cloned());
                let mut merged = p.sites.to_vec();
                merged.extend(sites);
                sites = merged;
                let out = b.append_code(p, &mut |b: &mut Builder, i: i64, width: i64, component: i64| {
                    b.emit(source_input(&p.sources[i as usize].1, source_inputs[i as usize], width, component))
                }, uniform)?;
                inputs.push(out[0].clone());
            } else {
                let source = b.append_source(t.sources[i].clone(), t.widths[i]);
                let mut out = Vec::new();
                for component in 0..t.widths[i] { out.push(b.emit(source_input(&t.sources[i].1, source, t.widths[i], component))?); }
                inputs.push(out);
            }
        }
        let uniform = b.append_uniforms(&t.uniforms);
        let outputs = b.append_code(&t, &mut |_b: &mut Builder, i: i64, _w: i64, component: i64| Ok(inputs[i as usize][component as usize]), uniform)?;
        let (sources, widths, uniforms, code) = b.finish();
        R::rebuild(&t, R::Rebuild {
            sources: Some(sources), widths: Some(widths), uniforms: Some(uniforms), code: Some(code), outputs: Some(outputs),
            iteration: Some(Iteration::Zip), stages: Some(stages), sites: Some(sites), ..Default::default()
        })
        .map_err(|_| Unsupported)
    };
    match attempt() { Ok(fused) => fused, Err(_) => t }
}

enum Resolved {
    Value(Value),
    Term(ResidualId, Term),
}

fn resolved(residual: ResidualId, term: &Term) -> Resolved {
    if let Node::RefBinding(id) = term.node {
        let binding = term.arena().ref_binding(id);
        if term.arena().name_ids(binding.fields).is_empty() {
            let name = term.name(binding.name);
            if let Ok(view) = Residual::residual_view(residual) {
                match view.bindings.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()) {
                    Some(Value::Residual(r)) => {
                        if let Ok(inner) = Residual::residual_view(r) { return resolved(r, &inner.term); }
                    }
                    Some(value) => return Resolved::Value(value),
                    None => {}
                }
            }
        }
    }
    Resolved::Term(residual, term.clone())
}

fn same_ops(a: ResidualId, b: ResidualId) -> bool {
    let (xs, ys) = (Residual::residual_ops(a), Residual::residual_ops(b));
    xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| crate::core_region::same_op(x, y))
}

fn same_source(a: &Source, b: &Source) -> bool {
    match (resolved(a.0, &a.1), resolved(b.0, &b.1)) {
        (Resolved::Value(a), Resolved::Value(b)) => a == b,
        (Resolved::Term(ar, x), Resolved::Term(br, y)) => {
            let dependent = |r: ResidualId| flow_eval::state_dependent(&Value::Residual(r));
            if dependent(ar) || dependent(br) { return false; }
            (ar == br && x == y)
                || (match (Residual::residual_view(ar), Residual::residual_view(br)) {
                    (Ok(va), Ok(vb)) => {
                        *va.term == x && *vb.term == y && same_ops(ar, br)
                            && Residual::residual_authored_key(ar) == Residual::residual_authored_key(br)
                            && flow_eval::structure(&Value::Residual(ar)) == flow_eval::structure(&Value::Residual(br))
                    }
                    _ => false,
                })
        }
        _ => false,
    }
}

/// Sibling Collect regions of equal count, origin or identical immutable source sequence, fused
/// into one region with an output each (in list order). `None` when they disagree or the register
/// limit declines.
pub fn merge(regions: &[RegionT]) -> Option<RegionT> {
    let first = regions.first()?;
    let one = |r: &RegionT| r.result == RegionResult::Collect && r.iteration == Iteration::Zip && r.skip.is_empty() && r.outputs.len() == 1 && r.dynamic == first.dynamic;
    let same_count = |r: &RegionT| {
        (first.count.is_some() && r.count == first.count)
            || (first.origin.is_some() && r.origin == first.origin)
            // Rewritten captures have new residual IDs. Their existing structural/declaration keys
            // prove the same immutable inputs, hence the same Zip minimum, without forcing.
            || (!first.sources.is_empty() && first.sources.len() == r.sources.len() && first.sources.iter().zip(r.sources.iter()).all(|(a, b)| same_source(a, b)))
    };
    if !regions.iter().all(|r| one(r) && same_count(r)) { return None; }
    let attempt = || -> Res<RegionT> {
        let mut b = Builder::new();
        let mut outputs = Vec::new();
        for r in regions {
            let mut inputs = Vec::new();
            for (i, source) in r.sources.iter().enumerate() { inputs.push(b.append_source(source.clone(), r.widths[i])); }
            let uniform = b.append_uniforms(&r.uniforms);
            let out = b.append_code(r, &mut |b: &mut Builder, i: i64, width: i64, component: i64| {
                b.emit(source_input(&r.sources[i as usize].1, inputs[i as usize], width, component))
            }, uniform)?;
            outputs.push(out[0].clone());
        }
        let (sources, widths, uniforms, code) = b.finish();
        R::rebuild(first, R::Rebuild {
            sources: Some(sources), widths: Some(widths), uniforms: Some(uniforms), code: Some(code), outputs: Some(outputs),
            stages: Some(regions.iter().flat_map(|r| r.stages.iter().cloned()).collect()),
            sites: Some(regions.iter().flat_map(|r| r.sites.iter().cloned()).collect()),
            ..Default::default()
        })
        .map_err(|_| Unsupported)
    };
    attempt().ok()
}

/// A device region binds what varies. A source that fills one constant (`array/int n 1`, a static
/// vector) becomes that constant, and sources that read the same attribute (`sop/attr g :P` bound
/// twice) are one upload.
fn fill(source: &Source, width: i64) -> Option<Vec<I>> {
    if let Resolved::Term(residual, term) = resolved(source.0, &source.1) {
        if let Node::Op(id) = term.node {
            let arena = term.arena();
            let op = arena.op(id);
            let value: Option<Value> = arena.fields(op.args).iter().find(|field| term.name(field.name) == "value").and_then(|field| {
                match arena.row(field.term).node {
                    Node::Lit(id) => match arena.literal(id) {
                        flow_check::term_arena::Literal::Int(n) => Some(Value::Int(n)),
                        flow_check::term_arena::Literal::Float(x) => Some(Value::Float(x)),
                        _ => None,
                    },
                    Node::RefBinding(id) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => match resolved(residual, &term.child(field.term)) {
                        Resolved::Value(v) => Some(v),
                        Resolved::Term(..) => None,
                    },
                    _ => None,
                }
            });
            return match (term.name(op.op), value, width) {
                ("array/int", Some(Value::Int(n)), 1) => Some(vec![I::Int_const(n)]),
                ("array/float", Some(Value::Float(x)), 1) => Some(vec![I::Const(x)]),
                ("array/float", Some(Value::Int(n)), 1) => Some(vec![I::Const(n as f64)]),
                ("array/vec3", Some(Value::Vec3(x, y, z)), 3) => Some(vec![I::Const(x), I::Const(y), I::Const(z)]),
                ("array/vec4", Some(Value::Vec4(x, y, z, w)), 4) => Some(vec![I::Const(x), I::Const(y), I::Const(z), I::Const(w)]),
                _ => None,
            };
        }
    }
    None
}

fn attribute_read(source: &Source) -> Option<Vec<u8>> {
    match resolved(source.0, &source.1) {
        Resolved::Value(v @ (Value::Struct(..) | Value::Source(..))) => {
            let (name, fields) = match &v { Value::Struct(g) | Value::Source(g) => (&g.name, &g.fields), _ => return None };
            if name == "sop/attr" && fields.iter().all(|(_, f)| !matches!(f, Value::Residual(_))) {
                Some(V::key_of(&|id| id.0, &v))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// For a device: a source that fills one constant becomes that constant; identical immutable
/// sources and sources reading the same attribute become one.
pub fn compact_sources(t: RegionT) -> RegionT {
    let n = t.sources.len();
    let fills: Vec<Option<Vec<I>>> = t.sources.iter().enumerate().map(|(i, s)| fill(s, t.widths[i])).collect();
    let reads: Vec<Option<Vec<u8>>> = t.sources.iter().map(attribute_read).collect();
    let canonical: Vec<usize> = (0..n)
        .map(|i| {
            if fills[i].is_some() { return i; }
            for j in 0..i {
                if fills[j].is_none()
                    && ((reads[i].is_some() && reads[j] == reads[i]) || same_source(&t.sources[j], &t.sources[i]))
                    && t.widths[j] == t.widths[i]
                {
                    return j;
                }
            }
            i
        })
        .collect();
    let changed = fills.iter().any(|f| f.is_some()) || canonical.iter().enumerate().any(|(i, c)| *c != i);
    if t.iteration != Iteration::Zip || !changed || fills.iter().all(|f| f.is_some()) { return t; }
    let attempt = || -> Res<RegionT> {
        let mut b = Builder::new();
        let mut slots = vec![-1_i64; n];
        for (i, source) in t.sources.iter().enumerate() {
            if fills[i].is_none() && canonical[i] == i { slots[i] = b.append_source(source.clone(), t.widths[i]); }
        }
        let mut registers: Vec<Vec<i64>> = Vec::new();
        for i in 0..n {
            match &fills[i] {
                Some(constants) => {
                    let mut out = Vec::new();
                    for c in constants { out.push(b.emit(c.clone())?); }
                    registers.push(out);
                }
                None => {
                    let j = canonical[i];
                    let mut out = Vec::new();
                    for component in 0..t.widths[i] {
                        out.push(b.emit(source_input(&t.sources[j].1, slots[j], t.widths[j], component))?);
                    }
                    registers.push(out);
                }
            }
        }
        let uniform = b.append_uniforms(&t.uniforms);
        let outputs = b.append_code(&t, &mut |_b: &mut Builder, i: i64, _w: i64, component: i64| Ok(registers[i as usize][component as usize]), uniform)?;
        let (sources, widths, uniforms, code) = b.finish();
        R::rebuild(&t, R::Rebuild { sources: Some(sources), widths: Some(widths), uniforms: Some(uniforms), code: Some(code), outputs: Some(outputs), ..Default::default() })
            .map_err(|_| Unsupported)
    };
    match attempt() { Ok(compact) => compact, Err(_) => t }
}
