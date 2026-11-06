//! The device side of scene/trace (port of lib/flow_shader/trace.ml). The region keeps a scene/trace
//! as its host fields (Trace), which the CPU answers through the ray host; a traced kernel instead
//! inlines trace.rays' trace-hit (trace-occluded for a ray whose only read field is hit), compiled once
//! a scene variant, in place of those fields.
use crate::lisp_stage;
use flow_ir::core_ir::Region::{self, Instruction, LiveFloat, Rebuild};
use flow_ir::kernel_layout;
use flow_ir::packed;
use flow_syntax::value;
use rays_diagnostic::Diagnostic;
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

pub const SOURCE: &str = include_str!("../trace.rays");

/// OCaml's polymorphic variant ``[ `Loop | `Native ]``.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Curves {
    Loop,
    Native,
}

/// Each new register's original slot (-1 for a helper's); the identity when nothing was spliced.
#[derive(Clone, Debug)]
pub struct Back(Option<Vec<i64>>);

impl Back {
    pub fn identity() -> Back {
        Back(None)
    }
    pub fn from_vec(map: Vec<i64>) -> Back {
        Back(Some(map))
    }
    pub fn is_identity(&self) -> bool {
        self.0.is_none()
    }
    pub fn at(&self, slot: i64) -> i64 {
        match &self.0 {
            None => slot,
            Some(map) => map[slot as usize],
        }
    }
}

pub fn flat(instruction: &Instruction) -> (Vec<(i64, bool)>, &Instruction) {
    match instruction {
        Instruction::Guarded(more, body) => {
            let (mut inner, i) = flat(body);
            let mut guards = more.clone();
            guards.append(&mut inner);
            (guards, i)
        }
        body => (vec![], body),
    }
}

pub fn wrap(guards: Vec<(i64, bool)>, instruction: Instruction) -> Instruction {
    if guards.is_empty() {
        instruction
    } else {
        Instruction::Guarded(guards, Box::new(instruction))
    }
}

fn fail<X>(message: &str) -> Result<X, Diagnostic> {
    Err(value::fail("E_GPU_FORM", message))
}

/// An instruction with every register operand through `f`.
pub fn remap(f: &dyn Fn(i64) -> i64, instruction: &Instruction) -> Instruction {
    use Instruction::*;
    let all = |r: &Vec<i64>| r.iter().map(|x| f(*x)).collect::<Vec<i64>>();
    match instruction {
        Binary(op, a, b) => Binary(*op, f(*a), f(*b)),
        Unary(op, a) => Unary(*op, f(*a)),
        Int_binary(op, a, b) => Int_binary(*op, f(*a), f(*b)),
        Int_compare(op, a, b) => Int_compare(*op, f(*a), f(*b)),
        Int_abs(a) => Int_abs(f(*a)),
        Convert(op, a) => Convert(op.clone(), f(*a)),
        Gather(a, i, w, c) => Gather(f(*a), f(*i), *w, *c),
        Int_gather(a, i) => Int_gather(f(*a), f(*i)),
        Text_gather(a, i) => Text_gather(f(*a), f(*i)),
        Text_select(c, a, b) => Text_select(f(*c), f(*a), f(*b)),
        Noise3(a, b, c, seed, octaves, lacunarity, gain) => {
            let live_int = |x: &Region::LiveInt| match x {
                Region::LiveInt::Fixed(v) => Region::LiveInt::Fixed(*v),
                Region::LiveInt::Live(r) => Region::LiveInt::Live(f(*r)),
            };
            let live_float = |x: &LiveFloat| match x {
                LiveFloat::Fixed(v) => LiveFloat::Fixed(*v),
                LiveFloat::Live(r) => LiveFloat::Live(f(*r)),
            };
            Noise3(f(*a), f(*b), f(*c), *seed, live_int(octaves), live_float(lacunarity), live_float(gain))
        }
        Rand_at(seed, index) => Rand_at(f(*seed), f(*index)),
        Fma(a, b, c) => Fma(f(*a), f(*b), f(*c)),
        Checked_mul(p, a, b) => Checked_mul(*p, f(*a), f(*b)),
        Checked_fma(p, a, b, c) => Checked_fma(*p, f(*a), f(*b), f(*c)),
        Trace(field, r) => Trace(*field, all(r)),
        Stage(stage, r) => Stage(*stage, all(r)),
        Select(c, a, b) => Select(f(*c), f(*a), f(*b)),
        Int_select(c, a, b) => Int_select(f(*c), f(*a), f(*b)),
        Loop_begin(l) => Loop_begin(Region::Loop { first: f(l.first), last: f(l.last), initial: all(&l.initial), ..l.clone() }),
        Loop_carry(owner, component, ty) => Loop_carry(f(*owner), *component, ty.clone()),
        Loop_end(owner, next, stop) => Loop_end(f(*owner), all(next), stop.map(f)),
        Loop_result(owner, component, ty) => Loop_result(f(*owner), *component, ty.clone()),
        Guarded(guards, body) => Guarded(guards.iter().map(|(c, yes)| (f(*c), *yes)).collect(), Box::new(remap(f, body))),
        other => other.clone(),
    }
}

/// The spliced `code` (in order) as `program`'s region, each loop closed at its Loop_end, with `back`:
/// each new register's original slot (-1 for a helper's), and `map`: each original slot's register.
pub fn rebuild(program: &packed::T, mut code: Vec<Instruction>, back: Vec<i64>, map: &[i64]) -> Result<(packed::T, Back), Diagnostic> {
    fn close(i: &Instruction, slot: i64) -> Instruction {
        match i {
            Instruction::Loop_begin(l) => Instruction::Loop_begin(Region::Loop { finish: slot, ..l.clone() }),
            Instruction::Guarded(g, body) => Instruction::Guarded(g.clone(), Box::new(close(body, slot))),
            other => other.clone(),
        }
    }
    for slot in 0..code.len() {
        if let (_, Instruction::Loop_end(owner, _, _)) = flat(&code[slot]) {
            let owner = *owner as usize;
            code[owner] = close(&code[owner], slot as i64);
        }
    }
    let outputs: Vec<Vec<i64>> = program.outputs.iter().map(|o| o.iter().map(|x| map[*x as usize]).collect()).collect();
    match Region::rebuild(program, Rebuild { code: Some(code), outputs: Some(outputs), ..Rebuild::default() }) {
        Ok(expanded) => Ok((expanded, Back::from_vec(back))),
        Err(mut d) if !d.is_empty() => Err(d.remove(0)),
        Err(_) => fail("the kernel with its spliced helpers did not rebuild"),
    }
}

type Variant = Result<(packed::T, Vec<(i64, i64)>), Diagnostic>;
static VARIANTS: LazyLock<Mutex<HashMap<(bool, bool, i64, bool, bool), Variant>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

/// The helper of one scene variant: trace-hit's fields t, prim, u, v, normal, hit and instance (the
/// host's Trace fields 0 to 8), or trace-occluded's hit, as (output, component) of its program.
fn helper(instanced: bool, motion: bool, strands: i64, spheres: bool, occluded: bool) -> Variant {
    let mut variants = VARIANTS.lock().unwrap();
    if let Some(compiled) = variants.get(&(instanced, motion, strands, spheres, occluded)) {
        return compiled.clone();
    }
    let call = if occluded {
        format!("{{:hit (trace-occluded o d tmin tmax time {strands} {spheres})}}")
    } else {
        format!("(trace-hit o d tmin tmax time {instanced} {motion} {strands} {spheres})")
    };
    let declarations =
        format!("{SOURCE}(defn trace-entry :context value [(o : vec3) (d : vec3) (tmin : float) (tmax : float) (time : float)] {call})");
    let compiled = lisp_stage::compile(&[], &declarations, "trace-entry").and_then(|stage| {
        let program = stage.program.clone();
        let field = |name: &str| -> i64 {
            stage.leaves.iter().position(|leaf| leaf.path == [name.to_owned()]).unwrap_or_else(|| panic!("Trace.helper")) as i64
        };
        let fields: Vec<(i64, i64)> = if occluded {
            vec![(field("hit"), 0); 9]
        } else {
            (0..9)
                .map(|f| match f {
                    0 => (field("t"), 0),
                    1 => (field("prim"), 0),
                    2 => (field("u"), 0),
                    3 => (field("v"), 0),
                    4..=6 => (field("n"), f - 4),
                    7 => (field("hit"), 0),
                    _ => (field("inst"), 0),
                })
                .collect()
        };
        let reads_host = program.code.iter().any(|i| match flat(i).1 {
            Instruction::Uniform(..) | Instruction::Array_prepare(..) | Instruction::Frame(_) | Instruction::Int_uniform(_) => true,
            Instruction::Int_frame(name) => !kernel_layout::scene_fact(name),
            _ => false,
        });
        if reads_host {
            fail("trace.rays: the trace helpers read no uniforms or frame facts")
        } else {
            Ok((program, fields))
        }
    });
    variants.insert((instanced, motion, strands, spheres, occluded), compiled.clone());
    compiled
}

type RayKey = (i64, Vec<(i64, bool)>, Vec<i64>);

/// The program with each ray (its Trace fields of one guard set, in one loop) replaced by its helper,
/// spliced before the ray's first field, and `back`: the original slot of each new register (-1 for a
/// helper's), which names the host's frame values.
pub fn expand(instanced: bool, motion: bool, curves: Option<Curves>, spheres: bool, program: &packed::T) -> Result<(packed::T, Back), Diagnostic> {
    let code: Vec<Instruction> = (*program.code).clone();
    let n = code.len();
    if !code.iter().any(|i| matches!(flat(i).1, Instruction::Trace(..))) {
        return Ok((program.clone(), Back::identity()));
    }
    let used = kernel_layout::plan(
        kernel_layout::PlanOptions { fast: true, image: false, instanced, motion, curves: curves.is_some() },
        program,
    )
    .used;
    let strands = match curves {
        None => 0,
        Some(Curves::Loop) => 1,
        Some(Curves::Native) => 2,
    };
    // the innermost loop around each slot
    let mut scope = vec![-1i64; n];
    let mut stack: Vec<i64> = Vec::new();
    for (slot, i) in code.iter().enumerate() {
        scope[slot] = stack.last().copied().unwrap_or(-1);
        match flat(i).1 {
            Instruction::Loop_begin(_) => stack.push(slot as i64),
            Instruction::Loop_end(..) => {
                stack.pop();
            }
            _ => {}
        }
    }
    let mut rays: HashMap<RayKey, (usize, Vec<i64>)> = HashMap::new();
    for (slot, i) in code.iter().enumerate() {
        if let (guards, Instruction::Trace(field, r)) = flat(i) {
            let key = (scope[slot], guards, r.clone());
            let (first, mut fields) = rays.get(&key).cloned().unwrap_or((slot, vec![]));
            if used[slot] {
                fields.push(*field);
            }
            rays.insert(key, (first, fields));
        }
    }
    let mut starts: HashMap<usize, (RayKey, Vec<i64>)> = HashMap::new();
    for (key, (first, fields)) in &rays {
        starts.insert(*first, (key.clone(), fields.clone()));
    }
    let mut out: Vec<Instruction> = Vec::new();
    let mut back: Vec<i64> = Vec::new();
    let mut push = |old: i64, instruction: Instruction, out: &mut Vec<Instruction>| -> i64 {
        out.push(instruction);
        back.push(old);
        out.len() as i64 - 1
    };
    let mut map = vec![-1i64; n];
    // key -> (occluded, helper outputs, helper program outputs, helper registers)
    let mut field_of: HashMap<RayKey, (bool, Vec<(i64, i64)>, Vec<Vec<i64>>, Vec<i64>)> = HashMap::new();
    for slot in 0..n {
        if let Some((key, fields)) = starts.get(&slot) {
            let r = &key.2;
            let occluded = fields.iter().all(|f| *f == 7);
            let (helper_program, outputs) = helper(instanced, motion, strands, spheres, occluded)?;
            let guards: Vec<(i64, bool)> = key.1.iter().map(|(c, yes)| (map[*c as usize], *yes)).collect();
            let mut hm = vec![-1i64; helper_program.code.len()];
            // elements o, d, tmin, tmax, time are the trace's operands after its scene
            let element = |k: i64, c: i64, map: &[i64]| -> i64 {
                let at = (match k {
                    0 => 1,
                    1 => 4,
                    k => k + 5,
                }) + c;
                map[r[at as usize] as usize]
            };
            for (j, i) in helper_program.code.iter().enumerate() {
                match flat(i) {
                    (_, Instruction::Input(k, _, c)) => hm[j] = element(*k, *c, &map),
                    (inner, body) => {
                        let mut g = guards.clone();
                        g.extend(inner.iter().map(|(c, yes)| (hm[*c as usize], *yes)));
                        let hmc = hm.clone();
                        let wrapped = wrap(g, remap(&|x| hmc[x as usize], body));
                        hm[j] = push(-1, wrapped, &mut out);
                    }
                }
            }
            field_of.insert(key.clone(), (occluded, outputs, (*helper_program.outputs).clone(), hm));
        }
        match flat(&code[slot]) {
            (guards, Instruction::Trace(field, r)) => {
                let (occluded, outputs, program_outputs, hm) = field_of.get(&(scope[slot], guards, r.clone())).expect("ray without its helper");
                let (k, c) = outputs[if *occluded { 0 } else { *field as usize }];
                map[slot] = hm[program_outputs[k as usize][c as usize] as usize];
            }
            _ => {
                let mapc = map.clone();
                map[slot] = push(slot as i64, remap(&|x| mapc[x as usize], &code[slot]), &mut out);
            }
        }
    }
    rebuild(program, out, back, &map)
}

/// A frame fact's uniform word named by its slot in the host's program, before the trace helpers.
pub fn frames(back: &Back, uniform_layout: &[(String, i64, i64)]) -> Vec<(String, i64, i64)> {
    uniform_layout
        .iter()
        .map(|(name, offset, width)| {
            let parts: Vec<&str> = name.split(':').collect();
            match parts.as_slice() {
                ["frame", slot, fact] => (format!("frame:{}:{}", back.at(slot.parse::<i64>().expect("frame slot")), fact), *offset, *width),
                _ => (name.clone(), *offset, *width),
            }
        })
        .collect()
}
