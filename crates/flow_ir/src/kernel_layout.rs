//! Immutable layout data shared by the Metal and WGSL instruction printers (port of
//! lib/flow_ir/kernel_layout.ml): which registers a kernel uses, which Int registers print as
//! 32-bit words, the uniform words, buffer bindings and scene facts.
//!
//! `iter` takes a [`Visit`] object instead of two closures: the loop callback re-enters the
//! visitor for the loop body, which two borrowed closures could not do.
#![allow(non_snake_case, non_camel_case_types)]

use crate::core_ir::Region::{self, Instruction, Loop};
use crate::packed::{self, host};
use flow_syntax::packed_ops::{Binary, Comparison, Integer, Unary};
use flow_syntax::ty::Ty;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct T {
    pub view: host::View,
    pub used: Vec<bool>,
    /// per-instruction finite checks: a fast kernel checks only its inputs, and its outputs
    pub finite_checks: Vec<bool>,
    /// fast: the approximate tier guarantees valid outputs (specification/gpu-codegen.md)
    pub output_checks: bool,
    /// Int registers printed as 32-bit unsigned words, with the CPU's bits: each always lies in
    /// 0 .. 2^32 - 1 or is read only through its low 32 bits (`Loop_result` follows its carry)
    pub narrow: Vec<bool>,
    pub uniforms: Vec<i64>,
    pub frames: Vec<i64>,
    pub image_width: Option<i64>,
    pub image_height: Option<i64>,
    pub table_seeds: Vec<i64>,
    pub gathers: Vec<(i64, (i64, i64))>,
    pub gather_lengths: Vec<i64>,
    pub input_used: Vec<bool>,
    pub input_count: i64,
    pub output_count: i64,
    pub uniform_layout: Vec<(String, i64, i64)>,
    pub uniform_bytes: i64,
    pub uniform_binding: i64,
    pub status_binding: i64,
    pub table_binding: Option<i64>,
    pub gather_base: i64,
    pub count_base: i64,
    pub counts: Option<i64>,
    /// `scene/trace`: the acceleration structure's buffer index; the triangle vertices follow.
    pub scene_binding: Option<i64>,
    /// uniform word holding the procedural sphere count
    pub scene_spheres: Option<i64>,
    /// an instanced scene (a static fact): the instance table's buffer index, after the spheres
    pub scene_instances: Option<i64>,
    /// a moving scene (a static fact): the query's time selects instance transforms (instanced) or
    /// vertex keyframes (flat)
    pub scene_motion: bool,
    /// flat motion: the shutter-end triangle vertices' buffer index
    pub scene_vertices_end: Option<i64>,
    /// a stranded scene (a static fact): the strand segments' buffer index, after every other scene
    /// buffer
    pub scene_strands: Option<i64>,
    /// uniform word holding the strand segment count
    pub scene_strand_count: Option<i64>,
}

/// Optional arguments of [`plan`]: `fast` includes gather lengths and the count record; `image`
/// reserves the two dimensions used by WGSL image kernels and has no input bindings.
#[derive(Clone, Copy, Debug, Default)]
pub struct PlanOptions {
    pub fast: bool,
    pub image: bool,
    pub instanced: bool,
    pub motion: bool,
    pub curves: bool,
}

fn guarded(i: &Instruction) -> &Instruction {
    match i {
        Instruction::Guarded(_, body) => guarded(body),
        other => other,
    }
}

/// `gpu/scene-spheres` and `gpu/scene-strands`: built-in counts read from the scene's uniform
/// words, not frame facts.
pub fn scene_fact(name: &str) -> bool {
    name == "gpu/scene-spheres" || name == "gpu/scene-strands"
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Bound {
    Bot,
    Max(i64),
    Top,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Use {
    Exact,
    Low(usize),
    Carry(usize),
    Bits,
}

// Kernels (an exact one's Int registers are its spliced helpers') print an Int register as a 32-bit
// unsigned word when that keeps its observable value: it always lies in [0, 2^32) (`fits`), or every
// use reads only its low 32 bits (wrapping +, -, *, and, or, xor, the shifted operand of a left
// shift, a select, a carried next value). The low 32 bits of signed-63 wrapping arithmetic are the
// uint32 results, so CPU bits are kept. Both are greatest fixpoints over the loop-carried registers.
fn narrow_registers(view: &host::View, used: &[bool]) -> Vec<bool> {
    let code: Vec<&Instruction> = view.code.iter().map(guarded).collect();
    let size = code.len();
    let integer = |slot: usize| Region::integer_register(&view.code[slot]);
    let mut carried: Vec<Option<(i64, i64)>> = vec![None; size];
    let mut results: Vec<Vec<usize>> = vec![Vec::new(); size];
    for (slot, instruction) in code.iter().enumerate() {
        match instruction {
            Instruction::Loop_begin(loop_) => {
                let next = match code[loop_.finish as usize] {
                    Instruction::Loop_end(_, next, _) => next,
                    _ => unreachable!("loop without its end"),
                };
                for (c, initial) in loop_.initial.iter().enumerate() {
                    carried[slot + 1 + c] = Some((*initial, next[c]));
                }
            }
            Instruction::Loop_result(loop_, c, _) => results[(*loop_ + 1 + *c) as usize].push(slot),
            _ => {}
        }
    }
    // upper bounds of nonnegative Int registers: Bot not yet reached, Max m in [0, m]
    let limit: i64 = 0xffff_ffff;
    let mut bound = vec![Bound::Bot; size];
    let at_most = |m: i64| if 0 <= m && m <= limit { Bound::Max(m) } else { Bound::Top };
    let join = |a: Bound, b: Bound| match (a, b) {
        (Bound::Bot, x) | (x, Bound::Bot) => x,
        (Bound::Max(a), Bound::Max(b)) => Bound::Max(a.max(b)),
        _ => Bound::Top,
    };
    let mut header = vec![-1i64; size];
    for (slot, instruction) in code.iter().enumerate() {
        if let Instruction::Loop_begin(loop_) = instruction {
            for c in 0..loop_.initial.len() {
                header[slot + 1 + c] = slot as i64;
            }
        }
    }
    // a carried register's largest step per pass: the carry plus nonnegative bounded terms, or a
    // select of such
    fn step(code: &[&Instruction], bound: &[Bound], carry: i64, r: i64) -> Option<i64> {
        if r == carry {
            return Some(0);
        }
        match code[r as usize] {
            Instruction::Int_binary(Integer::Iadd, a, b) => {
                let first = (step(code, bound, carry, *a), bound[*b as usize]);
                let second = (step(code, bound, carry, *b), bound[*a as usize]);
                match (first, second) {
                    ((Some(d), Bound::Max(k)), _) => Some(d + k),
                    (_, (Some(d), Bound::Max(k))) => Some(d + k),
                    _ => None,
                }
            }
            Instruction::Int_select(_, yes, no) => match (step(code, bound, carry, *yes), step(code, bound, carry, *no)) {
                (Some(a), Some(b)) => Some(a.max(b)),
                _ => None,
            },
            _ => None,
        }
    }
    let both = |bound: &[Bound], f: &dyn Fn(i64, i64) -> Bound, a: i64, b: i64| -> Bound {
        match (bound[a as usize], bound[b as usize]) {
            (Bound::Bot, _) | (_, Bound::Bot) => Bound::Bot,
            (Bound::Max(a), Bound::Max(b)) => f(a, b),
            _ => Bound::Top,
        }
    };
    let rule = |bound: &[Bound], slot: usize| -> Bound {
        match code[slot] {
            Instruction::Int_const(n) => at_most(*n),
            Instruction::Int_binary(Integer::Iand, a, b) => match (bound[*a as usize], bound[*b as usize]) {
                (Bound::Bot, _) | (_, Bound::Bot) => Bound::Bot,
                (Bound::Max(a), Bound::Max(b)) => Bound::Max(a.min(b)),
                (Bound::Max(m), _) | (_, Bound::Max(m)) => Bound::Max(m),
                _ => Bound::Top,
            },
            Instruction::Int_binary(Integer::Ior | Integer::Ixor, a, b) => both(
                bound,
                &|a, b| {
                    let mut m = 0i64;
                    while m < a.max(b) {
                        m = 2 * m + 1;
                    }
                    at_most(m)
                },
                *a,
                *b,
            ),
            Instruction::Int_binary(Integer::Ishr, a, b) => match (bound[*a as usize], code[*b as usize]) {
                (Bound::Max(m), Instruction::Int_const(k)) if 0 <= *k && *k <= 62 => Bound::Max(((m as u64) >> *k as u32) as i64),
                (m @ (Bound::Max(_) | Bound::Bot), _) => m,
                (Bound::Top, _) => Bound::Top,
            },
            Instruction::Int_binary(Integer::Iadd, a, b) => both(bound, &|a, b| at_most(a + b), *a, *b),
            Instruction::Int_binary(Integer::Imul, a, b) => {
                both(bound, &|a, b| if b == 0 || a <= limit / b { at_most(a * b) } else { Bound::Top }, *a, *b)
            }
            Instruction::Int_binary(op @ (Integer::Idiv | Integer::Imod), a, b)
                if matches!(code[*b as usize], Instruction::Int_const(k) if *k > 0) =>
            {
                let k = match code[*b as usize] {
                    Instruction::Int_const(k) => *k,
                    _ => unreachable!("constant divisor"),
                };
                match bound[*a as usize] {
                    Bound::Max(m) => Bound::Max(if *op == Integer::Idiv { m / k } else { m.min(k - 1) }),
                    other => other,
                }
            }
            Instruction::Int_binary(Integer::Idiv | Integer::Imod, a, b) => both(bound, &|a, _| Bound::Max(a), *a, *b),
            Instruction::Int_select(_, yes, no) => join(bound[*yes as usize], bound[*no as usize]),
            Instruction::Loop_carry(..) => {
                let (initial, next) = carried[slot].expect("a carry has its loop");
                let loop_ = match code[header[slot] as usize] {
                    Instruction::Loop_begin(l) => l,
                    _ => unreachable!("carry without its loop"),
                };
                match (step(&code, bound, slot as i64, next), bound[initial as usize], bound[loop_.last as usize]) {
                    (Some(d), Bound::Max(i), Bound::Max(trips)) if d == 0 || trips <= limit / d => at_most(i + d * trips),
                    _ => join(bound[initial as usize], bound[next as usize]),
                }
            }
            Instruction::Loop_begin(Loop { first, last, .. }) => both(bound, &|_, last| Bound::Max(last), *first, *last),
            // a scene count, a primitive and an instance index are 32-bit words
            Instruction::Int_frame(name) if scene_fact(name) => Bound::Max(limit),
            Instruction::Stage(
                Region::Stage::Trace_triangles(1 | 5) | Region::Stage::Trace_curves(1) | Region::Stage::Group_scan(_),
                _,
            ) => Bound::Max(limit),
            Instruction::Stage(Region::Stage::Store, _) => Bound::Max(1),
            Instruction::Stage(Region::Stage::Noise_table(_), _) => Bound::Max(255),
            Instruction::Loop_result(loop_, c, _) => bound[(*loop_ + 1 + *c) as usize],
            _ => Bound::Top,
        }
    };
    let mut changed = true;
    let mut passes = 0;
    while changed {
        changed = false;
        passes += 1;
        for slot in 0..size {
            if integer(slot) {
                let mut next = rule(&bound, slot);
                // widen a carried bound that keeps growing
                if passes > 16 && next != bound[slot] && header[slot] >= 0 {
                    next = Bound::Top;
                }
                let joined = join(bound[slot], next);
                if joined != bound[slot] {
                    bound[slot] = joined;
                    changed = true;
                }
            }
        }
    }
    let fits: Vec<bool> = bound.iter().map(|b| !matches!(b, Bound::Top)).collect();
    let mut uses: Vec<Vec<Use>> = vec![Vec::new(); size];
    let add = |uses: &mut Vec<Vec<Use>>, user: Use, operand: i64| uses[operand as usize].push(user);
    for (slot, instruction) in code.iter().map(|i| *i).enumerate() {
        if !used[slot] {
            continue;
        }
        match instruction {
            Instruction::Loop_carry(..) => {
                let (initial, _) = carried[slot].expect("a carry has its loop");
                add(&mut uses, Use::Carry(slot), initial);
            }
            Instruction::Loop_begin(Loop { first, last, .. }) => {
                add(&mut uses, Use::Exact, *first);
                add(&mut uses, Use::Exact, *last);
            }
            Instruction::Loop_end(header, next, stop) => {
                for (c, value) in next.iter().enumerate() {
                    add(&mut uses, Use::Carry((*header + 1 + c as i64) as usize), *value);
                }
                if let Some(stop) = stop {
                    add(&mut uses, Use::Exact, *stop);
                }
            }
            Instruction::Loop_result(..) => {}
            Instruction::Int_binary(Integer::Iadd | Integer::Isub | Integer::Imul | Integer::Iand | Integer::Ior | Integer::Ixor, a, b) => {
                add(&mut uses, Use::Low(slot), *a);
                add(&mut uses, Use::Low(slot), *b);
            }
            Instruction::Int_binary(Integer::Ishl, a, b) => {
                add(&mut uses, Use::Low(slot), *a);
                add(&mut uses, Use::Exact, *b);
            }
            Instruction::Int_select(condition, yes, no) => {
                add(&mut uses, Use::Exact, *condition);
                add(&mut uses, Use::Low(slot), *yes);
                add(&mut uses, Use::Low(slot), *no);
            }
            // a device gather clamps int(index), the low 32 bits
            Instruction::Gather(table, index, _, _) | Instruction::Int_gather(table, index) => {
                add(&mut uses, Use::Exact, *table);
                add(&mut uses, Use::Bits, *index);
            }
            // a store's index and Int value, a group scan's operand and a noise table index are
            // uint words on every target
            Instruction::Stage(Region::Stage::Store | Region::Stage::Group_scan(_) | Region::Stage::Noise_table(_) | Region::Stage::Float_bits, r) => {
                for r in r {
                    add(&mut uses, Use::Bits, *r);
                }
            }
            instruction => {
                for r in Region::operands(instruction) {
                    add(&mut uses, Use::Exact, r);
                }
            }
        }
    }
    // a compute stage's result is discarded: its stores are its effect
    if !code.iter().any(|i| matches!(i, Instruction::Stage(Region::Stage::Store, _))) {
        for output in &view.outputs {
            for r in output {
                add(&mut uses, Use::Exact, *r);
            }
        }
    }
    for (slot, instruction) in view.code.iter().enumerate() {
        if matches!(instruction, Instruction::Guarded(..)) && used[slot] {
            fn conditions(i: &Instruction, out: &mut Vec<i64>) {
                if let Instruction::Guarded(gs, body) = i {
                    out.extend(gs.iter().map(|(c, _)| *c));
                    conditions(body, out);
                }
            }
            let mut cs = Vec::new();
            conditions(instruction, &mut cs);
            for c in cs {
                add(&mut uses, Use::Exact, c);
            }
        }
    }
    let mut narrow: Vec<bool> = (0..size)
        .map(|slot| {
            integer(slot)
                && match code[slot] {
                    Instruction::Int_const(_)
                    | Instruction::Int_binary(
                        Integer::Iadd | Integer::Isub | Integer::Imul | Integer::Iand | Integer::Ior | Integer::Ixor | Integer::Ishl,
                        _,
                        _,
                    )
                    | Instruction::Int_select(..)
                    | Instruction::Loop_carry(..)
                    | Instruction::Stage(..) => true,
                    Instruction::Int_frame(name) => scene_fact(name),
                    Instruction::Int_binary(Integer::Ishr, a, _) => fits[*a as usize],
                    Instruction::Int_binary(Integer::Idiv | Integer::Imod, a, b) => fits[*a as usize] && fits[*b as usize],
                    Instruction::Loop_begin(_) => fits[slot],
                    _ => false,
                }
        })
        .collect();
    let mut changed = true;
    while changed {
        changed = false;
        for slot in 0..size {
            if !narrow[slot] {
                continue;
            }
            let low = |narrow: &[bool], u: &Use| match u {
                Use::Low(user) | Use::Carry(user) => narrow[*user],
                Use::Bits => true,
                Use::Exact => false,
            };
            let keeps = fits[slot]
                || (uses[slot].iter().all(|u| low(&narrow, u)) && results[slot].iter().all(|result| uses[*result].iter().all(|u| low(&narrow, u))));
            if !keeps {
                narrow[slot] = false;
                changed = true;
            }
        }
    }
    code
        .iter()
        .enumerate()
        .map(|(slot, instruction)| match instruction {
            Instruction::Loop_result(loop_, c, _) => narrow[(*loop_ + 1 + *c) as usize],
            _ => narrow[slot],
        })
        .collect()
}

// Static specialization: a condition whose value is known at emission (a constant, a comparison or
// boolean of constants, a select on a known condition) decides its guards. Statically false guards
// drop their instructions, statically true conditions leave the guard lists, and a select on a
// known condition reads only its chosen value. A dropped instruction would never run, so no result
// or status changes: an untaken guarded instruction never executes on any target.
fn specialize(view: &host::View) -> (host::View, Vec<bool>) {
    let size = view.code.len();
    let mut known: Vec<Option<f64>> = vec![None; size];
    let flag = |b: bool| Some(if b { 1.0 } else { 0.0 });
    for (slot, instruction) in view.code.iter().enumerate() {
        let both = |known: &[Option<f64>], a: i64, b: i64, f: &dyn Fn(f64, f64) -> Option<f64>| match (known[a as usize], known[b as usize]) {
            (Some(x), Some(y)) => f(x, y),
            _ => None,
        };
        known[slot] = match guarded(instruction) {
            Instruction::Const(c) => Some(*c),
            Instruction::Int_const(n) if n.abs() < 1 << 53 => Some(*n as f64),
            Instruction::Int_compare(op, a, b) => both(&known, *a, *b, &|x, y| {
                flag(match op {
                    Comparison::Ilt => x < y,
                    Comparison::Ile => x <= y,
                    Comparison::Igt => x > y,
                    Comparison::Ige => x >= y,
                    Comparison::Ieq => x == y,
                })
            }),
            Instruction::Binary(op @ (Binary::Lt | Binary::Le | Binary::Gt | Binary::Ge | Binary::Eq), a, b) => both(&known, *a, *b, &|x, y| {
                flag(match op {
                    Binary::Lt => x < y,
                    Binary::Le => x <= y,
                    Binary::Gt => x > y,
                    Binary::Ge => x >= y,
                    _ => x == y,
                })
            }),
            Instruction::Binary(Binary::And, a, b) => match (known[*a as usize], known[*b as usize]) {
                (Some(x), _) if x == 0.0 => flag(false),
                (_, Some(y)) if y == 0.0 => flag(false),
                (Some(_), Some(_)) => flag(true),
                _ => None,
            },
            Instruction::Binary(Binary::Or, a, b) => match (known[*a as usize], known[*b as usize]) {
                (Some(x), _) if x != 0.0 => flag(true),
                (_, Some(y)) if y != 0.0 => flag(true),
                (Some(_), Some(_)) => flag(false),
                _ => None,
            },
            Instruction::Unary(Unary::Not, a) => known[*a as usize].map(|x| if x == 0.0 { 1.0 } else { 0.0 }),
            Instruction::Convert(flow_syntax::packed_ops::Conversion::ToFloat, a) => known[*a as usize],
            Instruction::Select(c, yes, no) | Instruction::Int_select(c, yes, no) => {
                known[*c as usize].and_then(|x| known[(if x != 0.0 { *yes } else { *no }) as usize])
            }
            _ => None,
        };
    }
    let live = |(c, selected): &(i64, bool)| match known[*c as usize] {
        Some(x) => (x != 0.0) == *selected,
        None => true,
    };
    let mut dead = vec![false; size];
    let mut empty: Vec<(usize, Instruction)> = Vec::new();
    // a loop of no passes keeps only its initial values: its end reads them back
    for (slot, instruction) in view.code.iter().enumerate() {
        if let Instruction::Loop_begin(loop_) = guarded(instruction) {
            if let (Some(first), Some(last)) = (known[loop_.first as usize], known[loop_.last as usize]) {
                if first >= last {
                    for body in slot + 1 + loop_.initial.len()..loop_.finish as usize {
                        dead[body] = true;
                    }
                    empty.push((
                        loop_.finish as usize,
                        Instruction::Loop_end(slot as i64, (0..loop_.initial.len()).map(|c| (slot + 1 + c) as i64).collect(), None),
                    ));
                }
            }
        }
    }
    let mut code: Vec<Instruction> = Vec::with_capacity(size);
    for (slot, instruction) in view.code.iter().enumerate() {
        fn split(guards: &mut Vec<(i64, bool)>, i: &Instruction) -> Instruction {
            match i {
                Instruction::Guarded(more, body) => {
                    guards.extend(more.iter().copied());
                    split(guards, body)
                }
                body => body.clone(),
            }
        }
        let mut guards = Vec::new();
        let body = split(&mut guards, instruction);
        if !guards.iter().all(&live) {
            dead[slot] = true;
        }
        let guards: Vec<(i64, bool)> = guards.into_iter().filter(|(c, _)| known[*c as usize].is_none()).collect();
        let body = match body {
            Instruction::Select(c, yes, no) if known[c as usize].is_some() => {
                let pick = if known[c as usize].unwrap() != 0.0 { yes } else { no };
                Instruction::Select(c, pick, pick)
            }
            Instruction::Int_select(c, yes, no) if known[c as usize].is_some() => {
                let pick = if known[c as usize].unwrap() != 0.0 { yes } else { no };
                Instruction::Int_select(c, pick, pick)
            }
            body => body,
        };
        code.push(if guards.is_empty() { body } else { Instruction::Guarded(guards, Box::new(body)) });
    }
    for (finish, ending) in empty {
        code[finish] = match &code[finish] {
            Instruction::Guarded(guards, _) => Instruction::Guarded(guards.clone(), Box::new(ending)),
            _ => ending,
        };
    }
    (host::View { code, ..view.clone() }, dead)
}

// Registers an output reads, through the operands of the specialized code; `whole_loops` keeps every
// instruction of a reached loop body (for per-operation finite checks). A select on a known
// condition reads only its chosen value (`specialize`).
fn operands(i: &Instruction) -> Vec<i64> {
    match i {
        Instruction::Guarded(guards, body) => {
            let mut v: Vec<i64> = guards.iter().map(|(c, _)| *c).collect();
            v.extend(operands(body));
            v
        }
        Instruction::Select(_, a, b) | Instruction::Int_select(_, a, b) if a == b => vec![*a],
        instruction => Region::operands(instruction),
    }
}

fn reachable(whole_loops: bool, view: &host::View, dead: &[bool]) -> Vec<bool> {
    let mut used = vec![false; view.code.len()];
    fn mark(whole_loops: bool, view: &host::View, dead: &[bool], used: &mut Vec<bool>, slot: usize) {
        if used[slot] || dead[slot] {
            return;
        }
        used[slot] = true;
        for r in operands(&view.code[slot]) {
            mark(whole_loops, view, dead, used, r as usize);
        }
        if let Instruction::Loop_begin(loop_) = guarded(&view.code[slot]) {
            if !whole_loops {
                for carried in slot + 1..=slot + loop_.initial.len() {
                    mark(whole_loops, view, dead, used, carried);
                }
                mark(whole_loops, view, dead, used, loop_.finish as usize);
            } else {
                for body in slot + 1..=loop_.finish as usize {
                    mark(whole_loops, view, dead, used, body);
                }
            }
        }
    }
    for output in &view.outputs {
        for r in output {
            mark(whole_loops, view, dead, &mut used, *r as usize);
        }
    }
    used
}

pub fn plan(options: PlanOptions, program: &packed::T) -> T {
    let PlanOptions { fast, image, instanced, motion, curves } = options;
    let (view, dead) = specialize(&host::view(program));
    let mut offset: i64 = 4;
    let mut uniform_layout: Vec<(String, i64, i64)> = Vec::new();
    let mut uniform = |name: String, width: i64| -> i64 {
        let first = offset;
        offset = first + width * 4;
        uniform_layout.push((name, first, width));
        first / 4
    };
    let tabled: Vec<i64> = if fast {
        view.code.iter().filter_map(|i| match guarded(i) { Instruction::Array_prepare(u, _) => Some(*u), _ => None }).collect()
    } else {
        Vec::new()
    };
    let uniforms: Vec<i64> = view
        .uniform_widths
        .iter()
        .enumerate()
        .map(|(index, width)| if tabled.contains(&(index as i64)) { 0 } else { uniform(format!("uniform:{index}"), *width as i64) })
        .collect();
    let frames: Vec<i64> = view
        .code
        .iter()
        .enumerate()
        .map(|(index, instruction)| match guarded(instruction) {
            Instruction::Int_frame(name) if scene_fact(name) => 0,
            Instruction::Frame(name) | Instruction::Int_frame(name) => uniform(format!("frame:{index}:{name}"), 1),
            _ => 0,
        })
        .collect();
    let image_width = if image { Some(uniform("image:width".to_owned(), 1)) } else { None };
    let image_height = if image { Some(uniform("image:height".to_owned(), 1)) } else { None };
    // fast kernels check only their outputs (specification/gpu-codegen.md): dead loop code is not kept
    let used = reachable(!fast, &view, &dead);
    // fast checks its inputs (a resident producer's buffer is not scanned on the host) and outputs
    let finite_checks = if fast {
        view.code.iter().map(|i| matches!(guarded(i), Instruction::Input(..))).collect()
    } else {
        packed::finite_checks(program)
    };
    let mut table_seeds: Vec<i64> = view
        .code
        .iter()
        .enumerate()
        .filter_map(|(index, instruction)| match guarded(instruction) {
            Instruction::Noise3(_, _, _, seed, _, _, _) | Instruction::Stage(Region::Stage::Noise_table(seed), _) if used[index] => Some(*seed),
            _ => None,
        })
        .collect();
    table_seeds.sort_unstable();
    table_seeds.dedup();
    let gathers: Vec<(i64, (i64, i64))> = if !fast {
        Vec::new()
    } else {
        view.code
            .iter()
            .enumerate()
            .filter_map(|(slot, instruction)| match guarded(instruction) {
                Instruction::Array_prepare(u, ty) if used[slot] => Some((
                    slot as i64,
                    (
                        *u,
                        match ty {
                            Ty::Array(inner) if **inner == Ty::Vec2 => 2,
                            Ty::Array(inner) if **inner == Ty::Vec3 => 3,
                            Ty::Array(inner) if **inner == Ty::Vec4 => 4,
                            _ => 1,
                        },
                    ),
                )),
                _ => None,
            })
            .collect()
    };
    let gather_lengths: Vec<i64> = gathers.iter().enumerate().map(|(k, (_, (u, _)))| uniform(format!("gather:{k}:{u}"), 1)).collect();
    let traced = view.code.iter().any(|i| match guarded(i) {
        Instruction::Trace(..) => true,
        Instruction::Stage(
            Region::Stage::Trace_triangles(_)
            | Region::Stage::Trace_any
            | Region::Stage::Trace_curves(_)
            | Region::Stage::Trace_curves_any
            | Region::Stage::Scene_vertex(..)
            | Region::Stage::Scene_instance
            | Region::Stage::Scene_sphere(_)
            | Region::Stage::Scene_strand(_),
            _,
        ) => true,
        Instruction::Int_frame(name) => scene_fact(name),
        _ => false,
    });
    let scene_spheres = if traced { Some(uniform("scene:spheres".to_owned(), 1)) } else { None };
    let scene_strand_count = if traced && curves { Some(uniform("scene:strands".to_owned(), 1)) } else { None };
    let counts = if fast {
        let first = offset;
        offset = first + 12;
        Some(first)
    } else {
        None
    };
    let count = if image { 0 } else { view.widths.len() as i64 };
    let outputs = view.outputs.len() as i64;
    let uniform_binding = count + outputs;
    let status_binding = uniform_binding + 1;
    let table_binding = if table_seeds.is_empty() { None } else { Some(status_binding + 1) };
    let gather_base = count + outputs + 2 + if table_seeds.is_empty() { 0 } else { 1 };
    let count_base = gather_base + gathers.len() as i64;
    let scene = count_base + if counts.is_none() { 0 } else { 2 };
    let mut input_used = vec![false; count as usize];
    for (slot, instruction) in view.code.iter().enumerate() {
        if let Instruction::Input(input, _, _) | Instruction::Int_input(input) = guarded(instruction) {
            if used[slot] && !image {
                input_used[*input as usize] = true;
            }
        }
    }
    let narrow = narrow_registers(&view, &used);
    T {
        used,
        finite_checks,
        output_checks: fast,
        narrow,
        uniforms,
        frames,
        image_width,
        image_height,
        table_seeds,
        gathers,
        gather_lengths,
        input_used,
        input_count: count,
        output_count: outputs,
        uniform_layout,
        uniform_bytes: offset,
        uniform_binding,
        status_binding,
        table_binding,
        gather_base,
        count_base,
        counts,
        scene_spheres,
        scene_instances: if traced && instanced { Some(scene + 3) } else { None },
        scene_motion: traced && motion,
        scene_vertices_end: if traced && motion && !instanced { Some(scene + 3) } else { None },
        scene_strands: if traced && curves { Some(scene + 3 + if instanced || motion { 1 } else { 0 }) } else { None },
        scene_strand_count,
        scene_binding: if traced { Some(scene) } else { None },
        view,
    }
}

pub fn uniform_words(plan: &T) -> i64 {
    (plan.uniform_bytes + 15) / 16
}

/// Visit validated control scopes in authored order. The loop printer owns its carried
/// declarations, simultaneous update and optional early exit; `body` visits only its body (with the
/// visitor it is given).
pub trait Visit {
    fn instruction(&mut self, slot: usize, instruction: &Instruction);
    fn loop_(&mut self, header: usize, control: &Loop, next: &[i64], stop: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit));
}

pub fn iter(plan: &T, visitor: &mut dyn Visit) {
    fn visit(code: &[Instruction], first: usize, last: usize, visitor: &mut dyn Visit) {
        let mut slot = first;
        while slot < last {
            match guarded(&code[slot]) {
                Instruction::Loop_begin(control) => {
                    let header = slot;
                    let (next, stop) = match guarded(&code[control.finish as usize]) {
                        Instruction::Loop_end(_, values, stop) => (values.clone(), *stop),
                        _ => unreachable!("loop without its end"),
                    };
                    let (body_first, finish) = (header + 1 + control.initial.len(), control.finish as usize);
                    visitor.loop_(header, control, &next, stop, &mut |v: &mut dyn Visit| visit(code, body_first, finish, v));
                    slot = control.finish as usize + 1;
                }
                _ => {
                    visitor.instruction(slot, &code[slot]);
                    slot += 1;
                }
            }
        }
    }
    visit(&plan.view.code, 0, plan.view.code.len(), visitor);
}

/// A printed scope (the kernel body or one loop body) of the text printers: its registers are
/// declared first (`decls`), then its code runs, where consecutive instructions under the same
/// guards share one nest of blocks (`opened`, outermost first) instead of testing their conditions
/// instruction by instruction.
pub mod Scope {
    use super::*;

    /// `blocks` numbers the open blocks of `opened` (from `next`, shared by a kernel's scopes);
    /// `outer` is the block stack around a loop body.
    #[derive(Clone, Debug)]
    pub struct T {
        pub id: i64,
        pub decls: String,
        pub code: String,
        pub opened: Vec<(i64, bool)>,
        pub blocks: Vec<i64>,
        pub outer: Vec<i64>,
        pub next: Arc<AtomicI64>,
    }

    pub fn create(outer: Option<Vec<i64>>, next: Option<Arc<AtomicI64>>, id: i64) -> T {
        T {
            id,
            decls: String::with_capacity(1024),
            code: String::with_capacity(4096),
            opened: Vec::new(),
            blocks: Vec::new(),
            outer: outer.unwrap_or_default(),
            next: next.unwrap_or_else(|| Arc::new(AtomicI64::new(0))),
        }
    }

    pub fn indent(scope: &T) -> String {
        " ".repeat(2 * (1 + scope.opened.len()))
    }

    /// The blocks around the scope's current position, outermost first: a local declared there is
    /// visible wherever the stack extends this one.
    pub fn stack(scope: &T) -> Vec<i64> {
        let mut v = scope.outer.clone();
        v.extend(scope.blocks.iter().copied());
        v
    }

    /// Close the blocks `guards` leave and open, with `opening condition selected`, the ones it adds.
    pub fn enter(scope: &mut T, opening: &dyn Fn(i64, bool) -> String, guards: &[(i64, bool)]) {
        let kept = scope.opened.iter().zip(guards.iter()).take_while(|(x, y)| x == y).count();
        for depth in (kept + 1..=scope.opened.len()).rev() {
            scope.code.push_str(&" ".repeat(2 * depth));
            scope.code.push_str("}\n");
        }
        scope.blocks.truncate(kept);
        for (depth, (condition, selected)) in guards.iter().enumerate() {
            if depth >= kept {
                let n = scope.next.fetch_add(1, Ordering::Relaxed) + 1;
                scope.blocks.push(n);
                scope.code.push_str(&" ".repeat(2 * (depth + 1)));
                scope.code.push_str(&opening(*condition, *selected));
                scope.code.push('\n');
            }
        }
        scope.opened = guards.to_vec();
    }
}
