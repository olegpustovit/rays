//! The device side of noise3 (port of lib/flow_shader/noise.ml). A kernel inlines noise.rays'
//! noise-fbm, compiled once an octave count, at each Noise3, its permutation gathers becoming reads of
//! the seed's table entries (Region::Noise_table).
use crate::lisp_stage;
use crate::trace::{self, Back, Curves};
use flow_ir::core_ir::region::{Instruction, LiveFloat, LiveInt, Stage};
use flow_ir::kernel_layout;
use flow_ir::packed;
use rays_diagnostic::Diagnostic;
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};


pub const SOURCE: &str = include_str!("../noise.rays");

type Variant = Result<packed::T, Diagnostic>;
static VARIANTS: LazyLock<Mutex<HashMap<(Option<i64>, bool), Variant>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

/// The helper of an octave count, `fractal` taking its lacunarity and gain as inputs 3 and 4; a live
/// count (`None`) is input 5, its fold's bound.
fn helper(fractal: bool, octaves: Option<i64>) -> Variant {
    let mut variants = VARIANTS.lock().unwrap();
    if let Some(compiled) = variants.get(&(octaves, fractal)) {
        return compiled.clone();
    }
    let entry = match octaves {
        None => "(defn noise-entry :context value [(table : (array int)) (x : float) (y : float) (z : float) (l : float) (g : float) (o : int)] (noise-fbm-live table [x y z] o l g))".to_owned(),
        Some(octaves) => {
            if fractal {
                format!("(defn noise-entry :context value [(table : (array int)) (x : float) (y : float) (z : float) (l : float) (g : float)] (noise-fbm-fractal table [x y z] {octaves} l g))")
            } else {
                format!("(defn noise-entry :context value [(table : (array int)) (x : float) (y : float) (z : float)] (noise-fbm table [x y z] {octaves}))")
            }
        }
    };
    let compiled = lisp_stage::compile(&[], &format!("{SOURCE}{entry}"), "noise-entry").map(|stage| stage.program);
    variants.insert((octaves, fractal), compiled.clone());
    compiled
}

/// The program with each Noise3 replaced by its helper, and `back` as Trace.expand's but naming the
/// register that replaces a Noise3 by its slot.
pub fn expand(program: &packed::T) -> Result<(packed::T, Back), Diagnostic> {
    let code: Vec<Instruction> = (*program.code).clone();
    if !code.iter().any(|i| matches!(trace::flat(i).1, Instruction::Noise3(..))) {
        return Ok((program.clone(), Back::identity()));
    }
    let mut out: Vec<Instruction> = Vec::new();
    let mut back: Vec<i64> = Vec::new();
    fn push(out: &mut Vec<Instruction>, back: &mut Vec<i64>, old: i64, instruction: Instruction) -> i64 {
        out.push(instruction);
        back.push(old);
        out.len() as i64 - 1
    }
    let mut map = vec![-1i64; code.len()];
    let mut results: Vec<usize> = Vec::new();
    for (slot, instruction) in code.iter().enumerate() {
        match trace::flat(instruction) {
            (guards, Instruction::Noise3(x, y, z, seed, octaves, lacunarity, gain)) => {
                let count = match octaves {
                    LiveInt::Fixed(n) => Some(*n),
                    LiveInt::Live(_) => None,
                };
                let fractal = count.is_none() || *lacunarity != LiveFloat::Fixed(2.0) || *gain != LiveFloat::Fixed(0.5);
                let helper = helper(fractal, count)?;
                let guards: Vec<(i64, bool)> = guards.iter().map(|(c, yes)| (map[*c as usize], *yes)).collect();
                let register = |v: &LiveFloat, out: &mut Vec<Instruction>, back: &mut Vec<i64>| match v {
                    LiveFloat::Fixed(v) => push(out, back, -1, Instruction::Const(*v)),
                    LiveFloat::Live(r) => map[*r as usize],
                };
                let mut operands = vec![map[*x as usize], map[*y as usize], map[*z as usize]];
                if fractal {
                    let l = register(lacunarity, &mut out, &mut back);
                    let g = register(gain, &mut out, &mut back);
                    operands.push(l);
                    operands.push(g);
                }
                if let LiveInt::Live(r) = octaves {
                    operands.push(map[*r as usize]);
                }
                let mut hm = vec![-1i64; helper.code.len()];
                for (j, i) in helper.code.iter().enumerate() {
                    match trace::flat(i) {
                        (_, Instruction::Input(k, _, _) | Instruction::Int_input(k)) => hm[j] = operands[*k as usize],
                        (_, Instruction::Array_prepare(..)) => {}
                        (inner, body) => {
                            let body = match body {
                                Instruction::Int_gather(_, index) => Instruction::Stage(Stage::Noise_table(*seed), vec![*index]),
                                body => body.clone(),
                            };
                            let mut g = guards.clone();
                            g.extend(inner.iter().map(|(c, yes)| (hm[*c as usize], *yes)));
                            let hmc = hm.clone();
                            let wrapped = trace::wrap(g, trace::remap(&|r| hmc[r as usize], &body));
                            hm[j] = push(&mut out, &mut back, -1, wrapped);
                        }
                    }
                }
                map[slot] = hm[helper.outputs[0][0] as usize];
                results.push(slot);
            }
            _ => {
                let mapc = map.clone();
                map[slot] = push(&mut out, &mut back, slot as i64, trace::remap(&|r| mapc[r as usize], instruction));
            }
        }
    }
    for slot in &results {
        back[map[*slot] as usize] = *slot as i64;
    }
    trace::rebuild(program, out, back, &map)
}

/// A kernel's scene/trace and noise3 helpers spliced, `back` naming each register's slot in `program`.
pub fn helpers(instanced: bool, motion: bool, curves: Option<Curves>, spheres: bool, program: &packed::T) -> Result<(packed::T, Back), Diagnostic> {
    let (program, traced) = trace::expand(instanced, motion, curves, spheres, program)?;
    let (program, noised) = expand(&program)?;
    let back = match (&traced, &noised) {
        _ if traced.is_identity() && noised.is_identity() => Back::identity(),
        _ => {
            let length = program.code.len() as i64;
            Back::from_vec(
                (0..length)
                    .map(|slot| {
                        let s = noised.at(slot);
                        if s < 0 {
                            -1
                        } else {
                            traced.at(s)
                        }
                    })
                    .collect(),
            )
        }
    };
    Ok((program, back))
}

/// An exact kernel checks every operation finite; a helper's need not be, its operands are checked at
/// their source and a nonfinite coordinate makes its result NaN, the register that replaces a Noise3.
pub fn checked(back: &Back, layout: kernel_layout::T) -> kernel_layout::T {
    let finite_checks = layout.finite_checks.iter().enumerate().map(|(slot, check)| *check && back.at(slot as i64) >= 0).collect();
    kernel_layout::T { finite_checks, ..layout }
}
