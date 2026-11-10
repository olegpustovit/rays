//! Process-global region snapshots have one test owner. REGION_SNAPSHOT_DIR writes crates/flow_ir/snapshots/<name>.txt.
use crate::core_ir::{quoted, region::{self as R, Instruction as I, LiveFloat, LiveInt, Projection}};
use crate::core_ir::RegionT;
use crate::packed;
use flow_syntax::packed_ops::{self, Binary, Conversion, Unary};
use flow_syntax::ty;
use flow_syntax::Diagnostic;
use std::collections::HashSet;
use std::fmt::Write;
use std::sync::{LazyLock, Mutex};

#[derive(Default)]
struct State {
    buffer: String,
    seen: HashSet<String>,
}
static STATE: LazyLock<Mutex<State>> = LazyLock::new(|| Mutex::new(State::default()));

fn binary_name(op: Binary) -> &'static str {
    match op {
        Binary::Add => "Add", Binary::Sub => "Sub", Binary::Mul => "Mul", Binary::Div => "Div", Binary::Mod => "Mod",
        Binary::Pow => "Pow", Binary::Min => "Min", Binary::Max => "Max", Binary::Lt => "Lt", Binary::Le => "Le",
        Binary::Gt => "Gt", Binary::Ge => "Ge", Binary::Eq => "Eq", Binary::And => "And", Binary::Or => "Or",
        Binary::Atan2 => "Atan2", Binary::Hypot => "Hypot", Binary::Ldexp => "Ldexp",
    }
}

fn unary_name(op: Unary) -> &'static str {
    match op {
        Unary::Sin => "Sin", Unary::Cos => "Cos", Unary::Sqrt => "Sqrt", Unary::Abs => "Abs", Unary::Not => "Not", Unary::Tan => "Tan",
        Unary::Asin => "Asin", Unary::Acos => "Acos", Unary::Exp => "Exp", Unary::Log => "Log", Unary::Sign => "Sign", Unary::Rsqrt => "Rsqrt",
    }
}

fn registers(r: &[i64]) -> String {
    r.iter().map(|x| format!(" {}", x)).collect()
}

fn projection(p: &Projection) -> &'static str {
    match p { Projection::Value => "value", Projection::Valid => "valid" }
}

/// One instruction as the snapshot files print it.
pub fn instruction(x: &I) -> String {
    match x {
        I::Const(x) => format!("Const {}", flow_syntax::lisp::float(*x)),
        I::Input(i, w, c) => format!("Input {} {} {}", i, w, c),
        I::Uniform(i, c) => format!("Uniform {} {}", i, c),
        I::Frame(name) => format!("Frame {}", quoted(name.as_bytes())),
        I::Accumulator(c) => format!("Accumulator {}", c),
        I::Binary(op, a, b) => format!("Binary {} {} {}", binary_name(*op), a, b),
        I::Unary(op, a) => format!("Unary {} {}", unary_name(*op), a),
        I::Noise3(x, y, z, seed, octaves, lacunarity, gain) => {
            let fractal = |v: &LiveFloat| match v { LiveFloat::Fixed(v) => flow_syntax::lisp::float(*v), LiveFloat::Live(r) => format!("r{}", r) };
            let octaves = match octaves { LiveInt::Fixed(n) => n.to_string(), LiveInt::Live(r) => format!("r{}", r) };
            let extra = if *lacunarity == LiveFloat::Fixed(2.0) && *gain == LiveFloat::Fixed(0.5) {
                String::new()
            } else {
                format!(" lacunarity={} gain={}", fractal(lacunarity), fractal(gain))
            };
            format!("Noise3 {} {} {} seed={} octaves={}{}", x, y, z, seed, octaves, extra)
        }
        I::Stage(_, r) => format!("Stage{}", registers(r)),
        I::Trace(field, r) => format!("Trace {}{}", field, registers(r)),
        I::Fma(a, b, c) => format!("Fma {} {} {}", a, b, c),
        I::Checked_mul(p, a, b) => format!("Checked_mul {} {} {}", projection(p), a, b),
        I::Array_prepare(i, ty) => format!("Array_prepare {} {}", i, ty::to_string(ty)),
        I::Gather(a, i, w, c) => format!("Gather {} {} {} {}", a, i, w, c),
        I::Int_gather(a, i) => format!("Int_gather {} {}", a, i),
        I::Text_const(s) => format!("Text_const {}", quoted(s)),
        I::Text_input(i) => format!("Text_input {}", i),
        I::Text_uniform(i) => format!("Text_uniform {}", i),
        I::Text_gather(a, i) => format!("Text_gather {} {}", a, i),
        I::Text_select(c, a, b) => format!("Text_select {} {} {}", c, a, b),
        I::Checked_fma(p, a, b, c) => format!("Checked_fma {} {} {} {}", projection(p), a, b, c),
        I::Rand_at(seed, index) => format!("Rand_at {} {}", seed, index),
        I::Select(c, a, b) => format!("Select {} {} {}", c, a, b),
        I::Int_const(n) => format!("Int_const {}", n),
        I::Int_input(n) => format!("Int_input {}", n),
        I::Int_uniform(n) => format!("Int_uniform {}", n),
        I::Int_accumulator => "Int_accumulator".to_owned(),
        I::Int_frame(name) => format!("Int_frame {}", quoted(name.as_bytes())),
        I::Int_binary(op, a, b) => format!("Int_binary {} {} {}", packed_ops::integer_name(*op), a, b),
        I::Int_compare(op, a, b) => format!("Int_compare {} {} {}", packed_ops::comparison_name(*op), a, b),
        I::Int_abs(a) => format!("Int_abs {}", a),
        I::Int_select(c, a, b) => format!("Int_select {} {} {}", c, a, b),
        I::Convert(op, a) => {
            let name = match op {
                Conversion::ToFloat => "float".to_owned(),
                Conversion::Truncate => "int".to_owned(),
                Conversion::Floor => "floor".to_owned(),
                Conversion::Ceil => "ceil".to_owned(),
                Conversion::Round => "round".to_owned(),
                Conversion::CoerceInt => "coerce:int".to_owned(),
                Conversion::IntegerResult(n) => format!("int-result:{}", n),
            };
            format!("Convert {} {}", name, a)
        }
        I::Guarded(guards, body) => format!(
            "Guarded {} ({})",
            guards.iter().map(|(r, b)| format!("{}:{}", r, b)).collect::<Vec<_>>().join(";"),
            instruction(body)
        ),
        I::Raise(t, code, message, cause) => format!(
            "Raise {} {} {}{}",
            ty::to_string(t),
            quoted(code.as_bytes()),
            quoted(message.as_bytes()),
            cause.as_ref().map(|c| format!(" cause={}", quoted(c))).unwrap_or_default()
        ),
        I::Loop_begin(l) => format!("Loop_begin {} {} end={}", l.first, l.last, l.finish),
        I::Loop_carry(l, c, t) => format!("Loop_carry {} {} {}", l, c, ty::to_string(t)),
        I::Loop_end(l, _, stop) => format!("Loop_end {}{}", l, stop.map(|s| format!(" until={}", s)).unwrap_or_default()),
        I::Loop_result(l, c, t) => format!("Loop_result {} {} {}", l, c, ty::to_string(t)),
    }
}

fn ints(a: &[i64]) -> String {
    format!("[{}]", a.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(";"))
}

/// The `Packed.Host.view` widths of the uniforms: the highest component read, at least 1.
fn uniform_widths(t: &RegionT) -> Vec<i64> {
    fn visit(i: &I, widths: &mut [i64]) {
        match i {
            I::Uniform(index, component) => {
                let slot = &mut widths[*index as usize];
                *slot = (*slot).max(component + 1);
            }
            I::Guarded(_, body) => visit(body, widths),
            _ => {}
        }
    }
    let mut widths = vec![1_i64; t.uniforms.len()];
    for i in t.code.iter() { visit(i, &mut widths); }
    widths
}

/// The snapshot text of one compile result.
pub fn entry(result: &Result<RegionT, Vec<Diagnostic>>) -> String {
    let mut entry = String::new();
    match result {
        Err(ds) => {
            for d in ds {
                let span = d.span.as_ref().map(|s| format!(" {}..{}", s.start, s.finish)).unwrap_or_default();
                let _ = writeln!(entry, "refused {}{} {}", d.code, span, quoted(d.message.as_bytes()));
            }
            entry.push('\n');
        }
        Ok(t) => {
            entry.push_str("program\n");
            for (i, x) in t.code.iter().enumerate() {
                let _ = writeln!(entry, "  r{} = {}", i, instruction(x));
            }
            let _ = write!(entry, "  widths {}\n", ints(&t.widths));
            for o in t.outputs.iter() {
                let _ = writeln!(entry, "  output {}", ints(o));
            }
            let _ = write!(entry, "  uniform_widths {}\n  collecting {}\n", ints(&uniform_widths(t)), t.result == R::RegionResult::Collect);
            for d in packed::gpu_refusals(false, false, t) {
                let _ = writeln!(entry, "  gpu_refusal {} {}", d.code, quoted(d.message.as_bytes()));
            }
            entry.push('\n');
        }
    }
    entry
}

/// A compile trace: append each distinct entry once.
pub fn record(result: &Result<RegionT, Vec<Diagnostic>>) {
    let text = entry(result);
    let mut state = STATE.lock().unwrap();
    if state.seen.insert(text.clone()) {
        state.buffer.push_str(&text);
    }
}

pub fn append(text: &str) {
    STATE.lock().unwrap().buffer.push_str(text);
}

pub fn start() {
    let mut state = STATE.lock().unwrap();
    state.buffer.clear();
    state.seen.clear();
}

pub fn contents() -> String {
    STATE.lock().unwrap().buffer.clone()
}

/// Compare the buffer with `crates/flow_ir/snapshots/<name>.txt`, or write it to
/// `REGION_SNAPSHOT_DIR`. Err names the first differing line.
pub fn finish(name: &str) -> Result<(), String> {
    let text = contents();
    if let Ok(dir) = std::env::var("REGION_SNAPSHOT_DIR") {
        return std::fs::write(std::path::Path::new(&dir).join(format!("{}.txt", name)), text).map_err(|e| e.to_string());
    }
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../crates/flow_ir/snapshots").join(format!("{}.txt", name));
    let expected = std::fs::read_to_string(&path).map_err(|e| format!("{}: {}", path.display(), e))?;
    if expected == text { return Ok(()); }
    let (a, b): (Vec<&str>, Vec<&str>) = (expected.split('\n').collect(), text.split('\n').collect());
    let mut n = 1;
    for (x, y) in a.iter().zip(b.iter()) {
        if x != y {
            return Err(format!("region snapshot {} differs: line {}: expected {:?}, got {:?}", name, n, x, y));
        }
        n += 1;
    }
    Err(format!("region snapshot {} differs: length differs at line {}", name, n))
}

/// Compile every owned region case with fusion on and off and compare exact register-program output with golden/region_cases.expected.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{core_fuse, core_ir::region::CountSource, core_region};
    use flow_check::{catalog::Catalog, workspace as W};
    use flow_eval::residual;
    use flow_syntax::value::Value;

    fn compile_case(body: &str, fusion: bool) -> Result<String, String> {
        let source = format!("(workspace w  (graph g :context value (let* [tested {}] 0.0)))", body);
        let forms = flow_syntax::syntax::parse(&source).map_err(|d| rays_diagnostic::to_string(&d))?;
        let catalog = Catalog { version: 1, kinds: Vec::new() };
        let workspace = match W::check(&[], false, false, None, &catalog, &forms) {
            (Some(w), ds) if ds.is_empty() => w,
            (_, ds) => return Err(ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
        };
        let evaluated = flow_eval::r#static(&workspace, true, None).map_err(|d| rays_diagnostic::to_string(&d))?;
        let records = &evaluated.records.iter().find(|(p, _)| *p == ["g".to_owned(), "tested".to_owned()]).ok_or("no record")?.1;
        let Value::Residual(residual) = records[0].1.clone() else { return Err("expected live packed program".to_owned()) };
        let term = residual::residual_view(residual).map_err(|d| rays_diagnostic::to_string(&d))?.term;
        let result = core_region::lower(fusion, false, &CountSource::none(), residual, &term).map(|r| if fusion { core_fuse::fuse(false, r) } else { r });
        let mut text: String = entry(&result).lines().filter(|l| !l.is_empty() && !l.starts_with("  gpu_refusal")).map(|l| format!("{}\n", l)).collect();
        if result.is_err() {
            text.push_str("check-error\n");
        }
        Ok(text)
    }

    fn region_of(body: &str) -> RegionT {
        let source = format!("(workspace w  (graph g :context value (let* [tested {}] 0.0)))", body);
        let forms = flow_syntax::syntax::parse(&source).unwrap();
        let (workspace, ds) = W::check(&[], false, false, None, &Catalog { version: 1, kinds: Vec::new() }, &forms);
        assert!(ds.is_empty());
        let evaluated = flow_eval::r#static(&workspace.unwrap(), true, None).unwrap();
        let records = &evaluated.records.iter().find(|(p, _)| *p == ["g".to_owned(), "tested".to_owned()]).unwrap().1;
        let Value::Residual(residual) = records[0].1.clone() else { panic!("expected live packed program") };
        let term = residual::residual_view(residual).unwrap().term;
        core_fuse::fuse(false, core_region::lower(true, false, &CountSource::none(), residual, &term).unwrap())
    }

    /// test_core.ml: a packed region is the validated Core.Region itself.
    #[test]
    fn core_prints_a_region() {
        use crate::core_ir::{pp, Program};
        let region = region_of("(map (fn [x] (+ (* x 2) t)) (array/range 4))");
        assert!(region.dependent.get().is_none());
        assert_eq!(crate::core_ir::region::dependent_flags(&region).len(), region.code.len());
        assert!(region.dependent.get().is_some());
        let program = Program {
            constants: Vec::new(), locals: Vec::new(), body: Vec::new(), results: Vec::new(), state_slots: Vec::new(),
            regions: vec![std::sync::Arc::new(region)], call_slots: 0,
        };
        assert_eq!(
            pp(&program),
            "core\n  region[0] zip collect count=4 widths=[1] output=[4] uniforms=0 skip=[] stages=1\n    r0 = input[0].0/1\n    r1 = 2.0\n    r2 = *(r0 r1)\n    r3 = frame.t\n    r4 = +(r2 r3)\n  return \n"
        );
    }

    #[test]
    fn and_or_guard_only_a_costly_right_operand() {
        let guarded = |body: &str| region_of(body).code.iter().any(|i| matches!(i, R::Instruction::Guarded(..)));
        assert!(guarded("(map (fn [x] (and (> x 1.) (let* [y (* x t)] (< y 3.)))) (array/range 4))"));
        assert!(guarded("(map (fn [x] (or (> x 1.) (let* [y (* x t)] (< y 3.)))) (array/range 4))"));
        assert!(!guarded("(map (fn [x] (or (> x 1.) (< (* x t) 3.))) (array/range 4))"));
    }

    #[test]
    fn register_programs_match_owned_goldens() {
        let cases = include_str!("../golden/region_cases.txt");
        let mut actual = String::new();
        for (i, body) in cases.lines().enumerate() {
            if body.trim().is_empty() || body.starts_with('#') { continue; }
            for fusion in [true, false] {
                actual.push_str(&format!("## case {} fusion={}\n", i, fusion));
                match compile_case(body, fusion) {
                    Ok(text) => actual.push_str(&text),
                    Err(_) => actual.push_str("check-error\n"),
                }
            }
        }
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("golden/region_cases.expected");
        if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(&path, &actual).unwrap(); }
        assert_eq!(actual, std::fs::read_to_string(path).unwrap());
    }
}
