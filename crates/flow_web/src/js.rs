//! Print CPU packed regions and structured frame programs as JavaScript. Diagnostics are Results; frame options own Arc callbacks and printer contexts own their data.
use crate::text_literal;
use flow_syntax::symbol::Symbol;
use flow_check::workspace::DiagnosticPolicy;
use flow_eval as E;
use flow_ir::core_exec;
use flow_ir::core_ir::{self as C, Draw, Expr, FrameField, Iteration, Literal, Plane, Primitive, region, Statement, StatementNode};
use flow_ir::core_ir::region::{Instruction, Projection, RegionResult};
use flow_ir::core_lower;
use flow_ir::packed::{self, host};
use flow_syntax::packed_ops::{Binary, Comparison, Conversion, Integer, Unary};
use flow_syntax::ty::Ty;
use flow_syntax::value::{self, Value};
use flow_syntax::Diagnostic;
use std::collections::{BTreeMap, HashSet};
use std::fmt::Write;
use std::sync::Arc;

fn number(x: f64) -> String { param::float_text(x) }
fn register(index: i64) -> String { format!("r{index}") }
fn flag(condition: &str) -> String { format!("({condition} ? 1 : 0)") }
fn escape(s: &str) -> String { text_literal::quote(s.as_bytes()) }

fn error(code: &str, message: &str) -> Diagnostic { value::fail(code, message) }



fn binary(operation: Binary, a: &str, b: &str) -> String {
    flow_syntax::packed_ops::math_source(flow_syntax::packed_ops::math_prim(&flow_syntax::packed_ops::Primitive::Numeric(operation)).js, a, b)
}

fn unary(operation: Unary, a: &str) -> String {
    flow_syntax::packed_ops::math_source(flow_syntax::packed_ops::math_prim(&flow_syntax::packed_ops::Primitive::Math(operation)).js, a, "")
}

/// [fast] (the workspace's fast precision, a collecting region): integer registers are exact 64-bit
/// pairs (`FAST64`), as native fast precision's kernels hold them.
fn fast_less(a: i64, b: i64) -> String { format!("(r{a}h < r{b}h || (r{a}h === r{b}h && r{a}l < r{b}l))") }



fn guard_text(printer: &Printer, guards: &[(i64, bool)]) -> String {
    guards.iter().map(|(r, yes)| format!("{}{}", printer.numeric(*r), if *yes { " !== 0" } else { " === 0" })).collect::<Vec<_>>().join(" && ")
}

/// The state of one `region` print.
struct Printer {
    view: host::View,
    fast: bool,
    line: String,
    checks: Vec<bool>,
    nonfinite: &'static str,
}

impl Printer {
    fn numeric(&self, r: i64) -> String {
        if region::integer_register(&self.view.code[r as usize]) {
            if self.fast { format!("(r{r}h * 4294967296 + r{r}l)") } else { format!("Number({})", register(r)) }
        } else {
            register(r)
        }
    }

    fn index(&self, r: i64) -> String {
        if self.fast && region::integer_register(&self.view.code[r as usize]) { self.numeric(r) } else { register(r) }
    }

    fn expression(&self, slot: i64, instruction: &Instruction) -> Result<String, Diagnostic> {
        let fast = self.fast;
        Ok(match instruction {
            Instruction::Const(value) => number(*value),
            Instruction::LoopResult(loop_, component, _) => register(loop_ + 1 + component),
            Instruction::LoopBegin(_) | Instruction::LoopCarry(..) | Instruction::LoopEnd(..) => unreachable!("loop control is emitted by emit_range"),
            Instruction::Input(input, w, component) => format!("Number(in{input}[j{input} * {w} + {component}])"),
            Instruction::Uniform(index, component) => format!("Number(uniforms[{index}][{component}])"),
            Instruction::Frame(_) => format!("uniforms[{}][{slot}]", self.view.uniform_widths.len()),
            Instruction::Accumulator(component) => format!("st[{component}]"),
            Instruction::Binary(op, a, b) => binary(*op, &self.numeric(*a), &self.numeric(*b)),
            Instruction::Unary(op, a) => unary(*op, &self.numeric(*a)),
            Instruction::Select(c, yes, no) => format!("{} !== 0 ? {} : {}", self.numeric(*c), self.numeric(*yes), self.numeric(*no)),
            Instruction::Noise3(x, y, z, seed, octaves, lacunarity, gain) => {
                let fractal = |f: &region::LiveFloat| match f {
                    region::LiveFloat::Fixed(v) => number(*v),
                    region::LiveFloat::Live(r) => self.numeric(*r),
                };
                let octaves = match octaves {
                    region::LiveInt::Fixed(n) => format!("{n}n"),
                    region::LiveInt::Live(r) => {
                        if fast { format!("BigInt({})", self.numeric(*r)) } else { register(*r) }
                    }
                };
                format!(
                    "noise(Float64Array.of({}, {}, {}), {seed}n, {octaves}, {}, {})",
                    self.numeric(*x),
                    self.numeric(*y),
                    self.numeric(*z),
                    fractal(lacunarity),
                    fractal(gain)
                )
            }
            Instruction::Fma(a, b, c) => format!("fused({}, {}, {})", self.numeric(*a), self.numeric(*b), self.numeric(*c)),
            Instruction::CheckedMul(p, a, b) => format!("checked({}, {} * {})", *p == Projection::Valid, self.numeric(*a), self.numeric(*b)),
            Instruction::CheckedFma(p, a, b, c) => {
                format!("checked({}, fused({}, {}, {}))", *p == Projection::Valid, self.numeric(*a), self.numeric(*b), self.numeric(*c))
            }
            Instruction::ArrayPrepare(i, ty) => {
                if *ty == Ty::Array(Box::new(Ty::Text)) {
                    format!("table({i}, 'text', 1)")
                } else {
                    format!(
                        "table({i}, {}, {})",
                        *ty == Ty::Array(Box::new(Ty::Int)),
                        flow_syntax::packed_ops::array_width(ty).expect("array width")
                    )
                }
            }
            Instruction::Gather(array, i, width, component) => {
                format!("{}[row({}, {}, {width}) * {width} + {component}]", register(*array), register(*array), self.index(*i))
            }
            Instruction::IntGather(array, i) => format!("{}[row({}, {}, 1)]", register(*array), register(*array), self.index(*i)),
            Instruction::Trace(..) => {
                return Err(error("E_WEB_SOURCE", "scene/trace needs a native ray-query host; it is outside the 2D web target."))
            }
            Instruction::Stage(..) => return Err(error("E_WEB_SOURCE", "A raster-stage intrinsic runs only in a native fragment stage.")),
            Instruction::TextConst(value) => text_literal::quote(value),
            Instruction::TextInput(input) => format!("text(in{input}[j{input}])"),
            Instruction::TextUniform(input) => format!("text(uniforms[{input}])"),
            Instruction::TextGather(array, i) => format!("{}[row({}, {}, 1)]", register(*array), register(*array), self.index(*i)),
            Instruction::TextSelect(condition, yes, no) => format!("({} !== 0 ? {} : {})", self.numeric(*condition), register(*yes), register(*no)),
            Instruction::RandAt(seed, index) => format!("randAt({}, {})", register(*seed), register(*index)),
            Instruction::IntCompare(op, a, b) if fast => {
                let (a, b) = (*a, *b);
                let eq = format!("(r{a}h === r{b}h && r{a}l === r{b}l)");
                flag(&match op {
                    Comparison::Ilt => fast_less(a, b),
                    Comparison::Igt => fast_less(b, a),
                    Comparison::Ile => format!("!{}", fast_less(b, a)),
                    Comparison::Ige => format!("!{}", fast_less(a, b)),
                    Comparison::Ieq => eq,
                })
            }
            Instruction::IntConst(n) => format!("{n}n"),
            Instruction::IntInput(i) => format!("in{i}[j{i}]"),
            Instruction::IntUniform(i) => format!("uniforms[{i}][0]"),
            Instruction::IntAccumulator => "st[0]".to_owned(),
            Instruction::IntFrame(_) => format!("uniforms[{}][{slot}]", self.view.uniform_widths.len()),
            Instruction::IntBinary(op, a, b) => {
                let (a, b) = (register(*a), register(*b));
                match op {
                    Integer::Iadd => format!("wrap({a} + {b})"),
                    Integer::Isub => format!("wrap({a} - {b})"),
                    Integer::Imul => format!("wrap({a} * {b})"),
                    Integer::Idiv => format!("divide({a}, {b}, false)"),
                    Integer::Imod => format!("divide({a}, {b}, true)"),
                    Integer::Iand => format!("wrap({a} & {b})"),
                    Integer::Ior => format!("wrap({a} | {b})"),
                    Integer::Ixor => format!("wrap({a} ^ {b})"),
                    Integer::Ishl => format!("shift({a}, {b}, false)"),
                    Integer::Ishr => format!("shift({a}, {b}, true)"),
                }
            }
            Instruction::IntCompare(op, a, b) => flag(&format!(
                "{}{}{}",
                register(*a),
                match op {
                    Comparison::Ilt => " < ",
                    Comparison::Ile => " <= ",
                    Comparison::Igt => " > ",
                    Comparison::Ige => " >= ",
                    Comparison::Ieq => " === ",
                },
                register(*b)
            )),
            Instruction::IntAbs(a) => format!("{} < 0n ? wrap(-{}) : {}", register(*a), register(*a), register(*a)),
            Instruction::IntSelect(c, a, b) => format!("{} !== 0 ? {} : {}", self.numeric(*c), register(*a), register(*b)),
            Instruction::Convert(Conversion::ToFloat, a) => self.numeric(*a),
            Instruction::Convert(op, a) => {
                let (name, x) = match op {
                    Conversion::Truncate => ("int".to_owned(), format!("Math.trunc({})", self.numeric(*a))),
                    Conversion::Floor => ("floor".to_owned(), format!("Math.floor({})", self.numeric(*a))),
                    Conversion::Ceil => ("ceil".to_owned(), format!("Math.ceil({})", self.numeric(*a))),
                    Conversion::Round => {
                        let n = self.numeric(*a);
                        ("round".to_owned(), format!("Math.trunc({n}) + (Math.abs({n} - Math.trunc({n})) >= 0.5 ? Math.sign({n}) : 0)"))
                    }
                    Conversion::CoerceInt => ("int".to_owned(), format!("Math.floor({} + 0.5)", self.numeric(*a))),
                    Conversion::IntegerResult(n) => (n.clone(), format!("Math.floor({} + 0.5)", self.numeric(*a))),
                    Conversion::ToFloat => unreachable!("to_float is matched above"),
                };
                format!("convert({}, {x})", escape(&name))
            }
            Instruction::Raise(_, code, message, cause) => format!(
                "fail({}, {}{})",
                escape(code),
                escape(message),
                match cause {
                    Some(cause) => format!(", {}", crate::text_literal::quote(cause)),
                    None => String::new(),
                }
            ),
            Instruction::Guarded(_, body) => self.expression(slot, body)?,
        })
    }

    /// a fast integer register's statement: both halves of `r<slot>`
    fn pair(&self, slot: i64, body: &Instruction) -> Result<String, Diagnostic> {
        let h = |r: i64| format!("r{r}h");
        let l = |r: i64| format!("r{r}l");
        let lo = |e: &str| format!("r{slot}l = {e}; r{slot}h = RH;");
        let read = |v: &str| lo(&format!("(typeof ({v}) === 'bigint' ? unbig({v}) : split({v}))"));
        Ok(match body {
            Instruction::IntConst(n) => format!("r{slot}h = {}; r{slot}l = {};", (n >> 32) as i32, n & 0xffff_ffff),
            Instruction::IntInput(i) => read(&format!("in{i}[j{i}]")),
            Instruction::IntUniform(i) => read(&format!("uniforms[{i}][0]")),
            Instruction::IntFrame(_) => read(&format!("uniforms[{}][{slot}]", self.view.uniform_widths.len())),
            Instruction::IntGather(..) => read(&self.expression(slot, body)?),
            Instruction::IntBinary(op, a, b) => {
                let (a, b) = (*a, *b);
                let call = |f: &str| lo(&format!("{f}({}, {}, {}, {})", h(a), l(a), h(b), l(b)));
                let bits = |o: &str| format!("r{slot}h = {} {o} {}; r{slot}l = ({} {o} {}) >>> 0;", h(a), h(b), l(a), l(b));
                let head = match op {
                    Integer::Iadd => call("add64"),
                    Integer::Isub => call("sub64"),
                    Integer::Imul => call("mul64"),
                    Integer::Idiv => lo(&format!("div64({}, {}, {}, {}, false)", h(a), l(a), h(b), l(b))),
                    Integer::Imod => lo(&format!("div64({}, {}, {}, {}, true)", h(a), l(a), h(b), l(b))),
                    Integer::Iand => bits("&"),
                    Integer::Ior => bits("|"),
                    Integer::Ixor => bits("^"),
                    Integer::Ishl => lo(&format!("shift64({}, {}, {}, false)", h(a), l(a), self.numeric(b))),
                    Integer::Ishr => lo(&format!("shift64({}, {}, {}, true)", h(a), l(a), self.numeric(b))),
                };
                head
            }
            Instruction::IntAbs(a) => format!(
                "if ({} < 0) {{ r{slot}l = sub64(0, 0, {}, {}); r{slot}h = RH; }} else {{ r{slot}h = {}; r{slot}l = {}; }}",
                h(*a),
                h(*a),
                l(*a),
                h(*a),
                l(*a)
            ),
            Instruction::IntSelect(c, a, b) => format!(
                "if ({} !== 0) {{ r{slot}h = {}; r{slot}l = {}; }} else {{ r{slot}h = {}; r{slot}l = {}; }}",
                self.numeric(*c),
                h(*a),
                l(*a),
                h(*b),
                l(*b)
            ),
            Instruction::LoopResult(loop_, component, _) => {
                let source = loop_ + 1 + component;
                format!("r{slot}h = {}; r{slot}l = {};", h(source), l(source))
            }
            Instruction::Convert(_, _) => lo(&format!("split({})", self.expression(slot, body)?)),
            _ => return Err(error("INVALID_ARGUMENT", "fast integer register")),
        })
    }

    fn add(&mut self, text: &str) { self.line.push_str(text); }

    fn emit(&mut self, slot: i64, instruction: &Instruction) -> Result<(), Diagnostic> {
        let (guards, body) = Instruction::guarded(instruction);
        let guard = guard_text(self, &guards);
        if self.fast && region::integer_register(body) {
            let _ = writeln!(self.line, "    let r{slot}h = 0, r{slot}l = 0;");
            if !guards.is_empty() {
                let _ = writeln!(self.line, "    if ({guard}) {{");
            }
            let pair = self.pair(slot, body)?;
            let _ = writeln!(self.line, "    {pair}");
            if !guards.is_empty() {
                self.add("    }\n");
            }
        } else {
            let _ = writeln!(self.line, "    let r{slot};");
            if !guards.is_empty() {
                let _ = writeln!(self.line, "    if ({guard}) {{");
            }
            match body {
                Instruction::CheckedMul(_, a, b) => {
                    let _ = writeln!(self.line, "    if (!(Number.isFinite({}) && Number.isFinite({}))) return 1;", self.numeric(*a), self.numeric(*b));
                }
                Instruction::CheckedFma(_, a, b, c) => {
                    let _ = writeln!(
                        self.line,
                        "    if (!(Number.isFinite({}) && Number.isFinite({}) && Number.isFinite({}))) return 1;",
                        self.numeric(*a),
                        self.numeric(*b),
                        self.numeric(*c)
                    );
                }
                _ => {}
            }
            let expression = self.expression(slot, body)?;
            let _ = writeln!(self.line, "    r{slot} = {expression};");
            if !region::integer_register(body) && self.checks[slot as usize] {
                let _ = writeln!(self.line, "    if (!Number.isFinite(r{slot})) {}", self.nonfinite);
            }
            if !guards.is_empty() {
                self.add("    }\n");
            }
        }
        Ok(())
    }

    fn declare(&mut self, slot: i64) {
        if self.fast && region::integer_register(&self.view.code[slot as usize]) {
            let _ = writeln!(self.line, "    let r{slot}h = 0, r{slot}l = 0;");
        } else {
            let _ = writeln!(self.line, "    let r{slot};");
        }
    }

    fn assign(&mut self, target: i64, source: i64) {
        if self.fast && region::integer_register(&self.view.code[target as usize]) {
            let _ = writeln!(self.line, "    r{target}h = r{source}h; r{target}l = r{source}l;");
        } else {
            let _ = writeln!(self.line, "    r{target} = r{source};");
        }
    }

    fn exact(&self, slot: i64) -> String { if self.fast { format!("big(r{slot}h, r{slot}l)") } else { register(slot) } }

    fn emit_range(&mut self, first: i64, last: i64) -> Result<(), Diagnostic> {
        let mut slot = first;
        while slot < last {
            let instruction = self.view.code[slot as usize].clone();
            match Instruction::unguarded(&instruction) {
                Instruction::LoopBegin(lp) => {
                    let header = slot;
                    let count = lp.initial.len() as i64;
                    self.declare(header);
                    for component in 0..count {
                        self.declare(header + 1 + component);
                    }
                    let guards = match &instruction {
                        Instruction::Guarded(guards, _) => guards.clone(),
                        _ => Vec::new(),
                    };
                    if !guards.is_empty() {
                        let text = guard_text(self, &guards);
                        let _ = writeln!(self.line, "    if ({text}) {{");
                    }
                    if let Some(limit) = lp.limit {
                        let _ = writeln!(self.line, "    const length{header} = {} - {};", self.exact(lp.last), self.exact(lp.first));
                        let _ = writeln!(
                            self.line,
                            "    if (length{header} > {limit}n) fail('E_ITER_BOUND', `range ${{{}}}\u{2025}${{{}}} exceeds {} iterations.`);",
                            self.exact(lp.first),
                            self.exact(lp.last),
                            if limit == 4096 { "4,096".to_owned() } else { limit.to_string() }
                        );
                    }
                    self.assign(header, lp.first);
                    for (component, initial) in lp.initial.iter().enumerate() {
                        self.assign(header + 1 + component as i64, *initial);
                    }
                    let cond = if self.fast { fast_less(header, lp.last) } else { format!("r{header} < r{}", lp.last) };
                    let _ = writeln!(self.line, "    for (; {cond};) {{");
                    self.emit_range(header + 1 + count, lp.finish)?;
                    let (values, stop) = match Instruction::unguarded(&self.view.code[lp.finish as usize]) {
                        Instruction::LoopEnd(_, values, stop) => (values.clone(), *stop),
                        _ => unreachable!("loop without its end"),
                    };
                    if let Some(source) = stop {
                        let _ = writeln!(self.line, "    const stop{header} = {} !== 0;", self.numeric(source));
                    }
                    for (component, source) in values.iter().enumerate() {
                        if self.fast && region::integer_register(&self.view.code[*source as usize]) {
                            let _ = writeln!(self.line, "    const next{header}h{component} = r{source}h, next{header}l{component} = r{source}l;");
                        } else {
                            let _ = writeln!(self.line, "    const next{header}_{component} = r{source};");
                        }
                    }
                    for component in 0..values.len() {
                        let target = header + 1 + component as i64;
                        if self.fast && region::integer_register(&self.view.code[target as usize]) {
                            let _ = writeln!(self.line, "    r{target}h = next{header}h{component}; r{target}l = next{header}l{component};");
                        } else {
                            let _ = writeln!(self.line, "    r{target} = next{header}_{component};");
                        }
                    }
                    if stop.is_some() {
                        let _ = writeln!(self.line, "    if (stop{header}) break;");
                    }
                    if self.fast {
                        let _ = writeln!(self.line, "    r{header}l = add64(r{header}h, r{header}l, 0, 1); r{header}h = RH;");
                    } else {
                        let _ = writeln!(self.line, "    r{header}++;");
                    }
                    self.add("    }\n");
                    if !guards.is_empty() {
                        self.add("    }\n");
                    }
                    slot = lp.finish + 1;
                }
                _ => {
                    self.emit(slot, &instruction)?;
                    slot += 1;
                }
            }
        }
        Ok(())
    }
}

/// The source of `function region(inputs, uniforms, outputs, start, stop)`, which runs iteration
/// indices `[start, stop)` of one region over typed arrays and returns a status: `0` for success,
/// `1` when a value is not finite (native `Packed.force`'s `Nonfinite`, reported as `E_KERNEL`;
/// outputs and state are then partly written and must be discarded). Arithmetic is 64-bit and
/// follows `Packed.force` instruction by instruction, including the division/`mod`/`pow`/`sqrt`
/// guards and the finite checks, and every instruction runs, reachable or not.
///
/// `fast` (a workspace's `:precision "fast"`): a collecting region's integer registers are exact
/// 64-bit pairs (`FAST64`). Text leaf regions use immutable strings and string arrays, collect only,
/// and accept an optional sixth `outputBase = 0` argument. A refusal (a host ray query, a
/// raster-stage intrinsic) is an `E_WEB_SOURCE` diagnostic.
pub fn region(fast: bool, program: &packed::T) -> Result<String, Diagnostic> {
    let view = host::view(program);
    let has_rand = view.code.iter().any(|i| match i {
        Instruction::RandAt(..) => true,
        Instruction::Guarded(_, b) => matches!(**b, Instruction::RandAt(..)),
        _ => false,
    });
    let fast = fast && program.result == RegionResult::Collect && !has_rand;
    let checks = packed::finite_checks(program);
    let exists = |f: &dyn Fn(&Instruction) -> bool| view.code.iter().any(|i| f(Instruction::unguarded(i)));
    let gathered = exists(&|i| matches!(i, Instruction::ArrayPrepare(..)));
    let nonfinite: &'static str = if gathered { "fail('E_NONFINITE', 'packed produced a nonfinite value.');" } else { "return 1;" };
    let collecting = matches!(program.result, RegionResult::Collect | RegionResult::Accumulate(_, true));
    let accumulating = program.result != RegionResult::Collect;
    let outputs = view.outputs.len();
    let width = view.outputs[0].len();
    let textual = view.code.iter().any(region::text_register);
    let state = if collecting { outputs } else { 0 };
    let sources = view.widths.len();
    let product = program.iteration == region::Iteration::Product && sources > 1;
    let mut skip: Vec<i64> = program.skip.iter().copied().filter(|i| *i >= 0).collect();
    skip.sort();
    skip.dedup();
    let mut p = Printer { view: view.clone(), fast, line: String::with_capacity(4096), checks, nonfinite };
    p.add(if textual {
        "function region(inputs, uniforms, outputs, start, stop, outputBase = 0) {\n"
    } else {
        "function region(inputs, uniforms, outputs, start, stop) {\n"
    });
    if exists(&|i| matches!(i, Instruction::Raise(_, _, _, Some(_)))) {
        p.add("  const fail = (code, message, cause) => { const e = new Error(message); e.code = code; if (cause !== undefined) e.cause = cause; throw e; };\n");
    } else {
        p.add("  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n");
    }
    if fast {
        p.add(FAST64);
    } else {
        p.add("  const wrap = x => BigInt.asIntN(64, x);\n");
    }
    if gathered {
        p.add("  const tables = new Map();\n");
        p.add("  const table = (slot, integer, width) => { if (tables.has(slot)) return tables.get(slot); let data = uniforms[slot];\n");
        p.add("    const text = integer === 'text';\n");
        p.add("    if (text ? !Array.isArray(data) : !(data instanceof (integer ? BigInt64Array : Float64Array))) fail('E_ARRAY_TYPE', 'Kernel lookup table changed its packed element type.');\n");
        p.add("    if (text && !data.every(x => typeof x === 'string')) fail('E_ARRAY_TYPE', 'Text arrays contain text values.');\n");
        p.add("    if (data.length % width !== 0) fail('E_ARRAY_TYPE', `Packed vec${width} storage has ${['','','two','three','four'][width]} coordinates per element.`);\n");
        // fast: a table is read as the device would read it, unchecked
        if !fast {
            p.add("    if (!integer) for (const x of data) if (!Number.isFinite(x)) fail('E_NONFINITE', 'array input produced a nonfinite value.');\n");
        }
        if fast {
            // an integer table is read as numbers: converted once a call, not per gather
            p.add("    tables.set(slot, data); return data; };\n");
            p.add("  const row = (array, index, width) => { if (index < 0 || index >= array.length / width) fail('E_ARRAY_RANGE', 'Array index is outside its length.'); return index; };\n");
        } else {
            p.add("    tables.set(slot, data); return data; };\n");
            p.add("  const row = (array, index, width) => { if (index < 0n) fail('E_ARRAY_RANGE', 'array/nth needs a non-negative whole length.');\n");
            p.add("    if (index >= BigInt(array.length / width)) fail('E_ARRAY_RANGE', 'Array index is outside its length.'); return Number(index); };\n");
        }
    }
    if textual {
        p.add("  const text = value => typeof value === 'string' ? value : fail('E_ARRAY_TYPE', 'Kernel input changed its packed element type.');\n");
    }
    if exists(&|i| matches!(i, Instruction::Fma(..) | Instruction::CheckedFma(..))) {
        p.add(FUSED);
    }
    if exists(&|i| matches!(i, Instruction::CheckedMul(..) | Instruction::CheckedFma(..))) {
        p.add("  const checked = (valid, value) => Number.isFinite(value) ? (valid ? 1 : value) : 0;\n");
    }
    if exists(&|i| matches!(i, Instruction::Noise3(..))) {
        p.add("  const noise = (...args) => { try { return noise3(...args); } catch (e) { if (e?.code === 'E_NONFINITE') return NaN; throw e; } };\n");
    }
    if exists(&|i| matches!(i, Instruction::RandAt(..))) {
        p.add("  const u64 = x => BigInt.asUintN(64, x);\n  const randAt = (seed, index) => { let x = u64(seed + 0x9e3779b97f4a7c15n * u64(index + 1n)); x = u64((x ^ (x >> 30n)) * 0xbf58476d1ce4e5b9n); x = u64((x ^ (x >> 27n)) * 0x94d049bb133111ebn); x ^= x >> 31n; return Number(x >> 11n) / 9007199254740992; };\n");
    }
    if fast {
        p.add("  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n");
    } else {
        p.add("  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? BigInt(Math.trunc(x)) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n");
        p.add("  const divide = (a, b, mod) => b === 0n ? fail('E_RANGE', 'Integer division by zero.') : wrap(mod ? a % b : a / b);\n");
        p.add("  const shift = (a, b, right) => b < 0n || b > 63n ? fail('E_RANGE', 'Integer shift count must be within [0, 63].') : wrap(right ? BigInt.asUintN(64, a) >> b : a << b);\n");
    }
    for k in 0..view.widths.len() {
        let _ = writeln!(p.line, "  const in{k} = inputs[{k}];");
    }
    if product {
        for (k, w) in view.widths.iter().enumerate() {
            let _ = writeln!(p.line, "  const n{k} = in{k}.length / {w};");
        }
        let _ = writeln!(p.line, "  const s{} = 1;", sources - 1);
        for k in (0..sources - 1).rev() {
            let _ = writeln!(p.line, "  const s{k} = s{} * n{};", k + 1, k + 1);
        }
    }
    for k in 0..outputs {
        let _ = writeln!(p.line, "  const out{k} = outputs[{k}];");
    }
    if accumulating {
        let _ = writeln!(p.line, "  const st = outputs[{state}];");
    }
    if !skip.is_empty() {
        let _ = write!(p.line, "  const skip = [{}];\n  let s = 0;\n", skip.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(", "));
        p.add("  for (let hi = skip.length; s < hi;) {\n    const mid = (s + hi) >> 1;\n");
        p.add("    if (skip[mid] < start) s = mid + 1; else hi = mid;\n  }\n");
    }
    p.add("  for (let i = start; i < stop; i++) {\n");
    if !skip.is_empty() {
        p.add("    if (s < skip.length && skip[s] === i) { s++; continue; }\n    const o = i - s;\n");
    } else {
        p.add("    const o = i;\n");
    }
    if product {
        for k in 0..view.widths.len() {
            let _ = writeln!(p.line, "    const j{k} = Math.floor(i / s{k}) % n{k};");
        }
    } else {
        for k in 0..view.widths.len() {
            let _ = writeln!(p.line, "    const j{k} = i;");
        }
    }
    p.emit_range(0, view.code.len() as i64)?;
    for (k, registers) in view.outputs.iter().enumerate() {
        for (component, slot) in registers.iter().enumerate() {
            let slot = *slot;
            let code = &view.code[slot as usize];
            if !(region::integer_register(code) || region::text_register(code)) {
                let _ = writeln!(p.line, "    if (!Number.isFinite(r{slot})) {}", p.nonfinite);
            }
            match &program.result {
                RegionResult::Collect | RegionResult::Accumulate(..) => {}
                RegionResult::Sum(true) if region::integer_register(code) => {
                    let _ = writeln!(p.line, "    st[{component}] = st[{width}] !== 0 ? wrap(st[{component}] + r{slot}) : r{slot};");
                }
                RegionResult::Sum(false) if region::integer_register(code) => {
                    let _ = writeln!(p.line, "    st[{component}] = st[{width}] !== 0 ? convert('+', Math.floor(Number(st[{component}]) + Number(r{slot}) + 0.5)) : r{slot};");
                }
                RegionResult::Sum(_) => {
                    let _ = write!(
                        p.line,
                        "    st[{component}] = st[{width}] !== 0 ? st[{component}] + r{slot} : r{slot};\n    if (!Number.isFinite(st[{component}])) return 1;\n"
                    );
                }
            }
            if collecting {
                if region::text_register(code) {
                    let _ = writeln!(p.line, "    out{k}[o - outputBase] = r{slot};");
                } else {
                    let value = if registers.len() == 1 && region::integer_register(code) {
                        if fast { format!("big(r{slot}h, r{slot}l)") } else { register(slot) }
                    } else {
                        p.numeric(slot)
                    };
                    let _ = writeln!(p.line, "    out{k}[o * {} + {component}] = {value};", registers.len());
                }
            }
        }
    }
    match &program.result {
        RegionResult::Accumulate(..) => {
            for (component, slot) in view.outputs[0].iter().enumerate() {
                let _ = writeln!(p.line, "    st[{component}] = r{slot};");
            }
        }
        RegionResult::Sum(_) => {
            let _ = writeln!(p.line, "    st[{width}] = 1;");
        }
        RegionResult::Collect => {}
    }
    p.add("  }\n  return 0;\n}\n");
    Ok(p.line)
}

// ---- the frame level ----

fn literal(store: Option<&Arc<dyn Fn(&[f64]) -> String + Send + Sync>>, l: &Literal) -> String {
    match l {
        Literal::Int(n) => format!("{n}n"),
        Literal::Float(x) => number(*x),
        Literal::Bool(b) => b.to_string(),
        Literal::Text(s) => text_literal::quote(s),
        Literal::Vector(xs) => format!("Float64Array.of({})", xs.iter().map(|x| number(*x)).collect::<Vec<_>>().join(", ")),
        Literal::List(xs) => format!("[{}]", xs.iter().map(|x| literal(store, x)).collect::<Vec<_>>().join(", ")),
        Literal::Record(fields) => {
            format!("new Map([{}])", fields.iter().map(|(n, v)| format!("[{}, {}]", escape(n), literal(store, v))).collect::<Vec<_>>().join(", "))
        }
        Literal::Array { width, data } => match store {
            Some(store) => format!("new rt.PArray({width}, {})", store(data)),
            None => format!("new rt.PArray({width}, Float64Array.of({}))", data.iter().map(|x| number(*x)).collect::<Vec<_>>().join(", ")),
        },
        Literal::IntArray(data) => format!("new rt.PArray(1, BigInt64Array.of({}))", data.iter().map(|n| format!("{n}n")).collect::<Vec<_>>().join(", ")),
        Literal::TextArray(data) => format!("new rt.TextArray([{}])", data.iter().map(|s| text_literal::quote(s)).collect::<Vec<_>>().join(", ")),
        Literal::EmptyGeometry => "null".to_owned(),
    }
}

fn ty_json(ty: &Ty) -> String {
    match ty {
        Ty::Float => "\"float\"".to_owned(),
        Ty::Int => "\"int\"".to_owned(),
        Ty::Bool => "\"bool\"".to_owned(),
        Ty::Text => "\"text\"".to_owned(),
        Ty::Vec2 => "\"vec2\"".to_owned(),
        Ty::Vec3 => "\"vec3\"".to_owned(),
        Ty::Vec4 => "\"vec4\"".to_owned(),
        Ty::Any => "\"any\"".to_owned(),
        Ty::Array(t) => format!("{{array: {}}}", ty_json(t)),
        Ty::List(t) => format!("{{list: {}}}", ty_json(t)),
        Ty::Record(fields) => format!("{{record: [{}]}}", fields.iter().map(|(n, t)| format!("[{}, {}]", escape(n), ty_json(t))).collect::<Vec<_>>().join(", ")),
        _ => "\"other\"".to_owned(),
    }
}

/// The name prims.js exports a primitive under.
fn export_name(primitive: &Primitive) -> String {
    match C::primitive_name(primitive) {
        "+" => "add".to_owned(),
        "-" => "sub".to_owned(),
        "*" => "mul".to_owned(),
        "/" => "div".to_owned(),
        "<" => "lt".to_owned(),
        "<=" => "le".to_owned(),
        ">" => "gt".to_owned(),
        ">=" => "ge".to_owned(),
        "=" => "eq".to_owned(),
        "equal?" => "equal".to_owned(),
        "text/blank?" => "text_blank".to_owned(),
        name => name.replace('/', "_"),
    }
}

/// Fixed-arity primitives: the argument names, in prims.js's parameter order.
fn signature(primitive: &Primitive) -> Option<Vec<&'static str>> {
    use Primitive as P;
    Some(match primitive {
        P::Numeric(_) | P::Remainder | P::Integer(_) | P::IntegerCompare(_) | P::Equal => vec!["a", "b"],
        P::Math(Unary::Not) => vec!["a"],
        P::Math(_) | P::Floor | P::Ceil | P::Round | P::IntConvert | P::FloatConvert | P::ColorByte(_) => vec!["x"],
        P::Show | P::Truthy | P::Coerce(_) => vec!["value"],
        P::TextBlank | P::TextTrim | P::TextLower | P::TextRepr => vec!["text"],
        P::TextTableDecode => vec!["text"],
        P::TextTableEncode => vec!["rows"],
        P::TextTokens | P::TextToFloat | P::TextToInt => vec!["text"],
        P::TextSplit => vec!["text", "separator"],
        P::ArrayGrade => vec!["keys", "descending"],
        P::ArrayIndexOf => vec!["keys", "queries"],
        P::ArrayCompact => vec!["mask"],
        P::ArrayScanSum => vec!["array"],
        P::ArrayScatterAdd => vec!["indices", "values", "size"],
        P::ArrayReduce(_) => vec!["array"],
        P::ArraySegment(_) | P::ArraySegmentArgmax => vec!["values", "offsets"],
        P::NextAfter => vec!["x", "y"],
        P::SequenceLength => vec!["list"],
        P::SequenceGet => vec!["list", "index"],
        P::SequenceBuild => vec!["count", "width", "type", "cooked"],
        P::SequenceSlice => vec!["list", "first", "count"],
        P::ArrayMaxLength => vec!["type"],
        P::ArrayRange => vec!["count", "type"],
        P::ArrayInt | P::ArrayText | P::ArrayFloat | P::ArrayVec3 | P::ArrayVec4 => vec!["count", "value"],
        P::ArrayCount => vec!["array"],
        P::ArrayNth => vec!["array", "index"],
        P::ArraySlice => vec!["array", "first", "count"],
        P::ArrayConcat => vec!["first", "second"],
        P::Noise3 => vec!["position", "seed", "octaves", "lacunarity", "gain"],
        P::RandAt => vec!["seed", "index"],
        P::Error => vec!["code", "message", "cause"],
        P::Fma | P::CheckedFma => vec!["a", "b", "c"],
        P::CheckedMul => vec!["a", "b"],
        P::Fail(..) | P::FailDiagnostic(_) => vec![],
        P::TextConcat | P::SequenceConcat | P::Rand | P::ColorParse => return None,
    })
}

/// Helpers the module carries, emitted only when the body calls them (name, text).
fn helpers() -> [(&'static str, &'static str); 5] {
    [("floatPoint", H_FLOAT_POINT), ("field", H_FIELD), ("store", H_STORE), ("cell", H_CELL), ("num", H_NUM)]
}

type StoreFn = Arc<dyn Fn(&[f64]) -> String + Send + Sync>;
type KernelFn = Arc<dyn Fn(i64) -> Option<String> + Send + Sync>;
type GpuFn = Arc<dyn Fn(i64) -> bool + Send + Sync>;
type SlotFn = Arc<dyn Fn(i64) -> i64 + Send + Sync>;

/// The optional arguments of [`frame`]. `store`, when given, prints every packed-array constant: it
/// receives the elements and returns a JavaScript expression for a `Float64Array` of them (the
/// export keeps them in `data.bin`); without it the elements are written inline. `state_slot` maps
/// program-local slots into a shared workspace slot array (default identity). `gpu region`, when
/// given and true for a region index, prints the region's `Invoke` as `await rt.gpuArray(k)`.
#[derive(Clone, Default)]
pub struct FrameOptions {
    pub store: Option<StoreFn>,
    pub fast: bool,
    pub kernel: Option<KernelFn>,
    pub gpu: Option<GpuFn>,
    pub state_slot: Option<SlotFn>,
    pub bindings: bool,
    pub internal: bool,
}

struct Context {
    body: String,
    used: HashSet<String>,
    map: Vec<(i64, flow_ir::core_ir::Span)>,
    is_async: bool,
    gpu: Option<GpuFn>,
    state_slot: Option<SlotFn>,
    bindings: bool,
    templated: bool,
    plans: BTreeMap<i64, String>,
    fast: bool,
    remembered: bool,
    resident: bool,
    /// after a resident loop whose arrays stay (Core_exec.resident_tail), its ranges' sources and divisors
    tail_ranges: Vec<(i64, (i64, i64))>,
    /// a resident loop keeps its arrays: the frame's end closes the fold
    kept: bool,
    /// inside a resident loop, its array locals: a slot none holds is taken again
    loop_arrays: String,
    kernel: Option<KernelFn>,
}

impl Context {
    fn is_gpu(&self, region: i64) -> bool { self.gpu.as_ref().map(|g| g(region)).unwrap_or(false) }
    fn kernel_of(&self, region: i64) -> Option<String> { self.kernel.as_ref().and_then(|k| k(region)) }
    fn slot_of(&self, slot: i64) -> i64 { self.state_slot.as_ref().map(|f| f(slot)).unwrap_or(slot) }
}

fn local(n: i64) -> String { format!("l{n}") }

fn call(primitive: &Primitive, args: &[(Symbol, i64)]) -> Result<String, Diagnostic> {
    let refuse = |message: String| error("E_WEB_PRIMITIVE", &message);
    let values: Vec<String> = match signature(primitive) {
        None => args.iter().map(|(_, n)| local(*n)).collect(),
        Some(names) => {
            for (name, _) in args {
                if !names.contains(&name.as_str()) {
                    return Err(refuse(format!("Primitive {} has no argument {name}.", C::primitive_name(primitive))));
                }
            }
            let mut values: Vec<String> = names
                .iter()
                .map(|n| match args.iter().find(|(a, _)| a == n) {
                    Some((_, v)) => local(*v),
                    None => "undefined".to_owned(),
                })
                .collect();
            while values.last().map(|v| v == "undefined").unwrap_or(false) {
                values.pop();
            }
            values
        }
    };
    let mut statics: Vec<String> = match primitive {
        Primitive::Coerce(ty) => vec![ty_json(ty)],
        Primitive::ColorByte(C::ByteMode::Clamped) => vec!["\"clamped\"".to_owned()],
        Primitive::ColorByte(C::ByteMode::Unclamped) => vec!["\"unclamped\"".to_owned()],
        Primitive::Fail(code, message) => vec![escape(code), escape(message)],
        Primitive::FailDiagnostic(d) => {
            let mut s = vec![escape(&d.code), escape(&d.message), d.cause.as_ref().map(|c| crate::text_literal::quote(c)).unwrap_or_else(|| "undefined".to_owned())];
            if !(d.hints.is_empty() && d.trace.is_empty()) {
                s.push(format!("[{}]", d.hints.iter().map(|h| escape(h)).collect::<Vec<_>>().join(",")));
                s.push(format!(
                    "[{}]",
                    d.trace
                        .iter()
                        .map(|t| format!("{{node_id:{}n,label:{},operation:{}}}", t.node_id, escape(&t.label), escape(&t.operation)))
                        .collect::<Vec<_>>()
                        .join(",")
                ));
            }
            s
        }
        _ => Vec::new(),
    };
    statics.extend(values);
    Ok(format!("rt.{}({})", export_name(primitive), statics.join(", ")))
}

fn expression(ctx: &mut Context, e: &Expr) -> Result<String, Diagnostic> {
    Ok(match e {
        Expr::Constant(n) => format!("K[{n}]"),
        Expr::Local(n) => local(*n),
        Expr::CallIndex(slot) => format!("BigInt(calls[{slot}]++)"),
        Expr::Source(v) if ctx.bindings && argument_index(v).map(|i| i >= 0).unwrap_or(false) => {
            format!("rt.coreArgument({})", argument_index(v).expect("argument index"))
        }
        Expr::Source(_) | Expr::HostSource { .. } | Expr::SourceDescriptor { .. } => {
            return Err(error("E_WEB_SOURCE", "This host-owned source is outside the 2D web target."))
        }
        Expr::Frame(field) => match field {
            FrameField::Time => "input.t".to_owned(),
            FrameField::Dt => "input.dt".to_owned(),
            FrameField::Index => "BigInt(input.frame)".to_owned(),
            FrameField::Tick => "BigInt(input.tick)".to_owned(),
            FrameField::Width => "BigInt(input.size[0])".to_owned(),
            FrameField::Height => "BigInt(input.size[1])".to_owned(),
            FrameField::PointerX => "input.pointer[0]".to_owned(),
            FrameField::PointerY => "input.pointer[1]".to_owned(),
            FrameField::Input => "rt.frameValue(input)".to_owned(),
        },
        Expr::Prim(primitive, args) => call(primitive, args)?,
        Expr::Vec(ns) => format!("Float64Array.of({})", ns.iter().map(|n| format!("rt.float({})", local(*n))).collect::<Vec<_>>().join(", ")),
        Expr::Fields(fields) => format!("new Map([{}])", fields.iter().map(|(name, n)| format!("[{}, {}]", escape(name), local(*n))).collect::<Vec<_>>().join(", ")),
        Expr::Items(ns) => format!("rt.sequence_concat({})", ns.iter().map(|n| format!("[{}]", local(*n))).collect::<Vec<_>>().join(", ")),
        Expr::Field(n, name) => {
            ctx.used.insert("field".to_owned());
            format!("field(rt, {}, {})", local(*n), escape(name))
        }
    })
}

fn object_of(args: &[(Symbol, i64)]) -> String { format!("{{{}}}", args.iter().map(|(k, n)| format!("{k}: {}", local(*n))).collect::<Vec<_>>().join(", ")) }

fn kind_name(mark: C::Mark) -> &'static str { mark.name() }

fn draw(ctx: &mut Context, d: &Draw) -> String {
    ctx.used.insert("num".to_owned());
    match d {
        Draw::Background(c) => format!(
            "{{ const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, {}), null)); }}",
            local(*c)
        ),
        Draw::Point { at, ink } => {
            ctx.used.insert("floatPoint".to_owned());
            format!(
                "{{ const [x, y] = floatPoint(rt, {}); emitAll(rt, rt.polygon([[x, y], [x + 1, y], [x + 1, y + 1], [x, y + 1]], ink(rt, {}), null)); }}",
                local(*at),
                local(*ink)
            )
        }
        Draw::Mark { mark, args } => format!("markSet(rt, false, {}, {});", escape(kind_name(*mark)), object_of(args)),
        Draw::Batch { mark, args } => format!("markSet(rt, true, {}, {});", escape(kind_name(*mark)), object_of(args)),
        Draw::Points { positions, ink } => format!(
            "{{ const [count, position] = pointsAttr(rt, {}); emit(rt, {{type: \"mark_set\", kind: \"point\", count, position, fill: ink(rt, {})}}); }}",
            local(*positions),
            local(*ink)
        ),
        Draw::Path { path, args } => {
            ctx.used.insert("floatPoint".to_owned());
            format!(
                "pathOf(rt, {}, {});",
                escape(path.name()),
                object_of(args)
            )
        }
        // ponytail: the web runtime has one face and left runs; a styled run fails there until it
        // carries the faces. A default run exports as it always did.
        Draw::TextRun { style: Some(_), .. } => "rt.fail(\"E_WEB_TEXT_STYLE\", \"Text fonts, alignment and tracking are native only.\");".to_owned(),
        Draw::TextRun { at, text, ink, size, style: None } => format!(
            "if (typeof {t} !== \"string\") rt.fail(\"E_TYPE\", \"Drawing text is text.\"); else if ({t} !== \"\") {{ const [x, y] = point(rt, {}); emit(rt, {{type: \"text\", x, y, size: Number(rt.coerce(\"int\", {})), color: ink(rt, {}), text: {t}}}); }}",
            local(*at),
            local(*size),
            local(*ink),
            t = local(*text)
        ),
        Draw::Image { source: (_, id), at, scale, angle } => {
            ctx.is_async = true;
            format!(
                "{{ const resource = await rt.image({id}); const [x, y] = point(rt, {}), scale = num(rt, {}), angle = num(rt, {}); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail(\"E_DRAW_RANGE\", \"Scene.image: invalid transform\"); emit(rt, {{type: \"image\", resource, x, y, scale, angle}}); }}",
                local(*at),
                local(*scale),
                local(*angle)
            )
        }
        Draw::Push(t, n) => format!(
            "transform(rt, {}, {});",
            escape(t.name()),
            local(*n)
        ),
        Draw::Pop => "emit(rt, {type: \"pop_transform\"});".to_owned(),
    }
}

/// An argument declaration `$core/argument`: its index.
fn argument_index(v: &Value) -> Option<i64> {
    match v {
        Value::Source(g) if g.name == "$core/argument" => match (g.fields.len(), g.fields.get_str("index")) {
            (1, Some(Value::Int(i))) => Some(*i),
            _ => None,
        },
        _ => None,
    }
}

/// What each source and uniform of a region reads, by argument index (`Packed.Host.shape` of its
/// template, whose arguments are `$core/argument` markers): a resident fold takes a count on the
/// device from `["arg", i]` and `["range", i, mul, div]`, and declines a placeholder read any other way.
fn shapes_json(packed: &packed::T) -> String {
    let shape = |s: &host::Shape| -> String {
        match s {
            host::Shape::Value(v) => match argument_index(v) {
                Some(i) => format!("['arg',{i}]"),
                None => "['host']".to_owned(),
            },
            host::Shape::Range(v, mul, div) => match argument_index(v) {
                Some(i) => format!("['range',{i},{mul},{div}]"),
                None => "['host']".to_owned(),
            },
            host::Shape::Opaque(None) => "['opaque',null]".to_owned(),
            host::Shape::Opaque(Some(vs)) => {
                format!("['opaque',[{}]]", vs.iter().filter_map(|v| argument_index(v).map(|i| i.to_string())).collect::<Vec<_>>().join(","))
            }
        }
    };
    let list = |a: Vec<host::Shape>| format!("[{}]", a.iter().map(&shape).collect::<Vec<_>>().join(","));
    format!("{{sources:{},uniforms:{}}}", list(host::source_shapes(packed)), list(host::uniform_shapes(packed)))
}

fn line(ctx: &mut Context, depth: usize, text: &str) {
    for _ in 0..2 * depth {
        ctx.body.push(' ');
    }
    ctx.body.push_str(text);
    ctx.body.push('\n');
}

fn statements(ctx: &mut Context, program: &C::Program, depth: usize, list: &[Statement]) -> Result<(), Diagnostic> {
    for s in list {
        statement(ctx, program, depth, s)?;
    }
    Ok(())
}

fn slot_key(ctx: &mut Context, program: &C::Program, slot: i64) -> String {
    let state = &program.state_slots[slot as usize];
    let stamp = if state.plane == Plane::Host { "input.tick" } else { "input.frame" };
    let key = if state.source.iter.is_empty() {
        "\"\"".to_owned()
    } else {
        format!(
            "[{}].join(\",\")",
            state
                .source
                .iter
                .iter()
                .map(|i| match i {
                    Iteration::StaticIndex(i) => i.to_string(),
                    Iteration::LocalIndex(l) => format!("{}.toString()", local(*l)),
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    ctx.used.insert("cell".to_owned());
    format!("cell(state, {}, {key}, {stamp})", ctx.slot_of(slot))
}

fn statement(ctx: &mut Context, program: &C::Program, depth: usize, s: &Statement) -> Result<(), Diagnostic> {
    if let Some(span) = &s.source.span {
        ctx.map.push((s.id, span.clone()));
    }
    let at = format!("at = {}; ", s.id);
    let id = s.id;
    match &s.node {
        StatementNode::Assign(n, Expr::Items(ns)) if program.locals[*n as usize] == Ty::List(Box::new(flow_syntax::ty::geometry())) => {
            let text = format!("{at}{} = rt.sequence_cooked([{}]);", local(*n), ns.iter().map(|n| local(*n)).collect::<Vec<_>>().join(","));
            line(ctx, depth, &text);
        }
        // a resident loop's array is on the device: its count is the one read back
        StatementNode::Assign(n, Expr::Prim(Primitive::ArrayCompact, args)) if ctx.resident && args.len() == 1 => {
            ctx.is_async = true;
            let text = format!("{at}{} = await rt.resident.prim('compact', null, {});", local(*n), local(args[0].1));
            line(ctx, depth, &text);
        }
        StatementNode::Assign(n, Expr::Prim(Primitive::Coerce(ty @ Ty::Array(_)), args)) if ctx.resident && args.len() == 1 => {
            ctx.is_async = true;
            let text = format!("{at}{} = await rt.resident.prim('coerce', {}, {});", local(*n), ty_json(ty), local(args[0].1));
            line(ctx, depth, &text);
        }
        // a range of a kept array's count: the index buffer the fold binds
        StatementNode::Assign(n, Expr::Prim(Primitive::ArrayRange, _)) if ctx.tail_ranges.iter().any(|(k, _)| k == n) => {
            ctx.is_async = true;
            let (source, div) = ctx.tail_ranges.iter().find(|(k, _)| k == n).map(|(_, v)| *v).expect("tail range");
            let kind = match program.locals[*n as usize] {
                Ty::Array(ref e) if **e == Ty::Int => "'int'",
                _ => "'float'",
            };
            let text = format!("{at}{} = await rt.resident.range({}, {div}, {kind});", local(*n), local(source));
            line(ctx, depth, &text);
        }
        StatementNode::Assign(n, Expr::Prim(Primitive::ArrayCount, args)) if ctx.resident && args.len() == 1 => {
            ctx.is_async = true;
            let a = args[0].1;
            let text = format!("{at}{} = await rt.resident.settle({}); {} = rt.array_count({});", local(a), local(a), local(*n), local(a));
            line(ctx, depth, &text);
        }
        StatementNode::Assign(n, e) => {
            let text = format!("{at}{} = {};", local(*n), expression(ctx, e)?);
            line(ctx, depth, &text);
        }
        StatementNode::ResetCall(slot) => line(ctx, depth, &format!("{at}calls[{slot}] = 0;")),
        StatementNode::Draw(d) => {
            let text = format!("{at}{}", draw(ctx, d));
            line(ctx, depth, &text);
        }
        StatementNode::DiagnoseRows { rows, policy, index, handler, diagnostic, result } => {
            if *policy == DiagnosticPolicy::RangeLast {
                let mut d = error("E_WEB_SOURCE", "Range-last diagnosis needs its native host.");
                d.span = s.source.span.clone();
                return Err(d);
            }
            line(ctx, depth, &format!("{at}{{"));
            line(ctx, depth, &format!("  const row{id} = rt.diagnosticFirst({});", local(*rows)));
            line(ctx, depth, &format!("  if (row{id} >= 0) {{"));
            line(ctx, depth, &format!("    {} = BigInt(row{id});", local(*index)));
            line(ctx, depth, "    try {");
            statements(ctx, program, depth + 3, handler)?;
            line(
                ctx,
                depth,
                &format!("    }} catch (rowError{id}) {{ rt.error_mapper_failure(new rt.Fail(\"E_DIAGNOSTIC_ROW\", \"A diagnostic row failed.\"), rowError{id}); }}"),
            );
            line(ctx, depth, &format!("    rt.diagnosticRowsThrow({});", local(*diagnostic)));
            line(ctx, depth, "  }");
            line(ctx, depth, &format!("  {} = null;", local(*result)));
            line(ctx, depth, "}");
        }
        StatementNode::Protect { body, value, failure, handler, mapped, result } => {
            line(ctx, depth, &format!("{at}try {{"));
            statements(ctx, program, depth + 1, body)?;
            line(ctx, depth, &format!("  {} = {};", local(*result), local(*value)));
            line(ctx, depth, &format!("}} catch (error{id}) {{"));
            line(ctx, depth, &format!("  if (error{id} instanceof rt.Fail && error{id}.statement === undefined) error{id}.statement = at;"));
            line(ctx, depth, &format!("  {} = rt.error_payload(error{id});", local(*failure)));
            line(ctx, depth, "  try {");
            statements(ctx, program, depth + 2, handler)?;
            line(ctx, depth, &format!("  }} catch (mapper{id}) {{"));
            line(ctx, depth, &format!("    if (mapper{id} instanceof rt.Fail && mapper{id}.statement === undefined) mapper{id}.statement = at;"));
            line(ctx, depth, &format!("    rt.error_mapper_failure(error{id}, mapper{id});"));
            line(ctx, depth, "  }");
            line(ctx, depth, &format!("  rt.error_rethrow(error{id}, {});", local(*mapped)));
            line(ctx, depth, "}");
        }
        StatementNode::If { condition, yes, no } => {
            line(ctx, depth, &format!("{at}if (rt.truthy({})) {{", local(*condition)));
            statements(ctx, program, depth + 1, yes)?;
            if no.is_empty() {
                line(ctx, depth, "}");
            } else {
                line(ctx, depth, "} else {");
                statements(ctx, program, depth + 1, no)?;
                line(ctx, depth, "}");
            }
        }
        StatementNode::Iterate { sequence, index, item, limit, body } => {
            line(ctx, depth, &format!("{at}{{"));
            line(ctx, depth, &format!("  const s{id} = {}, c{id} = Array.isArray(s{id}) ? s{id}.length : Number(rt.array_count(s{id}));", local(*sequence)));
            line(ctx, depth, &format!("  if (c{id} > {limit}) rt.fail(\"E_ITER_BOUND\", \"Core iteration exceeds its bound.\");"));
            line(ctx, depth, &format!("  for (let i{id} = 0; i{id} < c{id}; i{id}++) {{"));
            line(ctx, depth, &format!("    {} = BigInt(i{id}); {} = Array.isArray(s{id}) ? s{id}[i{id}] : rt.array_nth(s{id}, i{id});", local(*index), local(*item)));
            statements(ctx, program, depth + 2, body)?;
            line(ctx, depth, "  }");
            line(ctx, depth, "}");
        }
        StatementNode::Counted { first, last, count, index, item, limit, stop, init, body } => {
            line(ctx, depth, &format!("{at}{{"));
            line(
                ctx,
                depth,
                &format!(
                    "  const f{id} = rt.coerce(\"int\", {}), e{id} = rt.coerce(\"int\", {}), n{id} = e{id} > f{id} ? e{id} - f{id} : 0n;",
                    local(*first),
                    local(*last)
                ),
            );
            line(ctx, depth, &format!("  if (n{id} > {limit}n) rt.fail(\"E_ITER_BOUND\", \"Core iteration exceeds its bound.\");"));
            line(ctx, depth, &format!("  {} = n{id};", local(*count)));
            statements(ctx, program, depth + 1, init)?;
            let exact_count = *limit > 9007199254740991;
            line(
                ctx,
                depth,
                &format!(
                    "  for (let i{id} = {}, c{id} = {}; i{id} < c{id}{}; i{id}++) {{",
                    if exact_count { "0n" } else { "0" },
                    if exact_count { format!("n{id}") } else { format!("Number(n{id})") },
                    match stop {
                        Some(n) => format!(" && !rt.truthy({})", local(*n)),
                        None => String::new(),
                    }
                ),
            );
            line(
                ctx,
                depth,
                &format!(
                    "    const j{id} = {}; {} = j{id}; {} = f{id} + j{id};",
                    if exact_count { format!("i{id}") } else { format!("BigInt(i{id})") },
                    local(*index),
                    local(*item)
                ),
            );
            statements(ctx, program, depth + 2, body)?;
            line(ctx, depth, "  }");
            line(ctx, depth, "}");
        }
        StatementNode::Store { sequence, index, value } => {
            ctx.used.insert("store".to_owned());
            line(ctx, depth, &format!("{at}store(rt, {}, {}, {});", local(*sequence), local(*index), local(*value)));
        }
        StatementNode::StateRead { slot, result, init, initial } => {
            let key = slot_key(ctx, program, *slot);
            line(ctx, depth, &format!("{at}{{ const c{id} = {key};"));
            line(ctx, depth, &format!("  if (c{id}.value !== undefined) {} = c{id}.value; else {{", local(*result)));
            statements(ctx, program, depth + 2, init)?;
            line(ctx, depth, &format!("    {} = {};", local(*result), local(*initial)));
            line(ctx, depth, "  } }");
        }
        StatementNode::StatePeek { slot, found, value } => {
            let key = slot_key(ctx, program, *slot);
            line(ctx, depth, &format!("{at}{{ const c{id} = {key}; {} = c{id}.pending !== undefined; {} = {} ? c{id}.pending : null; }}", local(*found), local(*value), local(*found)));
        }
        StatementNode::StateWrite { slot, value } => {
            let key = slot_key(ctx, program, *slot);
            line(ctx, depth, &format!("{at}{}.pending = {};", key, local(*value)));
        }
        StatementNode::Invoke { region, args, result } => {
            ctx.is_async = true;
            if ctx.is_gpu(*region) && ctx.kernel_of(*region).is_none() {
                line(ctx, depth, &format!("{at}{} = await rt.gpuArray({region});", local(*result)));
            } else {
                let r = &program.regions[*region as usize];
                let mut dynamic = ctx.templated || r.uniforms.iter().any(E::frame_dependent);
                if !dynamic {
                    for (residual, term) in r.sources.iter() {
                        let bound = E::residual::with_bindings(*residual, term, &[]).expect("residual with its term");
                        if E::frame_dependent(&Value::Residual(bound)) {
                            dynamic = true;
                            break;
                        }
                    }
                }
                let plan = if dynamic { format!(", B{region}") } else { String::new() };
                if dynamic && !ctx.plans.contains_key(region) {
                    let text = binding_plan(ctx.fast, ctx.kernel_of(*region), r)?;
                    ctx.plans.insert(*region, text);
                }
                let argument_list = args.iter().map(|a| local(*a)).collect::<Vec<_>>().join(", ");
                // a display region bound per call runs on the device (fast precision)
                if ctx.is_gpu(*region) && dynamic {
                    line(ctx, depth, &format!("{at}{} = await rt.gpuInvoke({region}, [{argument_list}]{plan});", local(*result)));
                } else {
                    let tail = if ctx.resident && dynamic { format!(", [{}]", ctx.loop_arrays) } else { String::new() };
                    line(ctx, depth, &format!("{at}{} = await rt.invoke({region}, [{argument_list}]{plan}{tail});", local(*result)));
                }
            }
        }
    }
    Ok(())
}

fn binding_plan(fast: bool, kernel: Option<String>, packed: &packed::T) -> Result<String, Diagnostic> {
    let r = packed;
    let refuse = |message: &str| error("E_WEB_SOURCE", message);
    if r.result != RegionResult::Collect || r.outputs.len() != 1 {
        return Err(refuse("Dynamic region arguments require a single Collect output."));
    }
    fn supported(ty: &Ty) -> bool {
        match ty {
            Ty::Int | Ty::Float | Ty::Bool | Ty::Text | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => true,
            Ty::Array(e) if matches!(**e, Ty::Int | Ty::Float | Ty::Text | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) => true,
            Ty::List(e) if **e == Ty::Any => true,
            Ty::List(t) => supported(t),
            Ty::Record(fields) => fields.iter().all(|(_, t)| supported(t)),
            _ => false,
        }
    }
    let argument_types = host::argument_types(packed)?;
    if !argument_types.iter().all(supported) {
        return Err(refuse("This compiled region argument type is outside the 2D web target."));
    }
    let mut needs_frame_value = false;
    let mut prepare = |uniform: bool, program: &C::Program| -> Result<String, Diagnostic> {
        if !program.state_slots.is_empty() || program.call_slots != 0 {
            return Err(refuse("Region preparation cannot own state or call-index slots."));
        }
        // A capture is read by synchronous selected instructions, including workers.
        let has = |predicate: &dyn Fn(&Statement) -> bool, list: &[Statement]| {
            let mut found = false;
            C::iter(&mut |s| found = found || predicate(s), list);
            found
        };
        let asynchronous = has(&|s| matches!(s.node, StatementNode::Invoke { .. }), &program.body);
        let input = has(&|s| matches!(s.node, StatementNode::Assign(_, Expr::Frame(FrameField::Input))), &program.body);
        if uniform && input {
            needs_frame_value = true;
        }
        if uniform && asynchronous {
            return Err(refuse("Asynchronous region captures require a compiled synchronous preparation path."));
        }
        let options = FrameOptions { fast, bindings: true, internal: true, ..Default::default() };
        Ok(frame_result(&options, program)?.0)
    };
    let mut sources: Vec<String> = Vec::new();
    for (residual, term) in r.sources.iter() {
        let view = E::residual::residual_view(*residual)?;
        let bound = E::residual::with_bindings(*residual, term, &view.owned_bindings())?;
        let program = match core_lower::lower(None, None, true, None, None, bound) {
            Ok(p) => p,
            Err(form) => return Err(refuse(&format!("Region input preparation: {form}"))),
        };
        sources.push(format!("{{type:{},source:{}}}", ty_json(term.ty()), escape(&prepare(false, &program)?)));
    }
    let mut captures: Vec<String> = Vec::new();
    for value in r.uniforms.iter() {
        let view = E::residual::residual_view(r.residual)?;
        let source = C::Source {
            site: view.site.as_ref().clone(),
            owner: view.owner.as_ref().clone(),
            instance: view.instance,
            prefix: view.prefix.as_ref().clone(),
            iter: view.iter.to_vec().into_iter().map(Iteration::StaticIndex).collect(),
            span: Some(view.term.form().span.clone()),
        };
        let mut result: i64 = -1;
        let lowered = core_lower::lower_draw(None, Some(r.residual), &mut |ctx: &mut dyn core_lower::DrawContext| {
            result = ctx.value(&source, value)?;
            Ok(())
        });
        let program = match lowered {
            Ok(mut p) => {
                p.results = vec![result];
                p
            }
            Err(form) => return Err(refuse(&format!("Region capture preparation: {form}"))),
        };
        let ty = match value {
            Value::Residual(residual) => E::residual::residual_view(*residual)?.term.ty().clone(),
            v => value::ty_of(v),
        };
        captures.push(format!("{{type:{},source:{}}}", ty_json(&ty), escape(&prepare(true, &program)?)));
    }
    fn facts(slot: usize, instruction: &Instruction) -> Vec<String> {
        match instruction {
            Instruction::Frame(name) => vec![format!("[{slot},{},false]", escape(name))],
            Instruction::IntFrame(name) => vec![format!("[{slot},{},true]", escape(name))],
            Instruction::Guarded(_, body) => facts(slot, body),
            _ => Vec::new(),
        }
    }
    let terminal = &r.outputs[0];
    let integer = terminal.len() == 1 && region::integer_register(&r.code[terminal[0] as usize]);
    let text = terminal.len() == 1 && region::text_register(&r.code[terminal[0] as usize]);
    let frame_facts: Vec<String> = r.code.iter().enumerate().flat_map(|(slot, i)| facts(slot, i)).collect();
    Ok(format!(
        "{{source:{},argumentTypes:[{}],sources:[{}],captures:[{}],product:{},skip:[{}],frame:[{}],length:{},width:{},integer:{integer},text:{text},frameValue:{needs_frame_value},countLimit:{}n,kernel:{},shapes:{}}}",
        escape(&region(fast, packed)?),
        argument_types.iter().map(ty_json).collect::<Vec<_>>().join(","),
        sources.join(","),
        captures.join(","),
        r.iteration == region::Iteration::Product,
        r.skip.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(","),
        frame_facts.join(","),
        r.code.len(),
        r.outputs[0].len(),
        i64::MAX,
        match &kernel {
            Some(id) => escape(id),
            None => "null".to_owned(),
        },
        shapes_json(packed)
    ))
}

fn names(a: &[i64]) -> String { a.iter().map(|n| local(*n)).collect::<Vec<_>>().join(", ") }

fn resident_statement(ctx: &mut Context, program: &C::Program, options: &FrameOptions, depth: usize, s: &Statement) -> Result<(), Diagnostic> {
    let live = core_exec::resident_live(program, s);
    match live {
        Some(live) if options.fast && !options.bindings && !options.internal => {
            ctx.is_async = true;
            // a cut capacity (native Core_exec.Resident_overflow) runs the loop again from its
            // writes' values before it
            let mut writes: Vec<i64> = Vec::new();
            C::iter(&mut |s| writes.extend(C::writes(s)), std::slice::from_ref(s));
            writes.sort();
            writes.dedup();
            let writes_text = names(&writes);
            let _ = write!(ctx.body, "    const r{} = [{writes_text}];\n    for (;;) {{\n    rt.resident.begin();\n    try {{\n", s.id);
            let mut arrays: Vec<i64> = Vec::new();
            C::iter(&mut |s| arrays.extend(C::writes(s)), std::slice::from_ref(s));
            arrays.sort();
            arrays.dedup();
            let arrays: Vec<i64> = arrays.into_iter().filter(|n| matches!(program.locals[*n as usize], Ty::Array(_))).collect();
            ctx.loop_arrays = names(&arrays);
            ctx.resident = true;
            statement(ctx, program, 3, s)?;
            ctx.resident = false;
            // its arrays stay on the device when what follows only ranges over and draws them
            let tail = core_exec::resident_tail(program, s, &|r| ctx.is_gpu(r));
            if let Some((_, ranges)) = &tail {
                let mut merged = ranges.clone();
                merged.extend(ctx.tail_ranges.iter().cloned());
                ctx.tail_ranges = merged;
                ctx.kept = true;
            }
            let ending = match &tail {
                Some(_) => "      await rt.resident.keep();\n".to_owned(),
                None => format!("      [{}] = await rt.resident.finish([{}]);\n", names(&live), names(&live)),
            };
            let _ = write!(
                ctx.body,
                "{ending}      break;\n    }} catch (e) {{ if (e !== rt.resident.overflow) throw e; [{writes_text}] = r{}; }}\n    finally {{ rt.resident.end(); }} }}\n",
                s.id
            );
            Ok(())
        }
        _ => statement(ctx, program, depth, s),
    }
}

/// The frame program and its side table (statement id to source span).
pub fn frame_result(options: &FrameOptions, program: &C::Program) -> Result<(String, Vec<(i64, flow_ir::core_ir::Span)>), Diagnostic> {
    // Once one region consumes computed locals, its producer Invokes need real source preparation
    // too. Argument-free fixed exports keep data.bin calls.
    let mut templated = options.bindings;
    if !templated {
        for region in program.regions.iter() {
            if !host::argument_types(region)?.is_empty() {
                templated = true;
                break;
            }
        }
    }
    let mut ctx = Context {
        body: String::with_capacity(4096),
        used: HashSet::new(),
        map: Vec::new(),
        is_async: false,
        gpu: options.gpu.clone(),
        state_slot: options.state_slot.clone(),
        bindings: options.bindings,
        templated,
        plans: BTreeMap::new(),
        fast: options.fast,
        remembered: false,
        resident: false,
        tail_ranges: Vec::new(),
        kept: false,
        loop_arrays: String::new(),
        kernel: options.kernel.clone(),
    };
    // A drawing's loop memory, as native `Core_exec.prepare ~remember_loops`: a top-level loop or
    // region call whose reads repeat one of its last 4 keys hands back that run's writes.
    for s in program.body.iter() {
        let footprint = core_exec::footprint(program, s);
        match (&s.node, footprint) {
            (StatementNode::Iterate { .. } | StatementNode::Counted { .. } | StatementNode::Invoke { .. }, Some((reads, writes)))
                if !options.bindings && !options.internal && !writes.is_empty() =>
            {
                ctx.remembered = true;
                let _ = write!(ctx.body, "    {{ const k{} = [{}], h{} = rt.recall(M, {}, k{});\n", s.id, names(&reads), s.id, s.id, s.id);
                let _ = write!(ctx.body, "    if (h{}) [{}] = h{}; else {{\n", s.id, names(&writes), s.id);
                // a resident fold (native Core_exec.resident_live): in fast precision its regions stay on
                // the device and its live writes come back once, as it ends
                resident_statement(&mut ctx, program, options, 3, s)?;
                let _ = write!(ctx.body, "    rt.keep(M, {}, k{}, [{}]); }} }}\n", s.id, s.id, names(&writes));
            }
            _ => resident_statement(&mut ctx, program, options, 2, s)?,
        }
    }
    let code = std::mem::take(&mut ctx.body);
    let mut out = String::with_capacity(8192);
    out.push_str("// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.\n");
    if !program.constants.is_empty() {
        out.push_str("let K;\n");
    }
    if ctx.remembered {
        out.push_str("const M = new Map();\n");
    }
    for (name, text) in helpers() {
        if ctx.used.contains(name) {
            out.push_str(text);
        }
    }
    for (id, plan) in ctx.plans.iter() {
        let _ = writeln!(out, "const B{id} = {plan};");
    }
    let _ = writeln!(out, "{}{}function frame(input, state, rt) {{", if options.internal { "" } else { "export " }, if ctx.is_async { "async " } else { "" });
    if !program.constants.is_empty() {
        let _ = writeln!(out, "  K ??= [{}];", program.constants.iter().map(|c| literal(options.store.as_ref(), c)).collect::<Vec<_>>().join(", "));
    }
    let n = program.locals.len();
    if n > 0 {
        let all: Vec<String> = (0..n as i64).map(local).collect();
        for row in all.chunks(16) {
            let _ = writeln!(out, "  let {};", row.join(", "));
        }
    }
    out.push_str("  let at = 0;\n");
    if program.call_slots > 0 {
        let _ = writeln!(out, "  const calls = new Int32Array({});", program.call_slots);
    }
    out.push_str("  try {\n");
    out.push_str(&code);
    out.push_str("  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }");
    // a fold that kept its arrays closes as the frame ends
    out.push_str(if ctx.kept { " finally { rt.resident.close(); }\n" } else { "\n" });
    match program.results.as_slice() {
        [r] => {
            let _ = writeln!(out, "  return {};", local(*r));
        }
        rs => {
            let _ = writeln!(out, "  return [{}];", rs.iter().map(|r| local(*r)).collect::<Vec<_>>().join(", "));
        }
    }
    out.push_str("}\n");
    Ok((out, ctx.map))
}

/// The frame level as one ES module exporting `frame(input, state, rt)` (an `async` function
/// exactly when the program invokes a region or resolves an image). A failure is thrown as
/// `rt.Fail` (or a JavaScript error) with the executing statement's id set as `error.statement`;
/// [`frame_map`] turns it into a source span. Frame generation returns a refusal for
/// target-incompatible primitives, host sources, range-last diagnosis, or region captures; these
/// use `E_WEB_PRIMITIVE` or `E_WEB_SOURCE`.
pub fn frame(options: &FrameOptions, program: &C::Program) -> Result<String, Diagnostic> { frame_result(options, program).map(|r| r.0) }

/// A region with arguments or frame-dependent inputs runs from a binding plan; this is whether one
/// can be made for it, so a draw lowering may keep it a region on the web.
pub fn bindable(packed: &packed::T) -> bool { binding_plan(true, None, packed).is_ok() }

/// The side table for `program.map`: one line `<statement id> <start> <finish>` per statement that
/// has a source span, in program order.
pub fn frame_map(program: &C::Program) -> String {
    match frame_result(&FrameOptions::default(), program) {
        Ok((_, map)) => map.iter().map(|(id, s)| format!("{id} {} {}\n", s.start, s.finish)).collect::<Vec<_>>().concat(),
        Err(_) => String::new(),
    }
}


// ---- JavaScript text carried verbatim from  ----

const FUSED: &str = r##"  const fmaBytes = new DataView(new ArrayBuffer(8));
  const fmaDecode = x => {
    fmaBytes.setFloat64(0, x, false);
    const b = fmaBytes.getBigUint64(0, false), exponent = Number((b >> 52n) & 2047n);
    const fraction = b & ((1n << 52n) - 1n);
    return {negative: !!(b >> 63n), n: exponent ? fraction | (1n << 52n) : fraction,
      e: exponent ? exponent - 1075 : -1074};
  };
  const fused = (a, b, c) => {
    if (!Number.isFinite(a) || !Number.isFinite(b) || !Number.isFinite(c)) return NaN;
    const aa = fmaDecode(a), bb = fmaDecode(b), cc = fmaDecode(c);
    const negative = aa.negative !== bb.negative, pe = aa.e + bb.e, e = Math.min(pe, cc.e);
    let n = aa.n * bb.n << BigInt(pe - e);
    if (negative) n = -n;
    const cn = cc.n << BigInt(cc.e - e);
    n += cc.negative ? -cn : cn;
    if (n === 0n) return aa.n * bb.n === 0n && cc.n === 0n && negative && cc.negative ? -0 : 0;
    const sign = n < 0n;
    if (sign) n = -n;
    const quantum = Math.max(-1074, e + n.toString(2).length - 53), shift = quantum - e;
    let rounded;
    if (shift > 0) {
      const s = BigInt(shift), q = n >> s, remainder = n - (q << s), half = 1n << (s - 1n);
      rounded = q + (remainder > half || (remainder === half && (q & 1n)) ? 1n : 0n);
    } else rounded = n << BigInt(-shift);
    const value = Number(rounded) * 2 ** quantum;
    return sign ? -value : value;
  };
"##;

const FAST64: &str = r##"  let RH = 0;
  const P32 = 4294967296;
  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };
  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };
  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);
  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };
  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };
  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;
    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);
    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };
  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };
  const div64 = (ah, al, bh, bl, mod) => {
    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');
    const a = ah * P32 + al, b = bh * P32 + bl;
    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));
    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };
  const shift64 = (ah, al, b, right) => {
    if (b < 0 || b > 63) fail('E_RANGE', 'Integer shift count must be within [0, 63].');
    if (b === 0) { RH = ah; return al; }
    if (right) { if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }
    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }
    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };
"##;

const H_FLOAT_POINT: &str = r##"const floatPoint = (rt, v) => { const c = comps(rt, v);
  if (![c[0], c[1]].every(x => Number.isFinite(x) && Math.abs(x) <= 3.4028234663852886e38))
    rt.fail("E_DRAW_RANGE", "A drawing coordinate must be finite and fit a 32-bit float.");
  return [c[0], c[1]];
};
"##;

const H_FIELD: &str = r##"const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
"##;

const H_STORE: &str = r##"const store = (rt, s, index, value) => {
  const i = Number(rt.coerce("int", index));
  if (s instanceof rt.TextArray) {
    if (i < 0 || i >= s.data.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (typeof value !== 'string') rt.fail("E_ARRAY_TYPE", "Text arrays contain text values.");
    s.data[i] = value;
  } else if (s instanceof rt.PArray) {
    const n = s.data.length / s.width;
    if (i < 0 || i >= n) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (s.data instanceof BigInt64Array) {
      if (typeof value !== 'number' && typeof value !== 'bigint' && typeof value !== 'boolean')
        rt.fail("E_ARRAY_TYPE", "Packed integer arrays contain integer values.");
      s.data[i] = rt.coerce("int", value);
      return;
    }
    const comps = value instanceof Float64Array ? value : Array(s.width).fill(rt.float(value));
    if (comps.length !== s.width) rt.fail("E_ARRAY_TYPE", "Packed array element has the wrong vector width.");
    for (let j = 0; j < s.width; j++)
      s.data[i * s.width + j] = Number.isFinite(comps[j]) ? comps[j] : rt.fail("E_NONFINITE", "array produced a nonfinite value.");
  } else {
    if (i < 0 || i >= s.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    s[i] = value;
  }
};
"##;

const H_CELL: &str = r##"const cell = (state, slot, key, stamp) => {
  const cells = state[slot] ??= new Map();
  let c = cells.get(key);
  if (c === undefined) cells.set(key, c = { value: undefined, pending: undefined, stamp });
  else if (c.stamp !== stamp) {
    if (c.pending !== undefined) c.value = c.pending;
    c.pending = undefined; c.stamp = stamp;
  }
  return c;
};
"##;

const H_NUM: &str = r##"const num = (rt, v) => typeof v === "number" ? v : typeof v === "bigint" ? Number(v)
  : typeof v === "boolean" ? +v : rt.fail("E_TYPE", "Expected a number.");
const comps = (rt, v) => v instanceof Float64Array && v.length === 3 ? v : (n => [n, n, n])(num(rt, v));
const toInt = (rt, x) => Number.isFinite(x) && x < 2 ** 62 && x >= -(2 ** 62) ? Math.trunc(x)
  : rt.fail("E_DRAW_RANGE", "A drawing coordinate must fit an integer.");
const point = (rt, v) => { const c = comps(rt, v); return [toInt(rt, c[0]), toInt(rt, c[1])]; };
const pair = (rt, v) => { const c = comps(rt, v); return [c[0], c[1]]; };
const rgba = (rt, v) => {
  const c = rt.color_parse(v);
  return (Number(c.get("r")) << 24 | Number(c.get("g")) << 16 | Number(c.get("b")) << 8 | Number(c.get("a"))) | 0;
};
const packed3 = (rt, v) => v instanceof rt.PArray && v.width === 3;
const ink = (rt, v) => packed3(rt, v) ? -1 : rgba(rt, v);
const emit = (rt, entry) => {
  if (rt.marks.length >= 1048576) rt.fail("E_DRAW_RANGE", "A drawing has too many entries.");
  rt.marks.push(entry);
};
const emitAll = (rt, entries) => { for (const e of entries) emit(rt, e); };
const buffer = (data, count, width) => ({ buffer: data, count, width });
const items = (rt, v) => v instanceof rt.PArray ? Array.from({ length: Number(rt.array_count(v)) }, (_, i) => rt.array_nth(v, i))
  : Array.isArray(v) ? v : rt.fail("E_TYPE", "Expected an array.");
const arg = (rt, a, k) => a[k] ?? rt.fail("E_DRAW_INPUT", "draw needs " + k + ".");
const colourAttr = (rt, count, v) => {
  if (!packed3(rt, v)) return { constant: rgba(rt, v) };
  if (v.data.length !== 3 * count) rt.fail("E_DRAW_LENGTH", "Colors and positions have different lengths.");
  return buffer(v.data, count, 3);
};
const checkColours = (rt, n, a) => {
  for (const k of ["fill", "stroke", "color"])
    if (packed3(rt, a[k]) && a[k].data.length !== 3 * n) rt.fail("E_DRAW_LENGTH", "Colors and positions have different lengths.");
};
const pointsAttr = (rt, v) => {
  if (v instanceof rt.PArray && (v.width === 2 || v.width === 3)) {
    const n = v.data.length / v.width;
    return [n, buffer(v.data, n, v.width)];
  }
  const xs = items(rt, v), data = new Float64Array(2 * xs.length);
  xs.forEach((x, i) => { const p = pair(rt, x); data[2 * i] = p[0]; data[2 * i + 1] = p[1]; });
  return [xs.length, buffer(data, xs.length, 2)];
};
const markSet = (rt, batch, kind, a) => {
  const style = n => [a.fill === undefined ? { constant: 0 } : colourAttr(rt, n, a.fill),
    a.stroke === undefined ? { constant: 0 } : colourAttr(rt, n, a.stroke)];
  const where = key => batch ? pointsAttr(rt, arg(rt, a, key)) : [1, { constant: pair(rt, arg(rt, a, key)) }];
  const one = v => ({ constant: pair(rt, v) });
  const width = { constant: 1 };
  if (kind === "circle") {
    const [n, position] = where(batch ? "positions" : "at");
    const r = arg(rt, a, "radius");
    let radius;
    if (!batch) radius = { constant: num(rt, r) };
    else if (r instanceof rt.PArray && r.width === 1) {
      if (r.data.length !== n) rt.fail("E_DRAW_LENGTH", "Radii and positions have different lengths.");
      radius = buffer(r.data, n, 1);
    } else if (typeof r === "bigint" || typeof r === "number") radius = { constant: num(rt, r) };
    else rt.fail("E_TYPE", "Radius is a number or packed float array.");
    checkColours(rt, n, a);
    const [fill, stroke] = style(n);
    emit(rt, { type: "mark_set", kind, count: n, position, radius, fill, stroke, stroke_width: width });
  } else if (kind === "rect") {
    const [n, position] = where(batch ? "positions" : "at");
    const s = arg(rt, a, batch ? "sizes" : "size");
    let size;
    if (!batch) size = one(s);
    else if (packed3(rt, s)) {
      if (s.data.length !== 3 * n) rt.fail("E_DRAW_LENGTH", "Sizes and positions have different lengths.");
      size = buffer(s.data, n, 3);
    } else if (s instanceof Float64Array && s.length === 3) size = one(s);
    else rt.fail("E_TYPE", "Sizes are a vec3 or packed vec3 array.");
    checkColours(rt, n, a);
    const [fill, stroke] = style(n);
    emit(rt, { type: "mark_set", kind, count: n, position, size, fill, stroke, stroke_width: width });
  } else {
    const [n, position] = where("from");
    let end;
    if (batch) {
      const [m, e] = pointsAttr(rt, arg(rt, a, "to"));
      if (m !== n) rt.fail("E_DRAW_LENGTH", "Line endpoints have different lengths.");
      end = e;
    } else end = one(arg(rt, a, "to"));
    checkColours(rt, n, a);
    emit(rt, { type: "mark_set", kind, count: n, position, end, stroke: colourAttr(rt, n, arg(rt, a, "color")),
      stroke_width: { constant: num(rt, arg(rt, a, "width")) } });
  }
};
const optColour = (rt, v) => v === undefined ? null : rgba(rt, v);
const pathOf = (rt, kind, a) => {
  if (kind === "polygon") emitAll(rt, rt.polygon(items(rt, arg(rt, a, "points")).map(p => floatPoint(rt, p)), optColour(rt, a.fill), optColour(rt, a.stroke)));
  else if (kind === "polyline") emitAll(rt, rt.polyline(items(rt, arg(rt, a, "points")).map(p => floatPoint(rt, p)), ink(rt, arg(rt, a, "color"))));
  else {
    const [w, h] = floatPoint(rt, arg(rt, a, "size")), [x, y] = floatPoint(rt, arg(rt, a, "at"));
    const radius = Math.max(0, Math.min(num(rt, arg(rt, a, "radius")), Math.min(Math.abs(w), Math.abs(h)) / 2));
    emit(rt, { type: "push_transform", xx: 1, xy: 0, yx: 0, yy: 1, tx: x, ty: y });
    emitAll(rt, rt.rounded_rect(w, h, radius, optColour(rt, a.fill), optColour(rt, a.stroke)));
    emit(rt, { type: "pop_transform" });
  }
};
const transform = (rt, kind, v) => {
  if (kind === "translate") {
    const [x, y] = point(rt, v);
    emit(rt, { type: "push_transform", xx: 1, xy: 0, yx: 0, yy: 1, tx: x, ty: y });
  } else if (kind === "rotate") {
    const a = num(rt, v);
    if (!Number.isFinite(a)) rt.fail("E_NONFINITE", "angle produced a nonfinite value.");
    emit(rt, { type: "push_transform", xx: Math.cos(a), xy: Math.sin(a), yx: -Math.sin(a), yy: Math.cos(a), tx: 0, ty: 0 });
  } else {
    const [x, y] = v instanceof Float64Array && v.length === 3 ? [v[0], v[1]] : (n => [n, n])(num(rt, v));
    if (!Number.isFinite(x) || !Number.isFinite(y)) rt.fail("E_NONFINITE", "scale produced a nonfinite value.");
    emit(rt, { type: "push_transform", xx: x, xy: 0, yx: 0, yy: y, tx: 0, ty: 0 });
  }
};
"##;

