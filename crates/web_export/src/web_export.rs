//! the static 2D workspace export, with its JavaScript/WGSL
//! runtime embedded. `lower` checks and lowers the first draw graph; `files` prints the export.
use rays_math::hash;
use rays_math::text_array::TextArray;
use crate::web_runtime;
use editor_document::workspace_doc as doc;
use flow_check::workspace::{self as W, Graph, Node, Term};
use flow_eval as E;
use flow_ir::core_draw;
use flow_ir::core_ir::{self as C, region};
use flow_ir::core_lower::{self, DrawContext, RegionAllowed};
use flow_ir::packed::{self, host};
use flow_syntax::context::{self, Context};
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::{self, Value};
use flow_syntax::Diagnostic;
use flow_web::{js, kernel_table, target, text_literal, wgsl};
use rays_diagnostic::Span;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

type R<T> = Result<T, Diagnostic>;

pub struct Lowered {
    pub program: C::Program,
    pub span: Span,
    pub root: Option<value::ResidualId>,
    pub plan: E::Plan,
}

fn err(code: &str, message: &str, span: Option<Span>) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), span, None, None, Vec::new(), Vec::new())
}


fn foreign_context(c: Context) -> bool {
    c == context::sop() || c == context::scene()
}

/// The first reference from the draw graph to a sop or scene graph, or a catalog call of those contexts.
fn foreign(d: &doc::T, draw: &Graph) -> Option<(Term, String, Context)> {
    let graphs: Vec<&Graph> = d.checked.graphs.iter().chain(d.checked.defs.iter()).collect();
    fn visit(t: &Term, graphs: &[&Graph], seen: &mut HashSet<String>) -> Option<(Term, String, Context)> {
        let here = match t.node {
            Node::GraphRef(reference) => {
                let graph = t.name(t.arena().graph_ref(reference).graph);
                match graphs.iter().find(|g| g.name == graph) {
                    Some(g) if foreign_context(g.context) => Some((graph.to_owned(), g.context)),
                    Some(g) if !seen.contains(&g.name) => {
                        seen.insert(g.name.clone());
                        visit(&g.body, graphs, seen).map(|(_, n, c)| (n, c))
                    }
                    _ => None,
                }
            }
            Node::Call(call) => {
                let call = t.arena().call(call);
                foreign_context(call.ctx).then(|| (t.name(call.kind).to_owned(), call.ctx))
            }
            _ => None,
        };
        match here {
            Some((name, ctx)) => Some((t.clone(), name, ctx)),
            None => {
                for &id in t.children() {
                    if let Some(found) = visit(&t.child(id), graphs, seen) {
                        return Some(found);
                    }
                }
                // The bypassed call is visited first, then its explicit pass-through input.
                if let Node::Bypass(id) = t.node {
                    if let Some(input) = W::bypass_input(&t.child(id)) {
                        return visit(&input, graphs, seen);
                    }
                }
                None
            }
        }
    }
    visit(&draw.body, &graphs, &mut HashSet::new())
}

fn first_draw(d: &doc::T) -> R<&Graph> {
    d.checked
        .graphs
        .iter()
        .find(|g| g.context == context::draw())
        .ok_or_else(|| err("E_WEB_GRAPH", "The workspace has no draw graph to export.", None))
}

fn fixed_region(region: &C::RegionT) -> bool {
    host::argument_types(region).map(|t| t.is_empty()).unwrap_or(false)
        && !(region.sources.iter().any(|(residual, term)| match E::residual::with_bindings(*residual, term, &[]) {
            Ok(id) => E::frame_dependent(&Value::Residual(id)),
            Err(_) => false,
        }) || region.uniforms.iter().any(E::frame_dependent))
}

pub fn lower(d: &doc::T) -> R<Lowered> {
    for graph in d.checked.graphs.iter().chain(d.checked.defs.iter()) {
        let mut pending = vec![graph.body.clone()];
        while let Some(term) = pending.pop() {
            if let W::Node::Op(id) = term.node {
                if term.name(term.arena().op(id).op) == "table/load" { return Err(err("E_WEB_TABLE", "table/load is a native resource and cannot be exported to the web.", None)); }
            }
            pending.extend(term.children().iter().map(|id| term.child(*id)));
        }
    }
    let draw = first_draw(d)?;
    if let Some((t, name, ctx)) = foreign(d, draw) {
        return Err(err(
            "E_WEB_GRAPH",
            &format!("The web export draws 2D canvas graphs only; this reaches the {} {}.", context::name(ctx), name),
            Some(t.form().span.clone()),
        ));
    }
    let options = flow_sop::lower::Options { inputs: Some((*d.inputs).clone()), ..Default::default() };
    let lowered = flow_sop::lower::of_checked(None, options, &d.checked)?;
    let evaluated = lowered.evaluated;
    let value = evaluated
        .results
        .iter()
        .find(|(n, _)| n == &draw.name)
        .map(|(_, v)| v.clone())
        .ok_or_else(|| err("E_WEB_LOWER", "The draw graph has no result.", None))?;
    // as native: a region stays a region when the web can run it, fixed (export data) or bound per call
    let runnable: RegionAllowed = Rc::new(|r: &C::RegionT| fixed_region(r) || js::bindable(r));
    match core_draw::lower(Some(runnable), &evaluated.plan, &value) {
        Ok((program, root)) => Ok(Lowered { program, span: draw.form.span.clone(), root, plan: evaluated.plan.clone() }),
        Err(message) => Err(err(
            "E_WEB_LOWER",
            &format!("This draw graph does not lower to the core: {message}"),
            Some(draw.form.span.clone()),
        )),
    }
}

// ---- the files ----

pub type Files = Vec<(String, Vec<u8>)>;

fn all_statements(body: &[C::Statement]) -> Vec<C::Statement> {
    let mut out = Vec::new();
    C::iter(&mut |s| out.push(s.clone()), body);
    out
}

/// The frame facts a region may read, named as the region's Frame instructions name them.
const FRAME_FACTS: [&str; 5] = ["t", "frame/dt", "frame/index", "frame/width", "frame/height"];

fn number(x: f64) -> String {
    param::float_text(x)
}

fn list<T>(f: &dyn Fn(&T) -> String, xs: &[T]) -> String {
    format!("[{}]", xs.iter().map(f).collect::<Vec<_>>().join(", "))
}

fn json_bytes(b: &[u8]) -> String {
    text_literal::quote(b)
}

// ---- the data buffer: placed arrays, found again by structural (`compare`) equality ----

struct Store {
    data: Vec<u8>,
    buckets: HashMap<u64, Vec<(Value, (i64, i64))>>,
}
type Shared = Arc<Mutex<Store>>;

fn bits(x: f64) -> u64 {
    if x.is_nan() { 0x7ff8_0000_0000_0000 } else if x == 0.0 { 0 } else { x.to_bits() }
}

fn same_float(a: f64, b: f64) -> bool {
    a == b || (a.is_nan() && b.is_nan())
}

fn key_hash(v: &Value) -> u64 {
    let mut h = hash::SEED;
    let mut mix = |x: u64| { h = hash::word(h, x); };
    match v {
        Value::IntArray(a) => {
            mix(1); mix(a.len() as u64);
            a.iter().for_each(|x| mix(*x as u64));
        }
        Value::Floats(width @ 1..=4, a) => {
            mix(*width as u64 + 1); mix(a.len() as u64);
            a.iter().for_each(|x| mix(bits(*x)));
        }
        _ => {}
    }
    h
}

fn same_key(a: &Value, b: &Value) -> bool {
    let f = |x: &Arc<Vec<f64>>, y: &Arc<Vec<f64>>| x.len() == y.len() && x.iter().zip(y.iter()).all(|(p, q)| same_float(*p, *q));
    match (a, b) {
        (Value::IntArray(x), Value::IntArray(y)) => x == y,
        (Value::Floats(aw @ 1..=4, x), Value::Floats(bw, y)) if aw == bw => f(x, y),
        _ => false,
    }
}

fn find(s: &Store, values: &Value) -> Option<(i64, i64)> {
    s.buckets.get(&key_hash(values))?.iter().find(|(k, _)| same_key(k, values)).map(|(_, p)| *p)
}

/// Appends the array to `data.bin` (always) and remembers it as the placement of an equal array.
fn store(shared: &Shared, values: &Value) -> (i64, i64) {
    let mut s = shared.lock().unwrap();
    let offset = s.data.len() as i64;
    let length = match values {
        Value::IntArray(a) => {
            a.iter().for_each(|x| s.data.extend_from_slice(&x.to_le_bytes()));
            a.len()
        }
        Value::Floats(1..=4, a) => {
            a.iter().for_each(|x| s.data.extend_from_slice(&x.to_bits().to_le_bytes()));
            a.len()
        }
        _ => panic!("Web region inputs must be packed arrays."),
    } as i64;
    let h = key_hash(values);
    let bucket = s.buckets.entry(h).or_default();
    match bucket.iter_mut().find(|(k, _)| same_key(k, values)) {
        Some(e) => e.1 = (offset, length),
        None => bucket.push((values.clone(), (offset, length))),
    }
    (offset, length)
}

/// A kernel's inputs are the data its regions already hold, when they are the same arrays.
fn shared_data(shared: &Shared, values: &Value) -> (i64, i64) {
    let found = find(&shared.lock().unwrap(), values);
    match found {
        Some(p) => p,
        None => store(shared, values),
    }
}

// ---- fixed regions ----

#[derive(Clone)]
struct Fixed {
    packed: C::RegionT,
    view: host::View,
    arrays: Vec<Value>,
    uniforms: Vec<Value>,
    count: i64,
    frame: Vec<(i64, String, bool)>,
}

fn region_instruction(i: &region::Instruction) -> &region::Instruction {
    match i {
        region::Instruction::Guarded(_, body) => region_instruction(body),
        other => other,
    }
}

fn uniform_components(v: &Value) -> Vec<f64> {
    match v {
        Value::Floats(1..=4, a) => (**a).clone(),
        Value::Vec2(x, y) => vec![*x, *y],
        Value::Vec3(x, y, z) => vec![*x, *y, *z],
        Value::Vec4(x, y, z, w) => vec![*x, *y, *z, *w],
        other => vec![value::num(other).unwrap_or(f64::NAN)],
    }
}

fn span_of_region(r: &C::RegionT) -> Option<Span> {
    Some(r.term.form().span.clone())
}

fn fixed_inputs(r: &C::RegionT) -> R<Fixed> {
    let refuse = |code: &str, message: &str| Err(err(code, message, span_of_region(r)));
    if !fixed_region(r) {
        return refuse("E_WEB_CAPTURE", "This region reads a value that changes from frame to frame; the web export needs its inputs fixed.");
    }
    let packed = r.clone();
    let view = host::view(&packed);
    let probe = |t: f64, frame: i64| input::Frame {
        dt: 0.25,
        frame,
        tick: frame,
        size: (640 + frame, 480),
        pointer: (100.0 + t, 100.0),
        ..input::at_time(t)
    };
    let prepare = |live: input::Frame| host::prepare_values(&packed, &host::PrepareOptions::default(), &live);
    let a = prepare(probe(0.0, 1))?;
    let b = prepare(probe(0.5, 7))?;
    if a.arrays != b.arrays || a.uniforms != b.uniforms || a.count != b.count {
        return refuse("E_WEB_CAPTURE", "This region reads a value that changes from frame to frame; the web export needs its inputs fixed.");
    }
    let non_finite = a.uniforms.iter().any(|v| match v {
        Value::Int(_) | Value::IntArray(_) | Value::Text(_) | Value::TextArray(_) => false,
        _ => uniform_components(v).iter().any(|x| !x.is_finite()),
    });
    if non_finite {
        return refuse("E_WEB_REGION", "This region captures a non-finite value.");
    }
    let frame: Vec<(i64, String, bool)> = view
        .code
        .iter()
        .enumerate()
        .filter_map(|(slot, i)| match region_instruction(i) {
            region::Instruction::Frame(name) => Some((slot as i64, name.clone(), false)),
            region::Instruction::IntFrame(name) => Some((slot as i64, name.clone(), true)),
            _ => None,
        })
        .collect();
    if let Some((_, name, _)) = frame.iter().find(|(_, name, _)| !FRAME_FACTS.contains(&name.as_str())) {
        return refuse("E_WEB_REGION", &format!("This region reads the frame fact {name}, which the web runtime does not provide."));
    }
    Ok(Fixed { packed, view, arrays: a.arrays, uniforms: a.uniforms, count: a.count, frame })
}

fn texts(values: &TextArray) -> String {
    format!("{{\"texts\":[{}]}}", values.iter().map(json_bytes).collect::<Vec<_>>().join(", "))
}

fn is_int_array(v: &Value) -> bool {
    matches!(v, Value::IntArray(_))
}

/// The data of a region as the JSON members both the region table and the GPU table carry.
fn fixed_members(kept: Option<i64>, data: &dyn Fn(&Value) -> (i64, i64), f: &Fixed) -> String {
    let placed = if f.arrays.is_empty() {
        "[]".to_owned()
    } else {
        list(
            &|v: &Value| match v {
                Value::TextArray(t) => texts(t),
                _ => {
                    let (offset, length) = data(v);
                    format!("[{}, {}, {}]", offset, length, is_int_array(v))
                }
            },
            &f.arrays,
        )
    };
    let uniforms = list(
        &|(index, v): &(usize, Value)| {
            let integer = f.view.code.iter().any(|i| matches!(region_instruction(i), region::Instruction::IntUniform(slot) if *slot == *index as i64));
            match (integer, v) {
                (_, Value::TextArray(t)) => texts(t),
                (_, Value::Text(t)) => format!("{{\"text\":{}}}", json_bytes(t)),
                (_, Value::IntArray(_) | Value::Floats(1..=4, _)) => {
                    let (offset, length) = data(v);
                    format!("{{\"table\":[{},{},{}]}}", offset, length, is_int_array(v))
                }
                (true, Value::Int(n)) => format!("[{}]", json_bytes(n.to_string().as_bytes())),
                _ => list(&|x: &f64| number(*x), &uniform_components(v)),
            }
        },
        &f.uniforms.iter().cloned().enumerate().collect::<Vec<_>>(),
    );
    let frame = list(&|(slot, name, integer): &(i64, String, bool)| format!("[{}, {}, {}]", slot, json_bytes(name.as_bytes()), integer), &f.frame);
    format!(
        "\"count\": {}, {}\"inputs\": {}, \"uniforms\": {}, \"frame\": {}",
        f.count,
        match kept { Some(n) => format!("\"kept\": {n}, "), None => String::new() },
        placed,
        uniforms,
        frame
    )
}

/// A region the web target can run as a kernel: a static count of at least 1,024 and nothing the
/// target refuses.
fn web_kernel(f: &Fixed) -> Option<wgsl::Wgsl> {
    if f.count >= 1024 { target::kernel(&f.packed, Some(f.count)).ok() } else { None }
}

struct RegionEntry {
    prefix: String,
    fixed: Fixed,
    kernel: Option<wgsl::Wgsl>,
    width: i64,
}

fn region_entry(fast: bool, data: &dyn Fn(&Value) -> (i64, i64), r: &C::RegionT) -> R<RegionEntry> {
    if !fixed_region(r) {
        // bound per call: the frame's binding plan carries its sources; the table holds the template
        let packed = r.clone();
        let view = host::view(&packed);
        let source = js::region(fast, &packed)?;
        let width = view.outputs[0].len() as i64;
        let f = Fixed { packed, view, arrays: Vec::new(), uniforms: Vec::new(), count: 0, frame: Vec::new() };
        return Ok(RegionEntry { prefix: format!("{{\"source\": {}", kernel_table::json_string(&source)), fixed: f, kernel: None, width });
    }
    let f = fixed_inputs(r)?;
    if r.result != region::RegionResult::Collect || f.view.outputs.len() != 1 {
        return Err(err(
            "E_WEB_REGION",
            "The web export runs collecting single-output regions only; this sum or fold needs a later release.",
            span_of_region(r),
        ));
    }
    let mut skip: Vec<i64> = r.skip.iter().copied().filter(|i| *i >= 0 && *i < f.count).collect();
    skip.sort();
    skip.dedup();
    let skipped = skip.len() as i64;
    let textual = f.view.code.iter().any(region::text_register);
    let source = js::region(fast, &f.packed)?;
    let members = fixed_members(Some(f.count - skipped), data, &f);
    let out0 = &f.view.outputs[0];
    let mut prefix = format!(
        "{{\"source\": {}, {}, \"length\": {}, \"width\": {}, \"integer\": {}",
        kernel_table::json_string(&source),
        members,
        f.view.code.len(),
        out0.len(),
        out0.len() == 1 && region::integer_register(&f.view.code[out0[0] as usize])
    );
    if textual {
        prefix.push_str(&format!(",\"text\":true,\"skip\":{}", list(&|i: &i64| i.to_string(), &skip)));
    }
    let kernel = if r.skip.is_empty() { web_kernel(&f) } else { None };
    let width = out0.len() as i64;
    Ok(RegionEntry { prefix, fixed: f, kernel, width })
}

/// A kernel the program dispatches: the regions it computes (one output each, in order) and their shared inputs.
struct Group {
    id: String,
    members: Vec<i64>,
    fixed: Fixed,
    wgsl: wgsl::Wgsl,
}

fn groups(fast: bool, program: &C::Program, entries: &[RegionEntry]) -> Vec<Group> {
    let width = |k: i64| entries[k as usize].kernel.as_ref().map(|_| entries[k as usize].width);
    let legal = |region: &C::RegionT| fixed_inputs(region).map(|f| web_kernel(&f).is_some()).unwrap_or(false);
    let (_, found) = core_draw::device_groups(&Default::default(), fast, false, program, &width, &legal);
    found
        .into_iter()
        .map(|group| {
            let k = group.members[0] as usize;
            let (fixed, w) = match group.members.as_slice() {
                [_] => (entries[k].fixed.clone(), entries[k].kernel.clone().unwrap()),
                _ => {
                    let fixed = fixed_inputs(&group.region).unwrap();
                    let w = web_kernel(&fixed).unwrap();
                    (fixed, w)
                }
            };
            Group { id: format!("k{k}"), members: group.members, fixed, wgsl: w }
        })
        .collect()
}

fn index_html(name: &str) -> String {
    format!(
        "<!doctype html>
<meta charset=\"utf-8\">
<title>{name}</title>
<style>html, body {{ margin: 0; height: 100%; background: #080a10; }} canvas {{ display: block; width: 100%; height: 100%; }}</style>
<!-- This page needs cross-origin isolation (Cross-Origin-Opener-Policy: same-origin and
     Cross-Origin-Embedder-Policy: require-corp) for its worker pool; `npx serve DIR` sends them from serve.json. -->
<canvas id=\"canvas\"></canvas>
<script type=\"module\">
import {{ start }} from \"./runtime/runtime.js\";
import * as program from \"./program.js\";
const data = await (await fetch(\"data.bin\")).arrayBuffer();
await start(document.getElementById(\"canvas\"), program, {{ data }});
</script>
"
    )
}

// ---- image producers ----

/// Image maps share the packed compiler with native images. One dummy UV is enough to compile the
/// body; WGSL derives the real UV from the pixel index.
fn image_arguments(canvas: bool, lowered: &Lowered, node: &E::Node, values: &[Value]) -> R<C::Program> {
    let mut results: Vec<i64> = Vec::new();
    let source = C::Source {
        site: node.site.clone(),
        owner: node.site.clone(),
        instance: node.inst,
        prefix: Vec::new(),
        iter: node.iter.iter().map(|i| C::Iteration::StaticIndex(*i)).collect(),
        span: Some(lowered.span.clone()),
    };
    fn residual(v: &Value) -> Option<value::ResidualId> {
        match v {
            Value::Residual(r) => Some(*r),
            Value::List(xs) => xs.iter().find_map(residual),
            v @ (Value::Record(_) | Value::Struct(_)) => v.geo_fields().unwrap().iter().find_map(|(_, v)| residual(v)),
            _ => None,
        }
    }
    let root = values.iter().find_map(residual).or(lowered.root);
    let mut program = core_lower::lower_draw(None, root, &mut |ctx: &mut dyn DrawContext| {
        for (i, v) in values.iter().enumerate() {
            let dimension = if i == 0 { "width" } else { "height" };
            let n = if canvas && i < 2 && !node.args.iter().any(|(k, _)| k == dimension) {
                let n = ctx.local(&Ty::Int);
                ctx.emit(&source, C::StatementNode::Assign(n, C::Expr::Frame(if i == 0 { C::FrameField::Width } else { C::FrameField::Height })));
                n
            } else {
                ctx.value(&source, v)?
            };
            results.push(n);
        }
        Ok(())
    })
    .map_err(|name| err("E_WEB_IMAGE", &format!("Image arguments do not lower to the core: {name}"), Some(lowered.span.clone())))?;
    program.results = results;
    Ok(program)
}

fn node_arg(node: &E::Node, name: &str, fallback: Value) -> Value {
    node.args.iter().find(|(k, _)| k == name).map(|(_, v)| v.clone()).unwrap_or(fallback)
}

struct MapEntry {
    args: C::Program,
    animated: bool,
    frames: Vec<(i64, String)>,
    wgsl: wgsl::Wgsl,
}

fn map_entry(lowered: &Lowered, id: usize) -> R<MapEntry> {
    let span = lowered.span.clone();
    let refuse = |m: &str| Err(err("E_WEB_IMAGE", m, Some(span.clone())));
    let node = &lowered.plan.nodes[id];
    match node_arg(node, "function", Value::Int(0)) {
        Value::Fn(f) => {
            let signature = FnSignature { params: vec![Ty::Vec2], result: Ty::Vec4 };
            let pixels = E::host::map_function(Some((node.inst, node.site.clone(), node.iter.clone())), None, &signature, f, &[Value::Floats(2, Arc::new(vec![0.5, 0.5]))])?;
            match pixels {
                Value::Residual(r) => {
                    let view = E::residual::residual_view(r)?;
                    let packed = packed::compile_result(&packed::CompileOptions::default(), r, &view.term)
                        .map_err(|mut ds| if ds.is_empty() { err("E_WEB_IMAGE", "Image kernel did not compile.", None) } else { ds.remove(0) })?;
                    if let Some(d) = target::region(&packed, None).into_iter().next() {
                        return Err(d);
                    }
                    let w = wgsl::kernel(true, &packed)?;
                    let mut values = vec![node_arg(node, "width", Value::Int(256)), node_arg(node, "height", Value::Int(256))];
                    values.extend(packed.uniforms.iter().cloned());
                    let args = image_arguments(false, &Lowered { program: lowered.program.clone(), span: lowered.span.clone(), root: Some(r), plan: lowered.plan.clone() }, node, &values)?;
                    let animated = values.iter().any(E::frame_dependent) || packed.code.iter().any(|i| matches!(i, region::Instruction::Frame(_)));
                    let frames: Vec<(i64, String)> = packed
                        .code
                        .iter()
                        .enumerate()
                        .filter_map(|(slot, i)| if let region::Instruction::Frame(n) = i { Some((slot as i64, n.clone())) } else { None })
                        .collect();
                    if let Some((_, name)) = frames.iter().find(|(_, n)| !FRAME_FACTS.contains(&n.as_str())) {
                        return refuse(&format!("Image kernel reads an unavailable frame fact: {name}"));
                    }
                    Ok(MapEntry { args, animated, frames, wgsl: w })
                }
                _ => refuse("Image map did not produce a packed pixel body."),
            }
        }
        _ => refuse("Image map needs a pixel function."),
    }
}

// ---- the export ----

struct Export<'a> {
    lowered: &'a Lowered,
    decode_image: &'a dyn Fn(&str) -> Result<(i64, i64, Vec<u8>), String>,
    fast: bool,
    store: Shared,
    /// (table entry, "// kernel id\n" ^ source)
    kernels: Vec<(String, String)>,
    modules: Vec<(String, String)>,
    imports: Vec<String>,
    states: HashMap<String, i64>,
    images: HashMap<i64, (bool, String)>,
    order: Vec<i64>,
    assets: Vec<(i64, i64, i64, Vec<u8>)>,
}

fn image_ids(program: &C::Program) -> Vec<i64> {
    let mut ids: Vec<i64> = all_statements(&program.body)
        .iter()
        .filter_map(|s| match &s.node {
            C::StatementNode::Draw(C::Draw::Image { source: (_, id), .. }) => Some(*id),
            _ => None,
        })
        .collect();
    ids.sort();
    ids.dedup();
    ids
}

fn live(program: &C::Program) -> bool {
    !program.state_slots.is_empty()
        || all_statements(&program.body).iter().any(|s| matches!(s.node, C::StatementNode::Assign(_, C::Expr::Frame(_))))
        || program.regions.iter().any(|r| r.code.iter().any(|i| matches!(i, region::Instruction::Frame(_))))
}

impl Export<'_> {
    fn kernel(&mut self, id: &str, w: &wgsl::Wgsl) {
        self.kernels.push((kernel_table::entry(None, id, w), format!("// kernel {id}\n{}", w.source)));
    }

    fn emit_program(&mut self, prefix: &str, program: &C::Program) -> R<String> {
        let fast = self.fast;
        let slots: Vec<i64> = program
            .state_slots
            .iter()
            .map(|slot| {
                let mut path = slot.prefix.clone();
                path.extend(slot.zone.iter().cloned());
                let key = format!("{:?}", (slot.source.instance, path, slot.plane));
                let next = self.states.len() as i64;
                *self.states.entry(key).or_insert(next)
            })
            .collect();
        let data = self.store.clone();
        let place = move |v: &Value| store(&data, v);
        let mut entries: Vec<RegionEntry> = Vec::new();
        for r in program.regions.iter() {
            entries.push(region_entry(fast, &place, r)?);
        }
        // fast precision: a region bound per call may run on the device (a resident fold's levels), from its fast kernel
        let mut fast_kernels: Vec<Option<String>> = Vec::new();
        for (k, r) in program.regions.iter().enumerate() {
            if !fast || fixed_region(r) {
                fast_kernels.push(None);
            } else {
                match wgsl::kernel_fast(r) {
                    Ok(w) => {
                        let id = format!("{prefix}f{k}");
                        self.kernel(&id, &w);
                        fast_kernels.push(Some(id));
                    }
                    Err(_) => fast_kernels.push(None),
                }
            }
        }
        // a mark set's attributes go to the device from a fixed kernel or, in fast precision, a region bound per call
        let (good, _) = core_draw::device_regions(program, false, &|k| {
            let k = k as usize;
            if entries[k].kernel.is_some() || fast_kernels[k].is_some() { Some(entries[k].width) } else { None }
        });
        let mut found = groups(fast, program, &entries);
        for g in found.iter_mut() {
            g.id = format!("{prefix}{}", g.id);
        }
        let mut group_of: HashMap<i64, (usize, i64)> = HashMap::new();
        for (gi, g) in found.iter().enumerate() {
            for (output, k) in g.members.iter().enumerate() {
                group_of.insert(*k, (gi, output as i64));
            }
        }
        let slot_arc = Arc::new(slots);
        let fk = Arc::new(fast_kernels.clone());
        let gd = Arc::new(good);
        let storing = self.store.clone();
        let options = js::FrameOptions {
            fast,
            kernel: Some(Arc::new(move |k| fk.get(k as usize).cloned().flatten())),
            state_slot: Some(Arc::new(move |k| slot_arc[k as usize])),
            gpu: Some(Arc::new(move |k| gd.get(k as usize).copied().unwrap_or(false))),
            store: Some(Arc::new(move |values: &[f64]| {
                let (offset, length) = store(&storing, &Value::Floats(1, Arc::new(values.to_vec())));
                format!("new Float64Array(rt.data, {offset}, {length})")
            })),
            ..Default::default()
        };
        let text = js::frame(&options, program)?;
        let region_text = |k: usize, e: &RegionEntry| match group_of.get(&(k as i64)) {
            Some((gi, output)) => format!("{}, \"kernel\": \"{}\", \"gpu\": {{\"output\": {}}}}}", e.prefix, found[*gi].id, output),
            None => format!(
                "{}, \"kernel\": {}}}",
                e.prefix,
                if e.kernel.is_some() { format!("\"{prefix}k{k}\"") } else { "null".to_owned() }
            ),
        };
        let regions = format!(
            "export const regions = [{}];\n",
            entries
                .iter()
                .enumerate()
                .map(|(k, e)| {
                    let t = region_text(k, e);
                    if prefix.is_empty() {
                        t
                    } else {
                        format!("{{\"id\": {}, {}", kernel_table::json_string(&format!("{prefix}r{k}")), &t[1..])
                    }
                })
                .collect::<Vec<_>>()
                .join(",\n  ")
        );
        let shared = self.store.clone();
        let table = if found.is_empty() {
            String::new()
        } else {
            format!(
                "export const gpu = {{{}}};\n",
                found
                    .iter()
                    .map(|g| format!("\"{}\": {{{}}}", g.id, fixed_members(None, &|v: &Value| shared_data(&shared, v), &g.fixed)))
                    .collect::<Vec<_>>()
                    .join(",\n  ")
            )
        };
        for (k, e) in entries.iter().enumerate() {
            match group_of.get(&(k as i64)) {
                Some((gi, _)) => {
                    if found[*gi].members[0] == k as i64 {
                        let (id, w) = (found[*gi].id.clone(), found[*gi].wgsl.clone());
                        self.kernel(&id, &w);
                    }
                }
                None => {
                    if let Some(w) = &e.kernel {
                        self.kernel(&format!("{prefix}k{k}"), w);
                    }
                }
            }
        }
        Ok(format!("{text}\n{regions}{table}"))
    }

    fn emit_module(&mut self, name: &str, program: &C::Program) -> R<String> {
        let js = self.emit_program(&format!("{name}:"), program)?;
        let identity = hash::digest(js.as_bytes());
        let store = self.store.lock().unwrap();
        let a = hash::bytes((identity >> 64) as u64, &store.data);
        let b = hash::bytes(identity as u64, &store.data);
        let file = format!("{name}-{a:016x}{b:016x}.js");
        drop(store);
        self.modules.push((file.clone(), js));
        self.imports.push(format!("import * as {name} from \"./{file}\";\n"));
        Ok(name.to_owned())
    }

    fn image(&mut self, active: &[i64], id: i64) -> R<()> {
        let lowered = self.lowered;
        let refuse = |m: &str| Err(err("E_WEB_IMAGE", m, Some(lowered.span.clone())));
        if active.contains(&id) {
            return refuse("An image/render dependency cycle cannot be exported.");
        }
        if self.images.contains_key(&id) {
            return Ok(());
        }
        if self.images.len() + active.len() >= 64 {
            return refuse("A web workspace owns at most 64 images.");
        }
        let node = &lowered.plan.nodes[id as usize];
        let name = format!("image{id}");
        let (animated, metadata) = match node.kind.as_str() {
            "image/map" => {
                let m = map_entry(lowered, id as usize)?;
                let module_name = self.emit_module(&name, &m.args)?;
                self.kernel(&name, &m.wgsl);
                let frames = list(&|(slot, fact): &(i64, String)| format!("[{}, {}]", slot, kernel_table::json_string(fact)), &m.frames);
                (m.animated || live(&m.args), format!("kind: 'map', kernel: '{name}', args: {module_name}, frame: {frames}"))
            }
            "image/noise" => {
                let args = image_arguments(
                    false,
                    lowered,
                    node,
                    &[
                        node_arg(node, "width", Value::Int(256)),
                        node_arg(node, "height", Value::Int(256)),
                        node_arg(node, "frequency", node_arg(node, "freq", Value::Float(0.02))),
                        node_arg(node, "seed", Value::Int(0)),
                    ],
                )?;
                let module_name = self.emit_module(&name, &args)?;
                (live(&args), format!("kind: 'noise', args: {module_name}"))
            }
            "image/render" => {
                let (drawing, _) = core_draw::lower(None, &lowered.plan, &node_arg(node, "drawing", Value::Int(0)))
                    .map_err(|m| err("E_WEB_IMAGE", &format!("Image drawing does not lower to the core: {m}"), Some(lowered.span.clone())))?;
                let mut next = active.to_vec();
                next.insert(0, id);
                for child in image_ids(&drawing) {
                    self.image(&next, child)?;
                }
                let args = image_arguments(true, lowered, node, &[node_arg(node, "width", Value::Int(0)), node_arg(node, "height", Value::Int(0))])?;
                let args_name = self.emit_module(&name, &args)?;
                let drawing_name = self.emit_module(&format!("drawing{id}"), &drawing)?;
                let animated = live(&args) || live(&drawing) || image_ids(&drawing).iter().any(|c| self.images[c].0);
                (animated, format!("kind: 'render', args: {args_name}, drawing: {drawing_name}"))
            }
            "image/load" => match node_arg(node, "path", Value::Int(0)) {
                Value::Text(path) => {
                    let path = String::from_utf8_lossy(&path).into_owned();
                    let (width, height, pixels) = (self.decode_image)(&path).map_err(|e| {
                        err("E_WEB_IMAGE", &format!("Cannot bundle image {:?}: {}", path, e), Some(lowered.span.clone()))
                    })?;
                    let expected = usize::try_from(width).ok().and_then(|width| {
                        usize::try_from(height).ok().and_then(|height| width.checked_mul(height)?.checked_mul(4))
                    });
                    if width <= 0 || height <= 0 || expected != Some(pixels.len()) {
                        return Err(err("E_WEB_IMAGE", &format!("Cannot bundle image {path:?}: decoder must return positive dimensions and exactly width × height × 4 RGBA bytes."), Some(lowered.span.clone())));
                    }
                    self.assets.push((id, width, height, pixels));
                    (false, "kind: 'load'".to_owned())
                }
                _ => return refuse("A standalone export needs image/load paths known at export time."),
            },
            kind => return refuse(&format!("Unknown image producer: {kind}")),
        };
        self.images.insert(id, (animated, metadata));
        self.order.push(id);
        Ok(())
    }
}

pub fn files(d: &doc::T, lowered: &Lowered, decode_image: &dyn Fn(&str) -> Result<(i64, i64, Vec<u8>), String>) -> R<Files> {
    // the workspace's precision, as the editor reads it: fast unless (settings/config :precision "exact")
    let fast = editor_document::lowering::plan_window(d, &lowered.plan).map(|w| w.fast).unwrap_or(true);
    let mut e = Export {
        lowered,
        decode_image,
        fast,
        store: Arc::new(Mutex::new(Store { data: Vec::new(), buckets: HashMap::new() })),
        kernels: Vec::new(),
        modules: Vec::new(),
        imports: Vec::new(),
        states: HashMap::new(),
        images: HashMap::new(),
        order: Vec::new(),
        assets: Vec::new(),
    };
    for id in image_ids(&lowered.program) {
        e.image(&[], id)?;
    }
    let js = e.emit_program("", &lowered.program)?;
    // Native-decoded bytes preserve every supported codec; store them after all f64 constants so every
    // constant view remains aligned to eight bytes.
    let assets = std::mem::take(&mut e.assets);
    for (id, width, height, pixels) in assets.into_iter() {
        let offset = {
            let mut s = e.store.lock().unwrap();
            while s.data.len() % 8 != 0 {
                s.data.push(0);
            }
            let offset = s.data.len();
            s.data.extend_from_slice(&pixels);
            offset
        };
        let (animated, metadata) = e.images.remove(&id).unwrap();
        e.images.insert(id, (animated, format!("{metadata}, offset: {offset}, length: {}, width: {width}, height: {height}", pixels.len())));
    }
    let image_table = if e.images.is_empty() {
        String::new()
    } else {
        format!(
            "export const images = {{{}}};\n",
            e.order
                .iter()
                .map(|id| {
                    let (animated, metadata) = &e.images[id];
                    format!("'{id}': {{{metadata}, animated: {animated}}}")
                })
                .collect::<Vec<_>>()
                .join(",\n  ")
        )
    };
    let assets = web_runtime::assets()?;
    let entries: Vec<String> = e.kernels.iter().map(|k| k.0.clone()).collect();
    let mut out: Files = vec![
        ("program.js".to_owned(), format!("{}{}{}", e.imports.concat(), js, image_table).into_bytes()),
        ("program.map".to_owned(), js::frame_map(&lowered.program).into_bytes()),
        ("kernels.json".to_owned(), kernel_table::table(&assets.compact, &assets.dispatch, &entries).into_bytes()),
        ("kernels.wgsl".to_owned(), e.kernels.iter().map(|k| k.1.clone()).collect::<Vec<_>>().join("\n").into_bytes()),
        ("data.bin".to_owned(), e.store.lock().unwrap().data.clone()),
        ("index.html".to_owned(), index_html(&doc::name(d)).into_bytes()),
        (
            "serve.json".to_owned(),
            "{\"headers\": [{\"source\": \"**/*\", \"headers\": [\n  {\"key\": \"Cross-Origin-Opener-Policy\", \"value\": \"same-origin\"},\n  {\"key\": \"Cross-Origin-Embedder-Policy\", \"value\": \"require-corp\"}]}]}\n"
                .as_bytes()
                .to_vec(),
        ),
    ];
    out.extend(e.modules.iter().map(|(f, t)| (f.clone(), t.clone().into_bytes())));
    out.extend(web_runtime::files(&assets).into_iter().map(|(f, t)| (format!("runtime/{f}"), t)));
    Ok(out)
}

/// Read text and check imports relative to the document file.
pub fn load(path: &str) -> (String, Result<doc::T, Vec<Diagnostic>>) {
    let read = |p: &str| std::fs::read(p).map(|b| String::from_utf8_lossy(&b).into_owned());
    let text = match read(path) {
        Ok(t) => t,
        Err(e) => return (String::new(), Err(vec![err("E_IMPORT", &e.to_string(), None)])),
    };
    let result = (|| {
        let relative = doc::import_paths(&text)?;
        let dir = std::path::Path::new(path).parent().map(|p| p.to_path_buf()).unwrap_or_default();
        let mut imports = Vec::new();
        for r in relative {
            match read(&dir.join(&r).to_string_lossy()) {
                Ok(t) => imports.push((r, t)),
                Err(e) => return Err(vec![err("E_IMPORT", &e.to_string(), None)]),
            }
        }
        let catalog = editor_document::kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).map_err(|d| vec![d])?;
        let mut ops = flow_sop::operators::all().to_vec();
        if let Some(op) = ops.iter_mut().find(|op| op.name == "table/load") { *op = flow_check::table::operator(dir); }
        let options = doc::TextOptions { ops: Some(Arc::new(ops)), imports, ..Default::default() };
        doc::check_text(&options, &catalog, &text).map(|(t, _)| t)
    })();
    (text, result)
}
