//! Port of lib/flow_ir/core_draw.ml: the lowering rules of the draw operators, and the device
//! placement and sibling fusion of a lowered program's mark sets. A rule names the inputs it needs,
//! supplies the defaults (white ink and fill, width 1) and writes statements; every check of a
//! value is the executor's. Argument order of the OCaml record/list literals (right to left) decides
//! which default numbers its constant first; it is kept here.
#![allow(non_camel_case_types, non_snake_case)]

use crate::core_ir as C;
use crate::core_lower::{self, DrawContext, RegionAllowed};
use crate::{core_fuse, packed};
use flow_check::workspace::Paths;
use flow_eval as E;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{ResidualId, Value, source_value};
use std::collections::HashMap;

/// Regions read exclusively as compatible mark-set attributes, and their per-batch groups in
/// argument order. `width` supplies each target's eligible single-output region width. Other
/// attributes can remain on the CPU.
pub fn device_regions(program: &C::Program, width: &dyn Fn(i64) -> Option<i64>) -> (Vec<bool>, Vec<Vec<i64>>) {
    let widths: Vec<Option<i64>> = (0..program.regions.len() as i64).map(width).collect();
    let n = program.locals.len();
    let mut uses = vec![0i64; n];
    let mut writes = vec![0i64; n];
    let mut attributes = vec![0i64; n];
    let mut producers = vec![-1i64; n];
    let mut invokes: Vec<(i64, i64)> = Vec::new();
    let mut batches: Vec<(C::Mark, Vec<(String, i64)>)> = Vec::new();
    C::iter(
        &mut |statement| {
            for l in C::reads(statement) { uses[l as usize] += 1; }
            for l in C::writes(statement) { writes[l as usize] += 1; }
            match &statement.node {
                C::StatementNode::Invoke { region, result, .. } => {
                    producers[*result as usize] = *region;
                    invokes.push((*region, *result));
                }
                C::StatementNode::Draw(C::Draw::Batch { mark, args }) => batches.push((*mark, args.clone())),
                _ => {}
            }
        },
        &program.body,
    );
    for r in &program.results { uses[*r as usize] += 1; }
    fn attribute(mark: C::Mark, name: &str, width: i64) -> bool {
        use C::Mark::*;
        matches!(
            (mark, name, width),
            (Circle | Rect, "positions", 2 | 3)
                | (Line, "from" | "to", 2 | 3)
                | (Circle, "radius", 1)
                | (Rect, "sizes", 2 | 3)
                | (Circle | Rect, "fill" | "stroke", 3)
                | (Line, "color", 3)
        )
    }
    for (mark, args) in &batches {
        for (name, local) in args {
            let region = producers[*local as usize];
            if region >= 0 {
                if let Some(width) = widths[region as usize] {
                    if attribute(*mark, name, width) { attributes[*local as usize] += 1; }
                }
            }
        }
    }
    let mut eligible = vec![false; program.regions.len()];
    for (region, _) in &invokes { eligible[*region as usize] = widths[*region as usize].is_some(); }
    for (region, local) in &invokes {
        let l = *local as usize;
        if writes[l] != 1 || uses[l] == 0 || uses[l] != attributes[l] { eligible[*region as usize] = false; }
    }
    let mut out = Vec::new();
    for (_, args) in &batches {
        let mut regions: Vec<i64> = Vec::new();
        for (_, local) in args {
            let region = producers[*local as usize];
            if region >= 0 && eligible[region as usize] && !regions.contains(&region) { regions.push(region); }
        }
        if !regions.is_empty() { out.push(regions); }
    }
    (eligible, out)
}

#[derive(Clone, Debug)]
pub struct Group {
    pub members: Vec<i64>,
    pub results: Vec<i64>,
    pub args: Vec<i64>,
    pub region: C::RegionT,
    /// runs where its first member is (a resident fold's siblings), else where its last is
    pub at_first: bool,
}

fn visit(statement: &C::Statement, f: &mut dyn FnMut(&C::Statement)) {
    f(statement);
    for child in C::children(statement) { visit(child, f); }
}

pub fn static_invokes(program: &C::Program) -> Vec<bool> {
    let n = program.locals.len();
    let mut writes = vec![0i64; n];
    let mut nested = vec![false; n];
    let mut mark = |top: bool, statement: &C::Statement| {
        let mut slots = C::writes(statement);
        if let C::StatementNode::State_write { value, .. } = &statement.node { slots.push(*value); }
        for slot in slots {
            writes[slot as usize] += 1;
            if !top { nested[slot as usize] = true; }
        }
    };
    for statement in &program.body {
        mark(true, statement);
        for child in C::children(statement) { visit(child, &mut |s| mark(false, s)); }
    }
    let mut is_static = vec![false; n];
    let once = |a: i64, writes: &[i64], nested: &[bool]| writes[a as usize] == 1 && !nested[a as usize];
    fn constant(e: &C::Expr, is_static: &[bool]) -> bool {
        match e {
            C::Expr::Constant(_) => true,
            C::Expr::Local(a) | C::Expr::Field(a, _) => is_static[*a as usize],
            C::Expr::Prim(op, args) => *op != C::Primitive::Rand && args.iter().all(|(_, a)| is_static[*a as usize]),
            C::Expr::Vec(ns) | C::Expr::Items(ns) => ns.iter().all(|a| is_static[*a as usize]),
            C::Expr::Fields(fs) => fs.iter().all(|(_, a)| is_static[*a as usize]),
            C::Expr::Frame(_) | C::Expr::Call_index(_) | C::Expr::Source(_) | C::Expr::Host_source { .. } | C::Expr::Source_descriptor { .. } => false,
        }
    }
    for s in &program.body {
        match &s.node {
            C::StatementNode::Assign(a, e) if once(*a, &writes, &nested) && constant(e, &is_static) => is_static[*a as usize] = true,
            C::StatementNode::Invoke { region, args, result }
                if once(*result, &writes, &nested) && args.iter().all(|a| is_static[*a as usize]) =>
            {
                let r = &program.regions[*region as usize];
                is_static[*result as usize] = !E::frame_dependent(&Value::Residual(r.residual))
                    && !E::state_dependent(&Value::Residual(r.residual))
                    && r.sources.iter().all(|(source, _)| !E::frame_dependent(&Value::Residual(*source)));
            }
            _ => {}
        }
    }
    let mut invokes = vec![false; n];
    for s in &program.body {
        if let C::StatementNode::Invoke { result, .. } = &s.node { invokes[*result as usize] = is_static[*result as usize]; }
    }
    invokes
}

/// What a statement block holds, for the statements groups may cross.
#[derive(Clone, Copy)]
enum Item { Invoke(i64), Setup { local: i64 }, Other }

struct Info { block: usize, index: usize, args: Vec<i64>, result: i64 }

/// `writer`: the block and index of the statement that last wrote each local
fn scan_block(statements: &[C::Statement], blocks: &mut Vec<Vec<Item>>, info: &mut [Option<Info>], occurrences: &mut [i64], writes: &mut [i64], writer: &mut [Option<(usize, usize)>]) {
    let id = blocks.len();
    blocks.push(Vec::new());
    let mut items = Vec::new();
    for (index, s) in statements.iter().enumerate() {
        for local in C::writes(s) {
            writes[local as usize] += 1;
            writer[local as usize] = Some((id, index));
        }
        if let C::StatementNode::Invoke { region, args, result } = &s.node {
            occurrences[*region as usize] += 1;
            info[*region as usize] = Some(Info { block: id, index, args: args.clone(), result: *result });
        }
        items.push(match &s.node {
            C::StatementNode::Invoke { region, .. } => Item::Invoke(*region),
            C::StatementNode::Assign(local, C::Expr::Constant(_) | C::Expr::Frame(_) | C::Expr::Local(_)) => Item::Setup { local: *local },
            _ => Item::Other,
        });
        match &s.node {
            C::StatementNode::If { yes, no, .. } => { scan_block(yes, blocks, info, occurrences, writes, writer); scan_block(no, blocks, info, occurrences, writes, writer); }
            C::StatementNode::Protect { body, handler, .. } => { scan_block(body, blocks, info, occurrences, writes, writer); scan_block(handler, blocks, info, occurrences, writes, writer); }
            C::StatementNode::Counted { init, body, .. } => { scan_block(init, blocks, info, occurrences, writes, writer); scan_block(body, blocks, info, occurrences, writes, writer); }
            C::StatementNode::Iterate { body, .. } => scan_block(body, blocks, info, occurrences, writes, writer),
            C::StatementNode::State_read { init, .. } => scan_block(init, blocks, info, occurrences, writes, writer),
            C::StatementNode::Diagnose_rows { handler, .. } => scan_block(handler, blocks, info, occurrences, writes, writer),
            _ => {}
        }
    }
    blocks[id] = items;
}

/// Device placement and sibling fusion, shared by native drawing and web export. Outputs follow
/// invocation order. Groups preserve scope, rate, precision and authored error order; incompatible
/// groups remain single-output regions.
/// Groups share a statement block and cross only infallible scalar setup. Delaying the
/// earlier invocations to the last therefore preserves branches, side effects and errors.
pub fn device_groups(approx: &Paths, fast: bool, program: &C::Program, width: &dyn Fn(i64) -> Option<i64>, legal: &dyn Fn(&C::RegionT) -> bool) -> (Vec<bool>, Vec<Group>) {
    let (eligible, batches) = device_regions(program, width);
    if batches.is_empty() && program.regions.len() < 2 { return (eligible, Vec::new()); }
    let mut info: Vec<Option<Info>> = (0..program.regions.len()).map(|_| None).collect();
    let mut occurrences = vec![0i64; program.regions.len()];
    let mut writes = vec![0i64; program.locals.len()];
    let mut blocks: Vec<Vec<Item>> = Vec::new();
    let mut writer: Vec<Option<(usize, usize)>> = vec![None; program.locals.len()];
    scan_block(&program.body, &mut blocks, &mut info, &mut occurrences, &mut writes, &mut writer);
    let static_ = static_invokes(program);
    let rate = |k: i64| info[k as usize].as_ref().map(|i| static_[i.result as usize]).unwrap_or(false);
    let precision = |k: i64| {
        fast || {
            let (_, site, _) = packed::site(&program.regions[k as usize]);
            approx.contains(&site)
        }
    };
    let single = |k: i64| {
        let i = info[k as usize].as_ref().expect("invoked region");
        Group { members: vec![k], results: vec![i.result], args: i.args.clone(), region: (*program.regions[k as usize]).clone(), at_first: false }
    };
    // `hoisted`: the group runs where its first member is, so every member's arguments are written
    // once before it in its block (or are inputs), whatever stands between the members; else
    // where its last is, past nothing but setup
    let merge = |members: &[i64], hoisted: bool| -> Option<Group> {
        let first = info[members[0] as usize].as_ref().unwrap().block;
        if !members.iter().all(|k| occurrences[*k as usize] == 1 && info[*k as usize].as_ref().map(|i| i.block == first).unwrap_or(false)) {
            return None;
        }
        let mut members = members.to_vec();
        members.sort_by_key(|k| info[*k as usize].as_ref().unwrap().index);
        let mut args: Vec<i64> = members.iter().flat_map(|k| info[*k as usize].as_ref().unwrap().args.clone()).collect();
        args.sort();
        args.dedup();
        let mut indices: HashMap<i64, i64> = HashMap::new();
        for (index, local) in args.iter().enumerate() { indices.insert(*local, index as i64); }
        let at = |k: i64| info[k as usize].as_ref().unwrap().index;
        let (first_index, last_index) = (at(members[0]), at(*members.last().unwrap()));
        let mut safe = true;
        if hoisted {
            safe = args.iter().all(|local| writes[*local as usize] <= 1 && writer[*local as usize].is_none_or(|(block, index)| block == first && index < first_index));
        }
        for index in (first_index..=last_index).filter(|_| !hoisted) {
            match blocks[first][index] {
                Item::Invoke(region) if members.contains(&region) => {}
                Item::Setup { local } if !args.contains(&local) || writes[local as usize] == 1 => {}
                _ => safe = false,
            }
        }
        if !safe { return None; }
        let mut normalized = Vec::new();
        for k in &members {
            let locals = &info[*k as usize].as_ref().unwrap().args;
            let values: Vec<Value> = locals
                .iter()
                .map(|local| source_value("$core/argument", program.locals[*local as usize].clone(), vec![("index".into(), Value::Int(indices[local]))]))
                .collect();
            match packed::host::bind_arguments(&program.regions[*k as usize], &values, Some(false)) {
                Ok(region) => normalized.push(region),
                Err(_) => return None,
            }
        }
        let region = core_fuse::merge(&normalized)?;
        // a resident fold runs in fast precision and declines what its device refuses
        if !(if hoisted { packed::gpu_refusals(true, true, &region).is_empty() } else { legal(&region) }) { return None; }
        let results = members.iter().map(|k| info[*k as usize].as_ref().unwrap().result).collect();
        Some(Group { members, args, region, results, at_first: hoisted })
    };
    let mut taken = vec![false; program.regions.len()];
    let mut groups = Vec::new();
    for members in &batches {
        let members: Vec<i64> = members.iter().copied().filter(|k| !taken[*k as usize]).collect();
        for k in &members { taken[*k as usize] = true; }
        for (is_static, approximate) in [(true, false), (true, true), (false, false), (false, true)] {
            let selected: Vec<i64> = members.iter().copied().filter(|k| rate(*k) == is_static && precision(*k) == approximate).collect();
            match selected.as_slice() {
                [] => {}
                [k] => groups.push(single(*k)),
                _ => match merge(&selected, false) {
                    Some(group) => groups.push(group),
                    None => for k in &selected { groups.push(single(*k)); },
                },
            }
        }
    }
    // Sibling regions of one count in a block run as one dispatch with an output each (a
    // resident fold's `positions`, `normals`, `colors` over one range), where the first runs.
    // ponytail: a merge attempt per open group a region meets, quadratic in a block's regions;
    // index the groups by count if blocks ever hold hundreds
    for block in &blocks {
        let mut open: Vec<(Vec<i64>, Option<Group>)> = Vec::new();
        for item in block {
            let Item::Invoke(k) = *item else { continue };
            if taken[k as usize] || occurrences[k as usize] != 1 {
                continue;
            }
            let joined = open.iter_mut().any(|(run, last)| {
                if rate(run[0]) != rate(k) || precision(run[0]) != precision(k) {
                    return false;
                }
                let mut members = run.clone();
                members.push(k);
                match merge(&members, true) {
                    Some(group) => {
                        run.push(k);
                        *last = Some(group);
                        true
                    }
                    None => false,
                }
            });
            if !joined {
                open.push((vec![k], None));
            }
        }
        for group in open.into_iter().filter_map(|(_, last)| last) {
            for k in &group.members { taken[*k as usize] = true; }
            groups.push(group);
        }
    }
    (eligible, groups)
}

fn find_residual(v: &Value) -> Option<ResidualId> {
    match v {
        Value::Residual(r) => Some(*r),
        Value::List(xs) => xs.iter().find_map(find_residual),
        v @ (Value::Record(_) | Value::Struct(_)) => v.geo_fields().unwrap().iter().find_map(|(_, v)| find_residual(v)),
        _ => None,
    }
}

enum Stop { Missing(String), Refused(String) }

fn no_source() -> C::Source {
    C::Source { site: Vec::new(), owner: Vec::new(), instance: 0, prefix: Vec::new(), iter: Vec::new(), span: None }
}

fn fail(ctx: &mut dyn DrawContext, source: &C::Source, code: &str, message: &str) {
    let n = ctx.local(&Ty::Any);
    ctx.emit(source, C::StatementNode::Assign(n, C::Expr::Prim(C::Primitive::Fail(code.into(), message.into()), Vec::new())));
}

fn drawing(ctx: &mut dyn DrawContext, plan: &E::Plan, value: &Value) -> Result<(), String> {
    match value {
        Value::List(xs) => { for x in xs.iter() { drawing(ctx, plan, x)?; } Ok(()) }
        Value::Deferred(Ty::Named(name), id) if name == "drawing" && *id >= 0 && (*id as usize) < plan.nodes.len() => node(ctx, plan, &plan.nodes[*id as usize]),
        Value::Residual(_) => Err("draw_dynamic".into()),
        _ => { fail(ctx, &no_source(), "E_TYPE", "The canvas needs a Drawing value."); Ok(()) }
    }
}

fn node(ctx: &mut dyn DrawContext, plan: &E::Plan, n: &E::Node) -> Result<(), String> {
    let source = C::Source {
        site: n.site.clone(),
        owner: n.site.clone(),
        instance: n.inst,
        prefix: Vec::new(),
        iter: n.iter.iter().map(|i| C::Iteration::Static_index(*i)).collect(),
        span: None,
    };
    if n.ty != ty::drawing() {
        fail(ctx, &source, "E_TYPE", "The canvas needs a Drawing node.");
        return Ok(());
    }
    // The data inputs, in written order, as drawing.ml forces them.
    let data: Vec<&(String, Value)> = match n.kind.as_str() {
        "draw/merge" => Vec::new(),
        "draw/translate" | "draw/rotate" | "draw/scale" => n.args.iter().filter(|(k, _)| k != "drawing").collect(),
        "draw/image" => n.args.iter().filter(|(k, _)| k != "image").collect(),
        _ => n.args.iter().collect(),
    };
    let mut locals: Vec<(String, i64)> = Vec::new();
    for (k, v) in data { locals.push((k.clone(), ctx.value(&source, v)?)); }
    match rule(ctx, plan, n, &source, &locals) {
        Ok(()) => Ok(()),
        Err(Stop::Missing(message)) => { fail(ctx, &source, "E_DRAW_INPUT", &message); Ok(()) }
        Err(Stop::Refused(name)) => Err(name),
    }
}

fn rule(ctx: &mut dyn DrawContext, plan: &E::Plan, n: &E::Node, source: &C::Source, locals: &[(String, i64)]) -> Result<(), Stop> {
    let white = Value::Vec3(1., 1., 1.);
    let need = |key: &str| -> Result<i64, Stop> {
        locals.iter().find(|(k, _)| k == key).map(|(_, n2)| *n2).ok_or_else(|| Stop::Missing(format!("{} needs {}.", n.kind, key)))
    };
    let optional = |key: &str| locals.iter().find(|(k, _)| k == key).map(|(_, n2)| *n2);
    let refuse = |name: String| Stop::Refused(name);
    // `default` lowers its constant only when the input is absent
    fn default(ctx: &mut dyn DrawContext, source: &C::Source, locals: &[(String, i64)], key: &str, v: &Value) -> Result<i64, Stop> {
        match locals.iter().find(|(k, _)| k == key) {
            Some((_, n)) => Ok(*n),
            None => ctx.value(source, v).map_err(Stop::Refused),
        }
    }
    fn style(ctx: &mut dyn DrawContext, source: &C::Source, locals: &[(String, i64)], white: &Value) -> Result<Vec<(String, i64)>, Stop> {
        let fill = locals.iter().find(|(k, _)| k == "fill").map(|(_, n)| *n);
        let stroke = locals.iter().find(|(k, _)| k == "stroke").map(|(_, n)| *n);
        let fill = if fill.is_none() && stroke.is_none() { Some(ctx.value(source, white).map_err(Stop::Refused)?) } else { fill };
        let mut out = Vec::new();
        if let Some(n) = fill { out.push(("fill".to_owned(), n)); }
        if let Some(n) = stroke { out.push(("stroke".to_owned(), n)); }
        Ok(out)
    }
    fn args(pairs: &[(&str, i64)]) -> Vec<(String, i64)> { pairs.iter().map(|(k, n)| ((*k).to_owned(), *n)).collect() }
    let emit = |ctx: &mut dyn DrawContext, d: C::Draw| ctx.emit(source, C::StatementNode::Draw(d));
    let push = |ctx: &mut dyn DrawContext, transform: C::Transform, key: &str| -> Result<(), Stop> {
        let operand = need(key)?;
        emit(ctx, C::Draw::Push(transform, operand));
        match n.args.iter().find(|(k, _)| k == "drawing") {
            Some((_, v)) => drawing(ctx, plan, v).map_err(Stop::Refused)?,
            None => return Err(Stop::Missing(format!("{} needs drawing.", n.kind))),
        }
        emit(ctx, C::Draw::Pop);
        Ok(())
    };
    match n.kind.as_str() {
        "draw/background" => { let c = need("color")?; emit(ctx, C::Draw::Background(c)); }
        "draw/point" => {
            let at = need("at")?;
            let ink = default(ctx, source, locals, "color", &white)?;
            emit(ctx, C::Draw::Point { at, ink });
        }
        "draw/rect" => {
            let at = need("at")?;
            let size = need("size")?;
            let mut a = args(&[("at", at), ("size", size)]);
            a.extend(style(ctx, source, locals, &white)?);
            emit(ctx, C::Draw::Mark { mark: C::Mark::Rect, args: a });
        }
        "draw/circle" => {
            let at = need("at")?;
            let radius = need("radius")?;
            let mut a = args(&[("at", at), ("radius", radius)]);
            a.extend(style(ctx, source, locals, &white)?);
            emit(ctx, C::Draw::Mark { mark: C::Mark::Circle, args: a });
        }
        "draw/line" | "draw/lines" => {
            let from = need("from")?;
            let to = need("to")?;
            // the list literal is built right to left: width before color
            let width = default(ctx, source, locals, "width", &Value::Float(1.))?;
            let color = default(ctx, source, locals, "color", &white)?;
            let a = args(&[("from", from), ("to", to), ("color", color), ("width", width)]);
            if n.kind == "draw/line" { emit(ctx, C::Draw::Mark { mark: C::Mark::Line, args: a }); } else { emit(ctx, C::Draw::Batch { mark: C::Mark::Line, args: a }); }
        }
        "draw/circles" => {
            let positions = need("positions")?;
            // `[...] @ style ()`: the style lowers first
            let st = style(ctx, source, locals, &white)?;
            let radius = default(ctx, source, locals, "radius", &Value::Float(1.))?;
            let mut a = args(&[("positions", positions), ("radius", radius)]);
            a.extend(st);
            emit(ctx, C::Draw::Batch { mark: C::Mark::Circle, args: a });
        }
        "draw/rects" => {
            let positions = need("positions")?;
            let sizes = need("sizes")?;
            let mut a = args(&[("positions", positions), ("sizes", sizes)]);
            a.extend(style(ctx, source, locals, &white)?);
            emit(ctx, C::Draw::Batch { mark: C::Mark::Rect, args: a });
        }
        "draw/points" => {
            let positions = need("positions")?;
            let ink = default(ctx, source, locals, "color", &white)?;
            emit(ctx, C::Draw::Points { positions, ink });
        }
        "draw/text" => {
            let at = need("at")?;
            let text = need("text")?;
            // record fields are built right to left: size before ink
            let size = default(ctx, source, locals, "size", &Value::Int(16))?;
            let ink = default(ctx, source, locals, "color", &white)?;
            emit(ctx, C::Draw::Text_run { at, text, ink, size });
        }
        "draw/image" => {
            let image = match n.args.iter().find(|(k, _)| k == "image") {
                Some((_, Value::Deferred(ty, id))) => (ty.clone(), *id),
                Some(_) => return Err(refuse("draw_image".into())),
                None => return Err(Stop::Missing(format!("{} needs image.", n.kind))),
            };
            let angle = default(ctx, source, locals, "angle", &Value::Float(0.))?;
            let scale = default(ctx, source, locals, "scale", &Value::Float(1.))?;
            let at = default(ctx, source, locals, "at", &Value::Vec3(0., 0., 0.))?;
            emit(ctx, C::Draw::Image { source: image, at, scale, angle });
        }
        "draw/rounded_rect" => {
            let at = need("at")?;
            let size = need("size")?;
            let st = style(ctx, source, locals, &white)?;
            let radius = default(ctx, source, locals, "radius", &Value::Int(0))?;
            let mut a = args(&[("at", at), ("size", size), ("radius", radius)]);
            a.extend(st);
            emit(ctx, C::Draw::Path { path: C::PathKind::Rounded_rect, args: a });
        }
        "draw/polygon" => {
            let points = need("points")?;
            let mut a = args(&[("points", points)]);
            a.extend(style(ctx, source, locals, &white)?);
            emit(ctx, C::Draw::Path { path: C::PathKind::Polygon, args: a });
        }
        "draw/polyline" => {
            let points = need("points")?;
            let color = default(ctx, source, locals, "color", &white)?;
            emit(ctx, C::Draw::Path { path: C::PathKind::Polyline, args: args(&[("points", points), ("color", color)]) });
        }
        "draw/translate" => push(ctx, C::Transform::Translate, "offset")?,
        "draw/rotate" => push(ctx, C::Transform::Rotate, "angle")?,
        "draw/scale" => push(ctx, C::Transform::Scale, "factor")?,
        "draw/merge" => { for (_, v) in &n.args { drawing(ctx, plan, v).map_err(Stop::Refused)?; } }
        kind => fail(ctx, source, "E_DRAW_KIND", &format!("No native drawing implementation for {kind}.")),
    }
    let _ = optional;
    Ok(())
}

/// Lowering rules of the draw operators: a draw graph's plan and root value become a core program
/// of draw statements, with no result, in painter order. The second component is a residual to run
/// it with (state reads need one), absent when nothing is live. A drawing chosen by a live value is
/// refused as `"draw_dynamic"`, an image that is not a plan node as `"draw_image"`.
pub fn lower(region_allowed: Option<RegionAllowed>, plan: &E::Plan, value: &Value) -> Result<(C::Program, Option<ResidualId>), String> {
    let root = find_residual(value).or_else(|| plan.nodes.iter().find_map(|n| n.args.iter().find_map(|(_, v)| find_residual(v))));
    let program = core_lower::lower_draw(region_allowed, root, &mut |ctx| drawing(ctx, plan, value))?;
    Ok((program, root))
}

#[cfg(test)]
mod tests {
    use super::*;
    use flow_check::{catalog::Catalog, workspace as W};
    use flow_syntax::syntax;

    fn checked(body: &str) -> W::Workspace {
        let forms = syntax::parse(&format!("(workspace w (graph g :context draw {body}))")).expect("parse");
        match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
            (Some(ws), ds) if ds.is_empty() => ws,
            (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
        }
    }

    const PICTURES: [&str; 24] = [
        "(draw/background \"#14161c\")",
        "(draw/merge (draw/rect [1 2 0] [30 40 0]) (draw/rect [1 2 0] [3 4 0] :stroke \"#fff\") (draw/rect [0 0 0] [5 5 0] :fill \"#f00\" :stroke \"#00f\"))",
        "(draw/circle [10 20 0] 4)",
        "(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill \"#5aaaf0\")",
        "(draw/line [0 0 0] [5 5 0])",
        "(draw/line [0 0 0] [5 5 0] :color \"#0f0\" :width 3)",
        "(draw/point [3 4 0])",
        "(draw/point [3 4 0] :color \"#abc\")",
        "(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate (* t 2) (draw/scale 2 (draw/point [1 1 0])))))",
        "(draw/scale [2 3 0] (draw/background \"#000\"))",
        "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 4))] (draw/circles p))",
        "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 4)) r (map (fn [i] (* i 0.5)) (array/range 4))] (draw/circles p :radius r :stroke \"#fff\"))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/rects p [10 10 0] :fill \"#f00\"))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4)) c (map (fn [i] [(/ i 4) 0 0]) (array/range 4))] (draw/rects p [10 10 0] :fill c))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/lines p p :width 2))",
        "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/points p :color \"#1e1e1e\"))",
        "(draw/text [12 12 0] (str \"frame \" (frame/index)))",
        "(draw/text [12 12 0] \"hi\" :color \"#abc\" :size 20)",
        "(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill \"#123\")",
        "(draw/rounded_rect [4 5 0] [60 40 0])",
        "(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke \"#fff\")",
        "(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color \"#f0f\")",
        "(draw/image (image/noise) :at [4 5 0] :scale 2 :angle (* t 0.5))",
        "(draw/image (image/noise))",
    ];

    fn dump() -> String {
        let mut out = String::new();
        for (i, body) in PICTURES.iter().enumerate() {
            out.push_str(&format!("picture {i}: {body}\n"));
            let evaluation = E::static_eval(&checked(body)).expect("static");
            let value = evaluation.results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
            match lower(None, &evaluation.plan, &value) {
                Ok((program, root)) => {
                    out.push_str(&C::pp(&program));
                    out.push_str(&format!("root {}\n", root.is_some()));
                }
                Err(name) => out.push_str(&format!("refused {name}\n")),
            }
        }
        out
    }

    #[test]
    fn draw_lowering_matches_the_ocaml_snapshot() {
        let actual = dump();
        let expected = include_str!("../../../crates/flow_ir/core_draw.expected").replace("\r\n", "\n");
        if actual != expected {
            let first = actual.lines().zip(expected.lines()).position(|(a, b)| a != b).unwrap_or(0);
            panic!("Core draw lowering changed at line {}:\nactual:   {:?}\nexpected: {:?}", first + 1, actual.lines().nth(first), expected.lines().nth(first));
        }
    }

}
