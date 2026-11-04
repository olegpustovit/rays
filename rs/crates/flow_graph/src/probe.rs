// Port of lib/flow_graph/probe.ml: probes and footers: what a recording evaluation saw at each path,
// per iteration tuple, and the text and numbers a node footer, the iteration counts and the
// inspector's per-iteration list show. No drawing here.
use crate::projection as p;
use flow_check::workspace as w;
use flow_eval as e;
use flow_eval::{Eval, State, Value};
use flow_syntax::syntax::{Node as SNode, Syntax};
use flow_syntax::ty::{self, Ty};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

pub type Path = Vec<String>;
pub type Records = e::Records;
pub type ResolveFn = Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>;

#[derive(Clone, Debug, PartialEq)]
pub struct Geometry {
    pub points: i64,
    pub prims: i64,
    pub groups: Vec<String>,
    /// the size of the bounding box, when there are points
    pub extent: Option<(f64, f64, f64)>,
    /// what the node itself took the last time it was really cooked
    pub seconds: Option<f64>,
    /// Owner, name, storage kind and length, in geometry order.
    pub attributes: Vec<(String, String, String, i64)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Image {
    pub node: i64,
    pub width: i64,
    pub height: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Summary {
    Value(Value),
    Geometry(Geometry),
    Image(Image),
}

/// Renderer-neutral host report.
#[derive(Clone, Debug, PartialEq)]
pub struct Execution {
    pub tier: String,
    pub group: Path,
    pub seconds: Option<f64>,
}

pub type Force = Rc<dyn Fn(Option<&State>, Option<&[(String, Value)]>, Option<ResolveFn>, &Value, &Frame) -> Result<Value, Diagnostic>>;
/// `(offset, count, at)`: host-owned domain selectors.
pub type Bulk = (i64, i64, Rc<dyn Fn(i64) -> Result<Records, Diagnostic>>);

/// The optional arguments of `make`.
#[derive(Clone, Default)]
pub struct Options {
    pub clock: Option<Arc<dyn Fn() -> f64 + Send + Sync>>,
    pub state: Option<State>,
    pub live: Option<Frame>,
    pub time: Option<f64>,
    pub resolve: Option<ResolveFn>,
    pub force: Option<Force>,
    pub bulk_calls: Option<Rc<dyn Fn(&[String], &[i64]) -> Vec<Bulk>>>,
    pub execution: Option<Rc<dyn Fn(&[String], &[i64]) -> Option<Execution>>>,
    pub geometry: Option<Rc<dyn Fn(i64) -> Option<Geometry>>>,
    pub image: Option<Rc<dyn Fn(i64) -> Option<(i64, i64)>>>,
    pub dynamic: Option<Rc<dyn Fn(&[String]) -> Option<i64>>>,
    pub element: Option<Rc<dyn Fn(&[String], i64) -> Option<Vec<(String, Value)>>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Footer {
    /// at the probe; its type when never recorded, `not run here` in a zone it skipped
    pub value: String,
    /// numbers or flags across the innermost zone, and the probed index
    pub spark: Option<(Vec<f64>, i64)>,
    /// `then a · else b` for an `if` on a name
    pub branch: Option<String>,
    /// `kept a of b` for a `filter`
    pub kept: Option<String>,
    /// `×n`, in a zone, when the value changes
    pub runs: Option<i64>,
    /// `↥ same each time`
    pub invariant: bool,
    /// depends on frame facts or a frame fold
    pub live: bool,
    pub execution: Option<Execution>,
}

pub struct Probe {
    force: Force,
    clock: Option<Arc<dyn Fn() -> f64 + Send + Sync>>,
    over: Cell<bool>,
    execution: Rc<dyn Fn(&[String], &[i64]) -> Option<Execution>>,
    live_frame: Option<Frame>,
    state: Option<State>,
    resolve: Option<ResolveFn>,
    maps: HashMap<(Path, Vec<i64>), Vec<Bulk>>,
    bulk_calls: Rc<dyn Fn(&[String], &[i64]) -> Vec<Bulk>>,
    calls: HashMap<(Path, Vec<i64>), Records>,
    dynamic: Rc<dyn Fn(&[String]) -> Option<i64>>,
    templates: Vec<Path>,
    element: Rc<dyn Fn(&[String], i64) -> Option<Vec<(String, Value)>>>,
    geometry: Rc<dyn Fn(i64) -> Option<Geometry>>,
    user_image: Rc<dyn Fn(i64) -> Option<(i64, i64)>>,
    /// per plan node: its arguments and whether it is `image/noise`, for the image kinds only
    image_nodes: Vec<Option<(bool, Vec<(String, Value)>)>>,
    raw: Rc<HashMap<Path, Vec<(Vec<i64>, Value)>>>,
    forced: HashMap<Path, Rc<Vec<(Vec<i64>, Summary)>>>,
    spots: HashMap<(Path, Vec<i64>), Option<Summary>>,
    across: HashMap<(Path, Vec<i64>), Rc<Vec<Summary>>>,
    feet: HashMap<(Path, Vec<i64>), Footer>,
    arms: HashMap<(Path, Vec<i64>), Option<i64>>,
}

fn stopped() -> Diagnostic {
    rays_diagnostic::error("cancelled".into(), "Probe over its time budget.".into(), None, None, None, Vec::new(), Vec::new())
}
const BUDGET: f64 = 0.008;
// the memos have a capacity: a full one starts over (its entries are computed again on demand)
const MEMO_CAPACITY: usize = 4096;

fn take(l: &[i64], n: usize) -> Vec<i64> {
    l[..n.min(l.len())].to_vec()
}

fn drop_last(l: &[i64]) -> Vec<i64> {
    if l.len() <= 1 { Vec::new() } else { l[..l.len() - 1].to_vec() }
}

fn drop_last_str(l: &[String]) -> Vec<String> {
    if l.len() <= 1 { Vec::new() } else { l[..l.len() - 1].to_vec() }
}

fn calls_total(calls: &[Bulk]) -> i64 {
    calls.iter().fold(0, |n, (offset, count, _)| n.max(offset + count))
}

pub fn make(eval: &Eval, o: Options) -> Probe {
    let mut raw: HashMap<Path, Vec<(Vec<i64>, Value)>> = HashMap::new();
    for (path, l) in &eval.records {
        raw.insert(path.clone(), l.clone());
    }
    let mut templates: Vec<Path> = Vec::new();
    let mut image_nodes = Vec::with_capacity(eval.plan.nodes.len());
    for node in &eval.plan.nodes {
        if node.kind == "zone/points" || node.kind == "zone/pieces" {
            templates.push(node.site.clone());
        }
        image_nodes.push(if node.kind == "image/noise" || node.kind == "image/render" { Some((node.kind == "image/noise", node.args.clone())) } else { None });
    }
    let live_frame = match (&o.live, o.time) {
        (Some(l), _) => Some(l.clone()),
        (None, Some(t)) => Some(input::at_time(t)),
        (None, None) => None,
    };
    let state = o.state.as_ref().map(e::fork_state);
    let force: Force = o.force.unwrap_or_else(|| {
        Rc::new(|state, elems, resolve, v, live| e::Host::force_reference(v, live, state, elems, resolve))
    });
    Probe {
        force,
        clock: o.clock,
        over: Cell::new(false),
        execution: o.execution.unwrap_or_else(|| Rc::new(|_, _| None)),
        live_frame,
        state,
        resolve: o.resolve,
        maps: HashMap::new(),
        bulk_calls: o.bulk_calls.unwrap_or_else(|| Rc::new(|_, _| Vec::new())),
        calls: HashMap::new(),
        dynamic: o.dynamic.unwrap_or_else(|| Rc::new(|_| None)),
        templates,
        element: o.element.unwrap_or_else(|| Rc::new(|_, _| None)),
        geometry: o.geometry.unwrap_or_else(|| Rc::new(|_| None)),
        user_image: o.image.unwrap_or_else(|| Rc::new(|_| None)),
        image_nodes,
        raw: Rc::new(raw),
        forced: HashMap::new(),
        spots: HashMap::new(),
        across: HashMap::new(),
        feet: HashMap::new(),
        arms: HashMap::new(),
    }
}

fn num(x: f64) -> String {
    let s = cfmt::f(x, 3);
    let s = if s.contains('.') {
        let t = s.trim_end_matches('0');
        t.strip_suffix('.').unwrap_or(t).to_owned()
    } else {
        s
    };
    if s == "-0" { "0".to_owned() } else { s }
}

fn text_of(b: &[u8]) -> String {
    String::from_utf8_lossy(b).into_owned()
}

fn describe_value(v: &Value) -> String {
    match v {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => num(*f),
        Value::Bool(b) => b.to_string(),
        Value::Text(s) => text_of(s),
        Value::Vec2(x, y) => format!("[{} {}]", num(*x), num(*y)),
        Value::Vec3(x, y, z) => format!("[{} {} {}]", num(*x), num(*y), num(*z)),
        Value::Vec4(x, y, z, w) => format!("[{} {} {} {}]", num(*x), num(*y), num(*z), num(*w)),
        Value::List(xs) => {
            let n = xs.len();
            if n == 0 {
                return "0 items".to_owned();
            }
            let numeric = |x: &Value| matches!(x, Value::Int(_) | Value::Float(_));
            let what = if xs.iter().all(numeric) {
                format!("[{}{}", xs.iter().take(4).map(describe_value).collect::<Vec<_>>().join(" "), if n > 4 { " …]" } else { "]" })
            } else {
                match &xs[0] {
                    Value::Deferred(Ty::Named(g), _) if g == "geometry" => "geometry".to_owned(),
                    Value::No_geo => "geometry".to_owned(),
                    Value::Text(_) => "text".to_owned(),
                    Value::Vec2(..) => "vec2".to_owned(),
                    Value::Vec3(..) => "vec3".to_owned(),
                    Value::Vec4(..) => "vec4".to_owned(),
                    Value::Bool(_) => "bool".to_owned(),
                    Value::List(_) => "list".to_owned(),
                    Value::Record(_) => "record".to_owned(),
                    Value::Fn(_) => "function".to_owned(),
                    _ => "value".to_owned(),
                }
            };
            format!("{} × {}", n, what)
        }
        Value::Int_array(xs) => format!("{} × int", xs.len()),
        Value::Text_array(xs) => format!("{} × text", xs.len()),
        Value::Float_array(xs) => format!("{} × float", xs.len()),
        Value::Vec2_array(xs) => format!("{} × vec2", xs.len() / 2),
        Value::Vec3_array(xs) => format!("{} × vec3", xs.len() / 3),
        Value::Vec4_array(xs) => format!("{} × vec4", xs.len() / 4),
        Value::Record(fs) => format!("{{{}}}", fs.iter().map(|(f, v)| format!("{} {}", f, describe_value(v))).collect::<Vec<_>>().join(" · ")),
        Value::Deferred(t, _) => ty::to_string(t),
        Value::No_geo => "nil".to_owned(),
        Value::Struct(op, _, _) | Value::Source(op, _, _) => op.clone(),
        Value::Fn(_) => "function".to_owned(),
        Value::Residual(_) => "?".to_owned(),
    }
}

fn spaced(n: i64) -> String {
    let s = n.to_string();
    let len = s.len();
    let mut out = String::new();
    for (i, c) in s.chars().enumerate() {
        if i > 0 && (len - i) % 3 == 0 {
            out.push(' ');
        }
        out.push(c);
    }
    out
}

fn describe(s: &Summary) -> String {
    match s {
        Summary::Value(v) => describe_value(v),
        Summary::Image(i) => format!("{} × {} image", i.width, i.height),
        Summary::Geometry(g) => {
            // point-only geometry (a scatter) reads in points, thousands apart as the sheet's `1 204 pts`
            let mut out = if g.prims == 0 && g.points > 0 {
                format!("{} pt{}", spaced(g.points), if g.points == 1 { "" } else { "s" })
            } else {
                format!("{} prim{}", g.prims, if g.prims == 1 { "" } else { "s" })
            };
            if !g.groups.is_empty() {
                out.push_str(" · groups ");
                out.push_str(&g.groups.join(", "));
            }
            if let Some(sec) = g.seconds {
                if g.prims > 0 {
                    out.push_str(&format!(" · {} pts", spaced(g.points)));
                }
                out.push_str(&format!(" · {} s", cfmt::f(sec, 3)));
            }
            out
        }
    }
}

// ---- chains, series ----

/// The enclosing iterating zones of every node below the scope, outermost first.
pub fn chains(sc: &p::Scope) -> HashMap<Path, Vec<Path>> {
    fn go(tbl: &mut HashMap<Path, Vec<Path>>, chain: &[Path], sc: &p::Scope) {
        for n in &sc.nodes {
            tbl.insert(n.path.clone(), chain.to_vec());
            if let Some(z) = &n.zone {
                if matches!(z.kind, p::ZoneKind::Let | p::ZoneKind::State | p::ZoneKind::Branch) {
                    go(tbl, chain, &z.scope);
                } else {
                    let mut c = chain.to_vec();
                    c.push(n.path.clone());
                    go(tbl, &c, &z.scope);
                }
            }
        }
    }
    let mut tbl = HashMap::new();
    go(&mut tbl, &[], sc);
    tbl
}

fn number(s: &Summary) -> Option<f64> {
    match s {
        Summary::Value(Value::Int(i)) => Some(*i as f64),
        Summary::Value(Value::Float(f)) => Some(*f),
        Summary::Value(Value::Bool(b)) => Some(if *b { 1.0 } else { 0.0 }),
        _ => None,
    }
}

fn arg(n: &p::Node, i: i64) -> Option<String> {
    n.rows.iter().find(|r| r.key == crate::edit_base::ArgKey::Pos(i)).and_then(|r| match &r.expr {
        Some(Syntax { node: SNode::Sym(s), .. }) => Some(s.clone()),
        _ => None,
    })
}

impl Probe {
    /// Both come from one evaluation.
    pub fn same_eval(&self, other: &Probe) -> bool {
        Rc::ptr_eq(&self.raw, &other.raw)
    }

    /// Host execution metadata without forcing a probe value.
    pub fn execution(&self, path: &[String], probes: &[i64]) -> Option<Execution> {
        (self.execution)(path, probes)
    }

    // runs `f` unless a previous evaluation went over the budget; false when it did not complete
    fn guard(&self, f: &mut dyn FnMut()) -> bool {
        match &self.clock {
            None => {
                f();
                true
            }
            Some(_) if self.over.get() => false,
            Some(clock) => {
                let start = clock();
                let c = clock.clone();
                let completed = e::with_interrupt(Arc::new(move || c() - start > BUDGET), f);
                if !completed {
                    self.over.set(true);
                }
                completed
            }
        }
    }

    fn live_or_zero(&self) -> Frame {
        self.live_frame.clone().unwrap_or_else(|| input::at_time(0.0))
    }

    fn force_value(&self, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<ResolveFn>, v: &Value, live: &Frame) -> Result<Value, Diagnostic> {
        let mut out = None;
        let force = self.force.clone();
        if self.guard(&mut || out = Some(force(state, elems, resolve.clone(), v, live))) {
            out.unwrap_or_else(|| Err(stopped()))
        } else {
            Err(stopped())
        }
    }

    fn image_of(&self, id: i64) -> Option<Image> {
        let dimensions = match (self.user_image)(id) {
            Some(d) => Some(d),
            None => {
                if id < 0 || id as usize >= self.image_nodes.len() {
                    None
                } else {
                    match &self.image_nodes[id as usize] {
                        None => None,
                        Some((noise, args)) => {
                            let live = self.live_or_zero();
                            let dimension = |name: &str, default: i64| -> Option<i64> {
                                match args.iter().find(|(n, _)| n == name) {
                                    None => Some(default),
                                    Some((_, value)) => match self.force_value(self.state.as_ref(), None, self.resolve.clone(), value, &live) {
                                        Ok(Value::Int(n)) if n > 0 => Some(n),
                                        _ => None,
                                    },
                                }
                            };
                            let (wd, ht) = if *noise { (256, 256) } else { (live.size.0, live.size.1) };
                            dimension("width", wd).and_then(|width| dimension("height", ht).map(|height| (width, height)))
                        }
                    }
                }
            }
        };
        dimensions.map(|(width, height)| Image { node: id, width, height })
    }

    fn summarize(&self, v: &Value) -> Summary {
        let v = if e::is_live(v) && (self.live_frame.is_some() || self.resolve.is_some() || !(e::frame_dependent(v) || e::state_dependent(v))) {
            match self.force_value(self.state.as_ref(), None, self.resolve.clone(), v, &self.live_or_zero()) {
                Ok(v) => v,
                Err(_) => v.clone(),
            }
        } else {
            v.clone()
        };
        match &v {
            Value::Deferred(Ty::Named(n), id) if n == "geometry" => match (self.geometry)(*id) {
                Some(g) => Summary::Geometry(g),
                None => Summary::Value(v.clone()),
            },
            Value::Deferred(Ty::Named(n), id) if n == "image" => match self.image_of(*id) {
                Some(image) => Summary::Image(image),
                None => Summary::Value(v.clone()),
            },
            _ => Summary::Value(v),
        }
    }

    /// The plan node of a geometry value at the probe.
    pub fn plan_node(&self, ty_: Option<&Ty>, path: &[String], probes: &[i64]) -> Option<i64> {
        let want = ty_.cloned().unwrap_or_else(ty::geometry);
        let find = |probes: &[i64]| -> Option<i64> {
            self.raw.get(path).and_then(|l| {
                l.iter().find_map(|(it, v)| match v {
                    Value::Deferred(d, id) if *d == want && it == probes => Some(*id),
                    _ => None,
                })
            })
        };
        if let Some(found) = find(probes) {
            return Some(found);
        }
        let mut template: Vec<i64> = probes.to_vec();
        for length in 1..path.len() {
            let zone = &path[..length];
            if self.templates.iter().any(|t| t == zone) {
                if let Some(l) = self.raw.get(zone) {
                    for (it, _) in l {
                        if *it == take(&template, it.len()) {
                            template = template.iter().enumerate().map(|(i, value)| if i == it.len() { 0 } else { *value }).collect();
                        }
                    }
                }
            }
        }
        find(&template)
    }

    /// Every recorded value of a path in evaluation order.
    pub fn records(&mut self, path: &[String]) -> Rc<Vec<(Vec<i64>, Summary)>> {
        if let Some(a) = self.forced.get(path) {
            return a.clone();
        }
        let a: Vec<(Vec<i64>, Summary)> = match self.raw.get(path) {
            None => Vec::new(),
            Some(l) => l.iter().map(|(it, v)| (it.clone(), self.summarize(v))).collect(),
        };
        let a = Rc::new(a);
        if self.forced.len() >= MEMO_CAPACITY { self.forced.clear(); } self.forced.insert(path.to_vec(), a.clone());
        a
    }

    fn record_count(&self, path: &[String]) -> i64 {
        self.raw.get(path).map(|l| l.len() as i64).unwrap_or(0)
    }

    fn map_calls(&mut self, zone: &[String], outer: &[i64]) -> Vec<Bulk> {
        let key = (zone.to_vec(), outer.to_vec());
        if let Some(calls) = self.maps.get(&key) {
            return calls.clone();
        }
        let live = self.live_or_zero();
        let mut calls: Vec<Bulk> = Vec::new();
        let mut call_path = zone.to_vec();
        call_path.push("~calls".to_owned());
        if let Some(l) = self.raw.get(&call_path) {
            for (it, value) in l {
                if it != outer {
                    continue;
                }
                if let Value::Record(fs) = value {
                    if let [(a, Value::Int(offset)), (b, Value::Residual(call))] = fs.as_slice() {
                        if a == "offset" && b == "call" {
                            let mut got: Option<Result<(usize, Box<dyn Fn(usize) -> Result<Records, Diagnostic>>), Diagnostic>> = None;
                            let (state, resolve) = (self.state.clone(), self.resolve.clone());
                            // the residual is run once before it is probed, as the host's execute step
                            let ran = self.force_value(state.as_ref(), None, resolve.clone(), &Value::Residual(*call), &live);
                            let completed = match ran {
                                Err(d) => {
                                    got = Some(Err(d));
                                    true
                                }
                                Ok(_) => self.guard(&mut || got = Some(e::Host::map_probe(*call, &live, state.as_ref(), resolve.clone(), None, *offset as usize))),
                            };
                            if let (true, Some(Ok((count, at)))) = (completed, got) {
                                let at: Rc<dyn Fn(usize) -> Result<Records, Diagnostic>> = Rc::from(at);
                                calls.push((*offset, count as i64, Rc::new(move |k| at(k as usize))));
                            }
                        }
                    }
                }
            }
        }
        let base = calls_total(&calls);
        let mut bulk_list: Vec<Bulk> = Vec::new();
        let bulk_calls = self.bulk_calls.clone();
        if !self.guard(&mut || bulk_list = bulk_calls(zone, outer)) {
            bulk_list = Vec::new();
        }
        let outer_len = outer.len();
        for (offset, count, at) in bulk_list {
            let at2: Rc<dyn Fn(i64) -> Result<Records, Diagnostic>> = Rc::new(move |k| {
                at(k).map(|recs| {
                    recs.into_iter()
                        .map(|(path, values)| {
                            (path, values.into_iter().map(|(iter, value)| (iter.iter().enumerate().map(|(i, n)| if i == outer_len { n + base } else { *n }).collect(), value)).collect())
                        })
                        .collect()
                })
            });
            calls.push((base + offset, count, at2));
        }
        if self.maps.len() >= MEMO_CAPACITY { self.maps.clear(); } self.maps.insert(key, calls.clone());
        calls
    }

    fn map_scope(&mut self, path: &[String], probes: &[i64]) -> Option<(Path, Vec<Bulk>)> {
        if probes.is_empty() {
            return None;
        }
        let mut zone = drop_last_str(path);
        loop {
            if zone.is_empty() {
                return None;
            }
            let calls = self.map_calls(&zone, &drop_last(probes));
            if calls.is_empty() {
                zone = drop_last_str(&zone);
            } else {
                return Some((zone, calls));
            }
        }
    }

    fn guarded_at(&self, at: &Rc<dyn Fn(i64) -> Result<Records, Diagnostic>>, k: i64) -> Result<Records, Diagnostic> {
        let mut out = None;
        if self.guard(&mut || out = Some(at(k))) {
            out.unwrap_or_else(|| Err(stopped()))
        } else {
            Err(stopped())
        }
    }

    fn map_at(&mut self, path: &[String], probes: &[i64]) -> Option<Summary> {
        let (zone, calls) = self.map_scope(path, probes)?;
        let k = *probes.last()?;
        let key = (zone.clone(), probes.to_vec());
        let records: Records = match self.calls.get(&key) {
            Some(r) => r.clone(),
            None => {
                let mut found: Option<Records> = None;
                for (offset, count, at) in &calls {
                    if k < *offset || k - offset >= *count {
                        continue;
                    }
                    if let Ok(r) = self.guarded_at(at, k - offset) {
                        found = Some(r);
                        break;
                    }
                }
                let records = found.unwrap_or_default();
                if self.calls.len() >= MEMO_CAPACITY { self.calls.clear(); } self.calls.insert(key, records.clone());
                records
            }
        };
        let (_, values) = records.iter().find(|(p, _)| p == path)?;
        values.iter().find_map(|(it, value)| if it == probes { Some(self.summarize(value)) } else { None })
    }

    // the template record of `path` forced for element `k` of the loop over geometry at `zone`
    fn at_element(&self, path: &[String], zone: &[String], k: i64) -> Option<Summary> {
        let raw = self.raw.get(path).and_then(|l| l.iter().find_map(|(it, v)| if it.iter().all(|x| *x == 0) { Some(v.clone()) } else { None }));
        match (raw, (self.element)(zone, k)) {
            (Some(raw), Some(elems)) if e::is_live(&raw) => {
                match self.force_value(self.state.as_ref(), Some(&elems), self.resolve.clone(), &raw, &self.live_or_zero()) {
                    Ok(v) => Some(self.summarize(&v)),
                    Err(_) => None,
                }
            }
            _ => None,
        }
    }

    fn dynamic_zones(&self, path: &[String]) -> Vec<usize> {
        (1..path.len()).filter(|i| (self.dynamic)(&path[..*i]).is_some()).collect()
    }

    // the record with exactly this tuple; inside a loop over geometry the body is one template
    // record (its iteration is 0), which reads the element
    fn compute_at(&mut self, path: &[String], probes: &[i64]) -> Option<Summary> {
        if let Some(found) = self.map_at(path, probes) {
            return Some(found);
        }
        if let Some(found) = self.raw.get(path).and_then(|l| l.iter().find_map(|(it, v)| if it == probes { Some(self.summarize(v)) } else { None })) {
            return Some(found);
        }
        let zones = self.dynamic_zones(path);
        if zones.is_empty() {
            return None;
        }
        let template = |it: &Vec<i64>| it.len() == probes.len() && it.iter().zip(probes.iter()).all(|(r, p)| r == p || *r == 0);
        let zone: Vec<String> = path[..zones.iter().copied().fold(0, usize::max)].to_vec();
        let raw = self.raw.get(path).and_then(|l| l.iter().find_map(|(it, v)| if template(it) { Some(v.clone()) } else { None }));
        let elems = if probes.is_empty() { None } else { (self.element)(&zone, *probes.last().unwrap()) };
        let fallback = |this: &Probe| -> Option<Summary> {
            this.raw.get(path).and_then(|l| l.iter().find_map(|(it, v)| if template(it) { Some(this.summarize(v)) } else { None }))
        };
        match (raw, elems) {
            (Some(raw), Some(elems)) if e::is_live(&raw) => {
                match self.force_value(self.state.as_ref(), Some(&elems), self.resolve.clone(), &raw, &self.live_or_zero()) {
                    Ok(v) => Some(self.summarize(&v)),
                    Err(_) => fallback(self),
                }
            }
            _ => fallback(self),
        }
    }

    /// Reference value at the selected iteration tuple.
    pub fn at(&mut self, path: &[String], probes: &[i64]) -> Option<Summary> {
        let key = (path.to_vec(), probes.to_vec());
        match self.spots.get(&key).cloned() {
            Some(Some(Summary::Value(value @ Value::Deferred(Ty::Named(_), _)))) if matches!(&value, Value::Deferred(Ty::Named(n), _) if n == "image") => Some(self.summarize(&value)),
            Some(Some(Summary::Image(image))) => self.image_of(image.node).map(Summary::Image),
            Some(value) => value,
            None => {
                let value = self.compute_at(path, probes);
                if self.spots.len() >= MEMO_CAPACITY { self.spots.clear(); } self.spots.insert(key, value.clone());
                value
            }
        }
    }

    fn missing(&self, node: &p::Node, key: &str, probes: &[i64]) -> Option<Summary> {
        let live = self.live_frame.clone()?;
        let records = self.raw.get(&node.path)?;
        records.iter().find_map(|(tuple, value)| match value {
            Value::Residual(residual) if tuple == probes => {
                let view = e::Residual::residual_view(*residual).ok()?;
                let term: Option<w::Term> = match (&view.term.node, key) {
                    (w::Node::If(test, _, _), "test") => Some((**test).clone()),
                    (w::Node::Case(test, _, _), "of") => Some((**test).clone()),
                    (w::Node::Cond(arms, _), key) if key.starts_with("test") => key[4..].parse::<usize>().ok().and_then(|i| arms.get(i)).map(|x| x.0.clone()),
                    _ => None,
                };
                term.and_then(|term| e::Residual::eval_term(*residual, &term, &live, self.state.as_ref(), None, self.resolve.clone()).ok()).map(Summary::Value)
            }
            _ => None,
        })
    }

    fn compute_taken_arm(&mut self, node: &p::Node, probes: &[i64]) -> Option<i64> {
        let tests: Vec<i64> = node.rows.iter().filter_map(|row| match row.key {
            crate::edit_base::ArgKey::Arm(i) if i >= 0 => Some(i),
            _ => None,
        }).collect();
        let count = tests.len() as i64;
        let at_key = |this: &mut Probe, key: &str| -> Option<Summary> {
            let (leaf, outer) = node.path.split_last()?;
            let mut path = outer.to_vec();
            path.push(format!("{}#{}", leaf, key));
            match this.at(&path, probes) {
                None => this.missing(node, key, probes),
                some => some,
            }
        };
        let truthy_value = |s: &Option<Summary>| -> Option<bool> {
            match s {
                Some(Summary::Value(v @ (Value::Bool(_) | Value::Int(_) | Value::Float(_)))) => flow_syntax::value::truthy(v).ok(),
                _ => None,
            }
        };
        match node.head.as_str() {
            "if" => {
                let t = at_key(self, "test");
                truthy_value(&t).map(|b| if b { 0 } else { 1 })
            }
            "cond" => {
                for i in tests {
                    let t = at_key(self, &format!("test{}", i));
                    match truthy_value(&t) {
                        Some(true) => return Some(i),
                        Some(false) => {}
                        None => return None,
                    }
                }
                Some(count)
            }
            "case" => {
                let equal = |value: &Value, literal: &Syntax| -> bool {
                    match (value, &literal.node) {
                        (Value::Text(a), SNode::Str(b)) => a == b,
                        (Value::Bool(_) | Value::Int(_) | Value::Float(_), SNode::Sym(s)) if s == "true" => flow_syntax::value::truthy(value).unwrap_or(false),
                        (Value::Bool(_) | Value::Int(_) | Value::Float(_), SNode::Sym(s)) if s == "false" => !flow_syntax::value::truthy(value).unwrap_or(true),
                        (Value::Bool(a), SNode::Num(b)) => *a == (cfmt::parse_ocaml_float(b).unwrap_or(0.0) != 0.0),
                        (Value::Int(a), SNode::Num(b)) => *a as f64 == cfmt::parse_ocaml_float(b).unwrap_or(f64::NAN),
                        (Value::Float(a), SNode::Num(b)) => *a == cfmt::parse_ocaml_float(b).unwrap_or(f64::NAN),
                        _ => false,
                    }
                };
                match at_key(self, "of") {
                    Some(Summary::Value(value)) => Some(
                        tests
                            .iter()
                            .find(|i| {
                                let key = crate::edit_base::ArgKey::Pos(2 * **i + 1);
                                node.rows.iter().any(|row| row.key == key && row.expr.as_ref().is_some_and(|l| equal(&value, l)))
                            })
                            .copied()
                            .unwrap_or(count),
                    ),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Selected conditional arm, zero based; the final else follows the tested arms.
    pub fn taken_arm(&mut self, node: &p::Node, probes: &[i64]) -> Option<i64> {
        let key = (node.path.clone(), probes.to_vec());
        if let Some(arm) = self.arms.get(&key) {
            return *arm;
        }
        let arm = self.compute_taken_arm(node, probes);
        if self.arms.len() >= MEMO_CAPACITY { self.arms.clear(); } self.arms.insert(key, arm);
        arm
    }

    // the records whose tuple is `outer` followed by one more index, in order; in a loop over
    // geometry the body has one template record, which is forced for each element once the zone cooked
    fn across(&mut self, path: &[String], outer: &[i64]) -> Rc<Vec<Summary>> {
        let key = (path.to_vec(), outer.to_vec());
        if let Some(a) = self.across.get(&key) {
            return a.clone();
        }
        let n = outer.len();
        let recs = self.records(path);
        let mut a: Vec<Summary> = recs.iter().filter(|(it, _)| it.len() == n + 1 && it[..n] == *outer).map(|(_, s)| s.clone()).collect();
        if !(a.len() > 1 || !outer.is_empty()) {
            let zones = self.dynamic_zones(path);
            if !zones.is_empty() {
                let zone: Vec<String> = path[..zones.iter().copied().fold(0, usize::max)].to_vec();
                if let Some(count) = (self.dynamic)(&zone) {
                    if count > 1 {
                        let forced: Vec<Option<Summary>> = (0..count).map(|k| self.at_element(path, &zone, k)).collect();
                        if forced.iter().all(|f| f.is_some()) {
                            a = forced.into_iter().map(|f| f.unwrap()).collect();
                        }
                    }
                }
            }
        }
        let mut probe_outer = outer.to_vec();
        probe_outer.push(0);
        if let Some((_, calls)) = self.map_scope(path, &probe_outer) {
            if !calls.is_empty() {
                let count = calls_total(&calls);
                // ponytail: at most 64 reference samples per packed function, shared
                // across its body cards; an explicit full-array inspector can page later.
                let samples = count.min(64);
                let mut sampled: Vec<Option<Summary>> = Vec::new();
                for i in 0..samples {
                    let k = if samples < 2 { 0 } else { (i as f64 * (count - 1) as f64 / (samples - 1) as f64).round() as i64 };
                    let mut pr = outer.to_vec();
                    pr.push(k);
                    sampled.push(self.map_at(path, &pr));
                }
                if sampled.iter().all(|s| s.is_some()) {
                    a = sampled.into_iter().map(|s| s.unwrap()).collect();
                }
            }
        }
        let a = Rc::new(a);
        if self.across.len() >= MEMO_CAPACITY { self.across.clear(); } self.across.insert(key, a.clone());
        a
    }

    fn series(&mut self, path: &[String], probes: &[i64]) -> Rc<Vec<Summary>> {
        if probes.is_empty() {
            Rc::new(Vec::new())
        } else {
            self.across(path, &drop_last(probes))
        }
    }

    // Where the probed iteration sits in `series`: the iterations a `:skip` left out have no record,
    // so it is the number of recorded ones before it (the probe itself when none is missing).
    fn position(&mut self, path: &[String], probes: &[i64], len: i64) -> i64 {
        let n = probes.len() - 1;
        let outer = drop_last(probes);
        let pr = probes[n];
        if let Some((_, calls)) = self.map_scope(path, probes) {
            if !calls.is_empty() {
                let count = calls_total(&calls);
                return if count < 2 { 0 } else { (len - 1).min((pr as f64 * (len - 1) as f64 / (count - 1) as f64).round() as i64) };
            }
        }
        let recs = self.records(path);
        let mine: Vec<&(Vec<i64>, Summary)> = recs.iter().filter(|(it, _)| it.len() == n + 1 && it[..n] == outer[..]).collect();
        // a loop over geometry has one template record, forced for each element: no iteration is missing
        if mine.len() as i64 != len {
            pr.min(len - 1)
        } else {
            (mine.iter().filter(|(it, _)| it[n] < pr).count() as i64).min(len - 1)
        }
    }

    /// Iterations (or calls) each zone ran under the probes of the zones around it.
    pub fn counts(&mut self, sc: &p::Scope, probe: &dyn Fn(&[String]) -> i64) -> Vec<(Path, i64)> {
        let chain = chains(sc);
        let mut out = Vec::new();
        for n in p::zones(sc) {
            let z = n.zone.as_ref().unwrap();
            let outer: Vec<i64> = chain.get(&n.path).cloned().unwrap_or_default().iter().map(|c| probe(c)).collect();
            let calls = self.map_calls(&n.path, &outer);
            if !calls.is_empty() {
                out.push((n.path.clone(), calls_total(&calls)));
            } else if let Some(count) = (self.dynamic)(&n.path) {
                out.push((n.path.clone(), count));
            } else {
                for r in &z.rail {
                    if r.role == p::Role::Capture {
                        continue;
                    }
                    let mut rp = n.path.clone();
                    rp.push(format!(":{}", r.names[0]));
                    let a = self.across(&rp, &outer);
                    if !a.is_empty() {
                        out.push((n.path.clone(), a.len() as i64));
                        break;
                    }
                }
            }
        }
        out
    }

    fn compute_footer(&mut self, n: &p::Node, probes: &[i64]) -> Footer {
        let parent = drop_last_str(&n.path);
        let here = self.at(&n.path, probes);
        let value = match &here {
            Some(s) => describe(s),
            None => {
                if !probes.is_empty() && self.record_count(&n.path) > 0 {
                    "not run here".to_owned()
                } else {
                    ty::to_string(&n.ty)
                }
            }
        };
        let spark = if probes.is_empty() || n.invariant || !matches!(n.ty, Ty::Float | Ty::Int | Ty::Bool | Ty::Any) {
            None
        } else {
            let a: Vec<Option<f64>> = self.series(&n.path, probes).iter().map(number).collect();
            if a.len() < 2 || a.iter().any(|x| x.is_none()) {
                None
            } else {
                let len = a.len() as i64;
                Some((a.into_iter().map(|x| x.unwrap()).collect(), self.position(&n.path, probes, len)))
            }
        };
        let branch = if matches!(n.head.as_str(), "cond" | "case") {
            self.taken_arm(n, probes).map(|i| format!("arm {}", i + 1))
        } else if probes.is_empty() || n.head != "if" {
            None
        } else {
            arg(n, 0).and_then(|c| {
                let mut cp = parent.clone();
                cp.push(c);
                let cs = self.series(&cp, probes);
                if cs.is_empty() {
                    None
                } else {
                    let yes = cs.iter().filter(|s| **s == Summary::Value(Value::Bool(true))).count();
                    Some(format!("then {} · else {}", yes, cs.len() - yes))
                }
            })
        };
        let kept = if n.head != "filter" {
            None
        } else {
            let other = arg(n, 1).and_then(|s| {
                let mut sp = parent.clone();
                sp.push(s);
                self.at(&sp, probes)
            });
            match (&here, other) {
                (Some(Summary::Value(Value::List(k))), Some(Summary::Value(Value::List(all)))) => Some(format!("kept {} of {}", k.len(), all.len())),
                _ => None,
            }
        };
        let invariant = !probes.is_empty() && n.invariant;
        let execution = match ((self.execution)(&n.path, probes), &here) {
            (None, Some(Summary::Geometry(_))) => Some(Execution { tier: "Cooked".to_owned(), group: n.path.clone(), seconds: None }),
            (execution, _) => execution,
        };
        let runs = if !probes.is_empty() && !n.invariant {
            Some(match self.map_scope(&n.path, probes) {
                Some((_, calls)) if !calls.is_empty() => calls_total(&calls),
                _ => self.record_count(&n.path),
            })
        } else {
            None
        };
        Footer { value, spark, branch, kept, runs, invariant, live: n.live, execution }
    }

    pub fn footer(&mut self, n: &p::Node, probes: &[i64]) -> Footer {
        if n.ty == ty::image() {
            return self.compute_footer(n, probes);
        }
        let key = (n.path.clone(), probes.to_vec());
        if let Some(f) = self.feet.get(&key) {
            return f.clone();
        }
        let f = self.compute_footer(n, probes);
        if self.feet.len() >= MEMO_CAPACITY { self.feet.clear(); } self.feet.insert(key, f.clone());
        f
    }

    /// The inspector's rows: `value` (or `value at probe` inside a zone) and `cook`.
    pub fn readouts(&mut self, n: &p::Node, probes: &[i64]) -> Vec<(String, String)> {
        let footer = self.footer(n, probes);
        let mut out = vec![
            (if probes.is_empty() { "value" } else { "value at probe" }.to_owned(), footer.value.clone()),
            ("cook".to_owned(), if n.live { "live, recooks every frame" } else { "cached" }.to_owned()),
        ];
        if let Some(e) = &footer.execution {
            out.push(("tier".to_owned(), e.tier.clone()));
            out.push(("execution group".to_owned(), e.group.join("/")));
            if let Some(seconds) = e.seconds {
                out.push(("group time".to_owned(), format!("{} ms", cfmt::f(seconds * 1000.0, 3))));
            }
        }
        out
    }

    /// The inspector's textual summary of each recorded iteration.
    pub fn iterations(&mut self, n: &p::Node, probes: &[i64]) -> Vec<String> {
        self.series(&n.path, probes).iter().map(describe).collect()
    }

    /// The plan nodes of every geometry node of the scope at its probes, with the node's path.
    pub fn geometry_targets(&self, sc: &p::Scope, probe: &dyn Fn(&[String]) -> i64) -> Vec<(Path, i64)> {
        let chain = chains(sc);
        fn go(this: &Probe, sc: &p::Scope, chain: &HashMap<Path, Vec<Path>>, probe: &dyn Fn(&[String]) -> i64, out: &mut Vec<(Path, i64)>) {
            for n in &sc.nodes {
                match &n.zone {
                    Some(z) => go(this, &z.scope, chain, probe, out),
                    None if n.ty == ty::geometry() => {
                        let probes: Vec<i64> = chain.get(&n.path).cloned().unwrap_or_default().iter().map(|c| probe(c)).collect();
                        if let Some(id) = this.plan_node(None, &n.path, &probes) {
                            out.push((n.path.clone(), id));
                        }
                    }
                    None => {}
                }
            }
        }
        let mut out = Vec::new();
        go(self, sc, &chain, probe, &mut out);
        out
    }
}

/// The footer as one line, parts joined by ` · `.
pub fn text(f: &Footer) -> String {
    let mut parts: Vec<String> = vec![f.value.clone()];
    parts.extend(f.branch.clone());
    parts.extend(f.kept.clone());
    parts.extend(f.runs.map(|r| format!("×{}", r)));
    if f.invariant {
        parts.push("↑ same each time".to_owned());
    }
    if f.live {
        parts.push("t".to_owned());
    }
    if let Some(e) = &f.execution {
        parts.push(format!("{}{}", e.tier, e.seconds.map(|s| format!(" · {} ms", cfmt::f(s * 1000.0, 3))).unwrap_or_default()));
    }
    parts.join(" · ")
}
