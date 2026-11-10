//! Fusion across cards (single-program design, E4). A point write whose geometry is, through card
//! markers and deletions of other attributes, a point write that nothing else reads is rewritten to
//! write over that write's input: its reads of an attribute the chain writes become the values
//! written, its other attribute reads and counts of the chain read the input, the earlier writes run
//! first in the same node (`FUSED_WRITES`, one record program) and the chain's deletions run after
//! them (`FUSED_DELETES`; they commute with writes of other attributes). The earlier write is no
//! longer cooked, so the maps are one region; folding left to right chains any number of writes. The
//! intermediate cards are unchanged, for display and probes.
use super::*;
use std::sync::atomic::AtomicBool;

/// `(attribute, type, values)`: one write a fused node performs.
type Entry = (String, String, Value);

/// The ids the copies of a folded term take: deterministic per plan, outside the evaluator's, because
/// the copies' residual ids are cache keys.
#[derive(Clone, Default)]
struct Fresh(Arc<AtomicI64>);

impl Fresh {
    fn next(&self) -> i64 {
        -(1i64 << 50) - (self.0.fetch_add(1, Ordering::SeqCst) + 1)
    }
}

/// Which plan nodes belong to a zone's template body.
pub(super) fn template_flags(nodes: &[ENode]) -> Vec<bool> {
    let count = nodes.len() as i64;
    let mut templ = vec![false; nodes.len()];
    for n in nodes {
        if is_zone(&n.kind) {
            if let (Some(Value::Int(lo)), Some(Value::Int(hi))) = (get(&n.args, "lo"), get(&n.args, "hi")) {
                for id in (*lo).max(0)..count.min(*hi) {
                    templ[id as usize] = true;
                }
            }
        }
    }
    templ
}

fn usage_counts(plan: &Plan, nodes: &[ENode], skip: &[bool]) -> Vec<i64> {
    let count = nodes.len() as i64;
    let mut uses = vec![0i64; nodes.len()];
    let mut note = |values: Vec<&Value>| {
        let mut ids: Vec<i64> = values.into_iter().flat_map(ak::sources).collect();
        ids.sort();
        ids.dedup();
        for id in ids {
            if id >= 0 && id < count {
                uses[id as usize] += 1;
            }
        }
    };
    for n in nodes {
        if !skip[n.id as usize] {
            note(n.args.iter().map(|(_, v)| v).collect());
        }
    }
    for i in &plan.instances {
        note(std::iter::once(&i.result).chain(i.inputs.iter().map(|(_, v)| v)).collect());
    }
    uses
}

/// The node count and the template flags: what every walk down a chain of nodes consults.
struct Shape {
    count: i64,
    templ: Vec<bool>,
}

/// Residual and closure environments share values: each residual or function is answered once per node
/// (without the memo the walk is exponential in the sharing: a driven transform's inlined body took
/// 30 s). The answer is a function of the id, so the memo is exact.
#[derive(Default)]
struct Seen {
    residuals: HashMap<ResidualId, bool>,
    functions: HashMap<FnId, bool>,
}

impl Shape {
    fn new(nodes: &[ENode]) -> Shape {
        Shape { count: nodes.len() as i64, templ: template_flags(nodes) }
    }

    /// The index of node `id` when a walk may look at it: in the plan and not in a zone template.
    fn open(&self, id: i64) -> Option<usize> {
        (id >= 0 && id < self.count && !self.templ[id as usize]).then_some(id as usize)
    }

    /// The geometry a node hands on unchanged: a card's `value`, the `geometry` of a write, a deletion
    /// or a group write. Always an earlier node.
    fn passes(&self, n: &ENode, id: i64) -> Option<i64> {
        let key = if n.kind == E::card_kind() {
            "value"
        } else if ["sop/with_attr", "sop/delete_attr", "sop/with_group"].contains(&n.kind.as_str()) {
            "geometry"
        } else {
            return None;
        };
        match get(&n.args, key) {
            Some(Value::Deferred(t, x)) if is_geometry_ty(t) && *x < id => Some(*x),
            _ => None,
        }
    }

    /// A count of a geometry that only writes or deletes attributes (or a card over one) is its
    /// input's count: counting the input instead, a read of counts alone keeps no chain alive.
    fn root(&self, nodes: &[ENode], mut id: i64) -> i64 {
        while let Some(at) = self.open(id) {
            match self.passes(&nodes[at], id) {
                Some(x) => id = x,
                None => break,
            }
        }
        id
    }

    fn recount(&self, nodes: &[ENode], v: &Value) -> Option<Value> {
        match v {
            Value::Source(g) | Value::Struct(g) if g.name == "sop/count" => match get(&g.fields, "geometry") {
                Some(Value::Deferred(gty, x)) if self.root(nodes, *x) != *x => {
                    let fields: Vec<(String, Value)> = g
                        .fields
                        .iter()
                        .map(|(k, v)| if k == "geometry" { (k.clone(), Value::Deferred(gty.clone(), self.root(nodes, *x))) } else { (k.clone(), v.clone()) })
                        .collect();
                    Some(if matches!(v, Value::Source(..)) { source_value(g.name.clone(), g.ty.clone(), fields) } else { struct_value(g.name.clone(), g.ty.clone(), fields) })
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn recounts(&self, nodes: &[ENode], v: &Value, seen: &mut Seen) -> bool {
        match v {
            Value::Source(..) | Value::Struct(..) if self.recount(nodes, v).is_some() => true,
            Value::Residual(r) => {
                if let Some(known) = seen.residuals.get(r) {
                    return *known;
                }
                seen.residuals.insert(*r, false);
                let found = E::residual::residual_view(*r).map(|view| view.bindings.iter().any(|(_, v)| self.recounts(nodes, v, seen))).unwrap_or(false);
                seen.residuals.insert(*r, found);
                found
            }
            Value::Fn(f) => {
                if let Some(known) = seen.functions.get(f) {
                    return *known;
                }
                seen.functions.insert(*f, false);
                let found = E::residual::function_bindings(*f).iter().any(|(_, v)| self.recounts(nodes, v, seen));
                seen.functions.insert(*f, found);
                found
            }
            Value::List(vs) => vs.iter().any(|v| self.recounts(nodes, v, seen)),
            v @ (Value::Record(_) | Value::Struct(_) | Value::Source(_)) => v.geo_fields().unwrap().iter().any(|(_, v)| self.recounts(nodes, v, seen)),
            _ => false,
        }
    }

    /// Every count of a pass-through chain counts the chain's root instead.
    fn recount_all(&self, nodes: &mut [ENode], fresh: &Fresh) {
        for id in 0..nodes.len() {
            if !self.templ[id] && nodes[id].args.iter().any(|(_, v)| self.recounts(nodes, v, &mut Seen::default())) {
                let snapshot = nodes.to_vec();
                nodes[id].args = nodes[id]
                    .args
                    .iter()
                    .map(|(k, v)| (k.clone(), E::host::rewrite_with(v, &|x: &Value| self.recount(&snapshot, x), &|| fresh.next(), &|_, _| None)))
                    .collect();
            }
        }
    }
}

/// A chain of point writes over a `sop/mesh` that nothing else reads becomes fields of the mesh's
/// record, so one program builds them: a compacted face list their maps gather is one producer on
/// the device, rather than evaluated on the CPU and uploaded for each write. A static mesh under a
/// frame-driven write keeps its own node, or the mesh would cook again every frame.
pub(super) fn fold_mesh_writes(plan: &Plan) -> Plan {
    let mut nodes = plan.nodes.clone();
    let shape = Shape::new(&nodes);
    // an earlier write `fold_writes` folded into a later one stays in the plan, unread: not a reader
    let mut folded = vec![false; nodes.len()];
    for n in &nodes {
        if let Some(Value::List(ids)) = get(&n.args, FUSED_CHAIN) {
            for id in ids.iter() {
                if let Value::Int(id) = id {
                    if *id >= 0 && *id < shape.count {
                        folded[*id as usize] = true;
                    }
                }
            }
        }
    }
    let uses = usage_counts(plan, &nodes, &folded);
    let mesh_under = |nodes: &[ENode], mut id: i64| -> Option<ENode> {
        loop {
            let at = shape.open(id).filter(|at| uses[*at] == 1)?;
            let n = &nodes[at];
            if n.kind == "sop/mesh" {
                return Some(n.clone());
            }
            if n.kind != E::card_kind() || !guards_of("guards", &n.args).is_empty() {
                return None;
            }
            id = shape.passes(n, id)?;
        }
    };
    for id in 0..nodes.len() {
        let w = nodes[id].clone();
        if w.kind == "sop/with_attr"
            && !shape.templ[id]
            && w.args.iter().all(|(k, _)| ["geometry", "attribute", "values", "owner", "type", FUSED_WRITES, FUSED_CHAIN].contains(&k.as_str()))
            && text_arg(&w.args, "owner").is_none_or(|o| o == "point")
        {
            if let (Some(Value::Deferred(_, g)), Some(name), Some(values)) = (get(&w.args, "geometry"), text_arg(&w.args, "attribute"), get(&w.args, "values")) {
                let kind = text_arg(&w.args, "type").unwrap_or_else(|| "vec3".into());
                let Ok(mut writes) = fused_entries(&w.args) else { continue };
                writes.push((name, kind, values.clone()));
                let plain = |(name, _, values): &Entry| !name.contains(':') && name != "P" && ak::sources(values).is_empty() && !matches!(values, Value::Record(_));
                if let Some(mesh) = mesh_under(&nodes, *g) {
                    let data = geometry_arrays_data(&mesh.args);
                    if writes.iter().all(plain)
                        && (E::frame_dependent(&data) || !writes.iter().any(|(_, _, v)| E::frame_dependent(v)))
                        // a stateful write folds when its mesh is stateful too: one node cooks both over the state's fork
                        && (E::state_dependent(&data) || !writes.iter().any(|(_, _, v)| E::state_dependent(v)))
                    {
                        let mut args = mesh.args.clone();
                        args.extend(writes.into_iter().map(|(name, kind, values)| (format!("{MESH_ATTRIBUTE_PREFIX}{name}:{kind}"), values)));
                        nodes[id] = ENode { kind: mesh.kind.clone(), args, ..w };
                    }
                }
            }
        }
    }
    Plan { id: E::fresh_plan_id(), nodes, ..plan.clone() }
}

fn kind_of(n: &ENode) -> String {
    text_arg(&n.args, "type").unwrap_or_else(|| "vec3".into())
}

fn deletes(n: &ENode) -> Vec<Value> {
    match get(&n.args, FUSED_DELETES) {
        Some(Value::List(ds)) => (**ds).clone(),
        _ => Vec::new(),
    }
}

/// The literal pattern of a `{owner, pattern}` deletion record.
fn pattern(v: &Value) -> Option<String> {
    match v {
        Value::Record(f) if f.len() == 2 && f[1].0 == "pattern" => match &f[1].1 {
            Value::Text(p) => Some(txt(p)),
            _ => None,
        },
        _ => None,
    }
}

/// An attribute's last write wins: the chain's writes but `name` and any attribute written again later.
fn last(entries: &[Entry], name: &str) -> Vec<Entry> {
    let mut out = Vec::new();
    for (i, e) in entries.iter().enumerate() {
        if e.0 == name || entries[i + 1..].iter().any(|m| m.0 == e.0) {
            continue;
        }
        out.push(e.clone());
    }
    out
}

/// The earlier write a write folds into: the ids passed on the way (earliest first), the geometry the
/// earlier write read and the deletions the chain performs.
struct Back {
    earlier: ENode,
    base: i64,
    chain: Vec<i64>,
    dels: Vec<Value>,
}

struct Fold {
    shape: Shape,
    uses: Vec<i64>,
    /// the write each write would fold into (a look-ahead: the read checks are not made)
    next: Vec<i64>,
    device: bool,
    defer: bool,
}

impl Fold {
    fn point_write(&self, n: &ENode) -> bool {
        n.kind == "sop/with_attr"
            && !self.shape.templ[n.id as usize]
            && n.args.iter().all(|(k, _)| ["geometry", "attribute", "values", "owner", "type", FUSED_DELETES, FUSED_WRITES, FUSED_CHAIN, FUSED_GUARDS].contains(&k.as_str()))
            && text_arg(&n.args, "owner").is_none_or(|o| o == "point")
            && text_arg(&n.args, "attribute").is_some()
            // a write of planes ({:x .. :y ..}) is not a value a read of the attribute could take
            && !matches!(get(&n.args, "values"), Some(Value::Record(_)))
    }

    /// Back from a write's geometry through cards and literal deletions of other attributes to the
    /// earlier point write of another attribute, when nothing else reads any of them.
    fn back(&self, nodes: &[ENode], name: &str, start: i64) -> Option<Back> {
        let mut id = start;
        let mut chain: Vec<i64> = Vec::new();
        let mut dels: Vec<Value> = Vec::new();
        loop {
            let at = self.shape.open(id).filter(|at| self.uses[*at] == 1)?;
            let u = &nodes[at];
            if u.kind == E::card_kind() {
                chain.push(id);
                id = self.shape.passes(u, id)?;
            } else if u.kind == "sop/delete_attr" {
                let pat = text_arg(&u.args, "attribute").filter(|pat| pat != name && literal_pattern(pat) && u.args.iter().all(|(k, _)| ["geometry", "attribute", "owner"].contains(&k.as_str())))?;
                let owner = text_arg(&u.args, "owner").unwrap_or_else(|| "point".into());
                chain.push(id);
                dels.push(Value::Record(vec![("owner".into(), Value::Text(owner.into_bytes().into())), ("pattern".into(), Value::Text(pat.into_bytes().into()))]));
                id = self.shape.passes(u, id)?;
            } else if self.point_write(u) && !deletes(u).iter().any(|d| pattern(d).as_deref() == Some(name)) {
                let Some(Value::Deferred(_, base)) = get(&u.args, "geometry") else { return None };
                chain.push(id);
                chain.reverse();
                dels.reverse();
                let mut all = deletes(u);
                all.extend(dels);
                return Some(Back { earlier: u.clone(), base: *base, chain, dels: all });
            } else {
                return None;
            }
        }
    }

    fn look_ahead(&mut self, nodes: &[ENode]) {
        for id in 0..nodes.len() {
            let w = &nodes[id];
            if let (Some(Value::Deferred(_, g)), Some(name)) = (get(&w.args, "geometry"), text_arg(&w.args, "attribute")) {
                if self.point_write(w) {
                    if let Some(b) = self.back(nodes, &name, *g) {
                        self.next[b.earlier.id as usize] = id as i64;
                    }
                }
            }
        }
    }

    fn writes_from(&self, nodes: &[ENode], mut id: i64) -> Vec<String> {
        let mut out = Vec::new();
        while id >= 0 {
            out.extend(text_arg(&nodes[id as usize].args, "attribute"));
            id = self.next[id as usize];
        }
        out
    }

    /// An indexed earlier write (a grid's P) folds only where a device makes it, and only into a chain
    /// that writes its attribute again: otherwise the chain's output would carry it, a new array with
    /// every recook (a driven noise would make P new each frame, and every reader of P cook again).
    fn indexed_fold(&self, nodes: &[ENode], u: &ENode, w: &ENode, id: i64) -> bool {
        let values = get(&u.args, "values").expect("write values");
        let fixed = (self.defer || self.device) && static_cpu_data(values);
        let next_fixed = static_cpu_data(get(&w.args, "values").expect("write values"));
        let rewritten = self.device && self.writes_from(nodes, id).iter().any(|a| Some(a) == text_arg(&u.args, "attribute").as_ref());
        // Keep a static prefix cached when the following write changes. An indexed field rewritten
        // later can still fold into that device chain.
        (!(fixed && !next_fixed) || rewritten) && (has(&u.args, FUSED_CHAIN) || !(indexed(values) && ak::reads(values).is_empty()) || (fixed && next_fixed) || rewritten)
    }

    /// The arguments write `w` (node `id`) takes when it folds into its chain, or none when it stays.
    fn folded_args(&self, nodes: &[ENode], w: &ENode, id: i64, fresh: &Fresh) -> R<Option<Vec<(String, Value)>>> {
        let (Some(Value::Deferred(_, g)), Some(name)) = (get(&w.args, "geometry"), text_arg(&w.args, "attribute")) else { return Ok(None) };
        if !self.point_write(w) {
            return Ok(None);
        }
        let Some(Back { earlier: u, base, chain, dels }) = self.back(nodes, &name, *g) else { return Ok(None) };
        if !self.indexed_fold(nodes, &u, w, id) {
            return Ok(None);
        }
        let deleted: Vec<String> = dels.iter().filter_map(pattern).collect();
        let reads = ak::reads(get(&w.args, "values").expect("write values"));
        if reads.iter().any(|r| r == "*") || deleted.iter().any(|p| reads.contains(p)) {
            return Ok(None);
        }
        // what the chain writes, in order, over `base`
        let mut entries = fused_entries(&u.args)?;
        entries.push((text_arg(&u.args, "attribute").expect("write attribute"), kind_of(&u), get(&u.args, "values").expect("write values").clone()));
        let take = Arc::new(Take { base, chain: chain.clone(), entries, fresh: fresh.clone(), fits: Arc::new(AtomicBool::new(true)) });
        let values = E::host::rewrite_with(get(&w.args, "values").expect("write values"), &|v: &Value| take.value(v), &|| fresh.next(), &{
            let take = take.clone();
            move |term: &Term, bindings: &[(String, Value)]| take.term(term, bindings)
        });
        // this write supersedes the chain's of `name`
        let earlier: Vec<Value> = last(&take.entries, &name).into_iter().map(|(n, k, v)| fused_entry(&n, &k, v)).collect();
        // the folded writes, whose cards key this one's kernels with its own
        let mut chain_ids: Vec<Value> = match get(&u.args, FUSED_CHAIN) {
            Some(Value::List(ids)) => (**ids).clone(),
            _ => Vec::new(),
        };
        chain_ids.push(Value::Int(u.id));
        // the deferred checks of the folded writes and of the cards passed
        let mut guards = guards_of(FUSED_GUARDS, &w.args);
        guards.extend(guards_of(FUSED_GUARDS, &u.args));
        for i in &chain {
            if nodes[*i as usize].kind == E::card_kind() {
                guards.extend(guards_of("guards", &nodes[*i as usize].args));
            }
        }
        if !take.fits.load(Ordering::SeqCst) {
            return Ok(None);
        }
        let mut args: Vec<(String, Value)> = w
            .args
            .iter()
            .filter_map(|(k, v)| {
                if k == "geometry" {
                    Some((k.clone(), Value::Deferred(ty::geometry(), base)))
                } else if k == "values" {
                    Some((k.clone(), values.clone()))
                } else if [FUSED_DELETES, FUSED_WRITES, FUSED_CHAIN, FUSED_GUARDS].contains(&k.as_str()) {
                    None
                } else {
                    Some((k.clone(), v.clone()))
                }
            })
            .collect();
        args.push((FUSED_CHAIN.into(), Value::List(Arc::new(chain_ids))));
        if !earlier.is_empty() {
            args.push((FUSED_WRITES.into(), Value::List(Arc::new(earlier))));
        }
        if !dels.is_empty() {
            args.push((FUSED_DELETES.into(), Value::List(Arc::new(dels))));
        }
        if !guards.is_empty() {
            args.push((FUSED_GUARDS.into(), Value::List(Arc::new(guards))));
        }
        Ok(Some(args))
    }
}

/// `device`: a fused write may run on a device. Without one an indexed earlier write (a grid's P) is
/// not folded: the CPU would compute it again with every recook of the chain, where its own node is
/// cooked once and cached; a device makes it from the element index instead of an upload.
pub(super) fn fold_writes(device: bool, defer: bool, plan: &Plan) -> R<Plan> {
    let mut nodes = plan.nodes.clone();
    let fresh = Fresh::default();
    let shape = Shape::new(&nodes);
    shape.recount_all(&mut nodes, &fresh);
    let uses = usage_counts(plan, &nodes, &vec![false; nodes.len()]);
    let mut fold = Fold { shape, uses, next: vec![-1; nodes.len()], device, defer };
    fold.look_ahead(&nodes);
    for id in 0..nodes.len() {
        let w = nodes[id].clone();
        if let Some(args) = fold.folded_args(&nodes, &w, id as i64, &fresh)? {
            nodes[id].args = args;
        }
    }
    Ok(Plan { id: E::fresh_plan_id(), nodes, ..plan.clone() })
}

/// What folding a write into the chain `chain` (over `base`) rewrites in the values it takes over:
/// reads of an attribute the chain wrote become the values written, other reads and counts of the
/// chain read `base`, and any other use of the chain makes the write stay where it is (`fits`).
struct Take {
    base: i64,
    chain: Vec<i64>,
    entries: Vec<Entry>,
    fresh: Fresh,
    fits: Arc<AtomicBool>,
}

impl Take {
    fn written(&self, attribute: &str) -> Option<&Entry> {
        self.entries.iter().rev().find(|(n, _, _)| n == attribute)
    }

    fn on_base(&self, fields: &[(String, Value)]) -> Vec<(String, Value)> {
        let mut out = vec![("geometry".to_owned(), Value::Deferred(ty::geometry(), self.base))];
        out.extend(fields.iter().filter(|(k, _)| k != "geometry").cloned());
        out
    }

    /// A struct or source value of the chain: `sop/attr` of a written attribute is its values, of
    /// another attribute and `sop/count` the same read over `base`.
    fn value(&self, v: &Value) -> Option<Value> {
        match v {
            Value::Struct(g) | Value::Source(g) => {
                let (op, ty, fields) = (&g.name, &g.ty, &g.fields);
                let chained = matches!(get(fields, "geometry"), Some(Value::Deferred(_, x)) if self.chain.contains(x));
                let rebuilt = |fields: Vec<(String, Value)>| if matches!(v, Value::Struct(..)) { struct_value(op.clone(), ty.clone(), fields) } else { source_value(op.clone(), ty.clone(), fields) };
                if !chained {
                    return None;
                }
                match (op.as_str(), get(fields, "attribute")) {
                    ("sop/attr", Some(Value::Text(a))) => match self.written(&txt(a)) {
                        None => Some(rebuilt(self.on_base(fields))),
                        Some((_, kind, earlier))
                            if fields.iter().all(|(k, v)| match k.as_str() {
                                "geometry" | "attribute" => true,
                                "owner" => *v == Value::Text(b"point".as_slice().into()),
                                "type" => *v == Value::Text(kind.clone().into_bytes().into()),
                                _ => false,
                            }) =>
                        {
                            Some(E::host::rewrite_with(earlier, &|_| None, &|| self.fresh.next(), &|_, _| None))
                        }
                        Some(_) => {
                            self.fits.store(false, Ordering::SeqCst);
                            Some(v.clone())
                        }
                    },
                    ("sop/count", _) => Some(rebuilt(self.on_base(fields))),
                    _ => {
                        self.fits.store(false, Ordering::SeqCst);
                        Some(v.clone())
                    }
                }
            }
            Value::Deferred(ty, x) if is_geometry_ty(ty) && self.chain.contains(x) => {
                self.fits.store(false, Ordering::SeqCst);
                None
            }
            _ => None,
        }
    }

    /// A lazy term that reads the chain itself (`(map f (sop/attr in0 :P))`): its reads of a written
    /// attribute become captures of the values written, and the geometry it captures is the input.
    /// Any other use of the capture refuses.
    fn term(&self, term: &Term, bindings: &[(String, Value)]) -> Option<(Term, Vec<(String, Value)>)> {
        let chained: Vec<String> = bindings
            .iter()
            .filter_map(|b| match b {
                (name, Value::Deferred(ty, x)) if is_geometry_ty(ty) && self.chain.contains(x) => Some(name.clone()),
                _ => None,
            })
            .collect();
        if chained.is_empty() {
            return None;
        }
        let mut scan = Scan { take: self, chained: &chained, extra: Vec::new(), exit: false, replacements: HashMap::new() };
        scan.visit(term);
        let mut builder = TermBuilder::new();
        let mapped = builder.rewrite(term, &mut |_, source, draft| match scan.replacements.get(&source.identity()) {
            Some(name) => Draft::RefBinding(name.clone(), Vec::new()),
            None => draft,
        });
        let mapped = builder.freeze().handle(mapped);
        if scan.exit {
            self.fits.store(false, Ordering::SeqCst);
            return None;
        }
        let mut bound: Vec<(String, Value)> = chained.iter().map(|name| (name.clone(), Value::Deferred(ty::geometry(), self.base))).collect();
        bound.extend(scan.extra);
        Some((mapped, bound))
    }
}

/// One walk over a term that reads a chain through the captures `chained`.
struct Scan<'a> {
    take: &'a Take,
    chained: &'a [String],
    /// the captures standing for the values an earlier write made
    extra: Vec<(String, Value)>,
    /// the term uses the chain in a way a fold cannot follow
    exit: bool,
    replacements: HashMap<(u64, u32), String>,
}

fn literal_text(t: &Term) -> Option<String> {
    match t.node {
        W::Node::Text(id) => Some(txt(t.arena().bytes(id))),
        _ => None,
    }
}

impl Scan<'_> {
    fn binds(&self, t: &Term, p: flow_check::term_arena::PatternId) -> bool {
        use flow_check::term_arena::PatternNode;
        match t.arena().pattern(p) {
            PatternNode::Name(n) => self.chained.iter().any(|s| s == t.name(n)),
            PatternNode::Seq(ps) => t.arena().pattern_ids(ps).iter().any(|&p| self.binds(t, p)),
            PatternNode::Keys(ns) => t.arena().name_ids(ns).iter().any(|&n| self.chained.iter().any(|s| s == t.name(n))),
        }
    }

    fn visit(&mut self, t: &Term) {
        let reads_chain = |args| {
            t.arena().fields(args).iter().find(|field| t.name(field.name) == "geometry").is_some_and(|field| {
                let child = t.child(field.term);
                match child.node {
                    W::Node::RefBinding(id) => {
                        let binding = child.arena().ref_binding(id);
                        child.arena().name_ids(binding.fields).is_empty() && self.chained.iter().any(|s| s == child.name(binding.name))
                    }
                    _ => false,
                }
            })
        };
        match t.node {
            W::Node::RefBinding(id) if self.chained.iter().any(|s| s == t.name(t.arena().ref_binding(id).name)) => {
                self.exit = true;
                return;
            }
            W::Node::Op(id) if t.name(t.arena().op(id).op) == "sop/count" && reads_chain(t.arena().op(id).args) => return,
            W::Node::Op(id) if t.name(t.arena().op(id).op) == "sop/attr" && reads_chain(t.arena().op(id).args) => {
                let args = t.arena().fields(t.arena().op(id).args);
                let attribute = args.iter().find(|field| t.name(field.name) == "attribute").and_then(|field| literal_text(&t.child(field.term)));
                match attribute.as_deref().map(|a| self.take.written(a)) {
                    Some(None) => {}
                    Some(Some((_, kind, earlier)))
                        if args.iter().all(|field| match t.name(field.name) {
                            "geometry" | "attribute" => true,
                            "owner" => literal_text(&t.child(field.term)).as_deref() == Some("point"),
                            "type" => literal_text(&t.child(field.term)).as_deref() == Some(kind.as_str()),
                            _ => false,
                        }) =>
                    {
                        let name = format!("$fold{}", self.extra.len());
                        self.extra.push((name.clone(), E::host::rewrite_with(earlier, &|_| None, &|| self.take.fresh.next(), &|_, _| None)));
                        self.replacements.insert(t.identity(), name);
                    }
                    _ => self.exit = true,
                }
                return;
            }
            W::Node::Let(id) if t.arena().bindings(t.arena().let_node(id).bindings).iter().any(|binding| self.binds(t, binding.pattern)) => {
                self.exit = true;
                return;
            }
            W::Node::Loop(id)
                if {
                    let node = t.arena().loop_node(id);
                    t.arena().bindings(node.accs).iter().chain(t.arena().bindings(node.clauses)).any(|binding| self.binds(t, binding.pattern))
                } =>
            {
                self.exit = true;
                return;
            }
            W::Node::Fn(id) if t.arena().params(t.arena().function(id).params).iter().any(|param| self.binds(t, param.pattern)) => {
                self.exit = true;
                return;
            }
            W::Node::State(id) if self.binds(t, t.arena().state(id).binder) => {
                self.exit = true;
                return;
            }
            _ => {}
        }
        for &child in t.children() {
            self.visit(&t.child(child));
        }
    }
}

fn fused_entry(name: &str, kind: &str, values: Value) -> Value {
    Value::Record(vec![("attribute".into(), Value::Text(name.as_bytes().into())), ("type".into(), Value::Text(kind.as_bytes().into())), ("values".into(), values)])
}

/// whether a write's values map an index (`array/range`); one that reads no attribute too is a
/// generated attribute (a grid's P)
fn indexed(value: &Value) -> bool {
    fn term(t: &Term) -> bool {
        matches!(t.node, W::Node::Op(id) if t.name(t.arena().op(id).op) == "array/range")
            || t.children().iter().any(|&id| term(&t.child(id)))
    }
    fn visit(v: &Value, seen: &mut BTreeSet<i64>) -> bool {
        match v {
            Value::Residual(r) => {
                let id = E::residual::residual_id(*r);
                if !seen.insert(id) {
                    return false;
                }
                let Ok(view) = E::residual::residual_view(*r) else { return false };
                term(&view.term) || view.bindings.iter().any(|(_, v)| visit(v, seen))
            }
            Value::Record(fs) => fs.iter().any(|(_, v)| visit(v, seen)),
            Value::List(vs) => vs.iter().any(|v| visit(v, seen)),
            _ => false,
        }
    }
    visit(value, &mut BTreeSet::new())
}
