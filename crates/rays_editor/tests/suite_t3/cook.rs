//! Worker barriers block a node cook; owner-thread preparation stays outside the cook worker.
use super::t3c::*;
use super::support::*;
use editor_document::settings;
use flow_sop::network::{self, Network};
use param::{Effects, Value};
use rays_editor::cook::{self, Key2};
use rays_editor::{pick, timeline as tl};
use sop::async_cook::Status;
use sop::context::dependencies as deps;
use sop::edit_graph;
use sop::node::{self, build, CookMode};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::{Arc, Condvar, Mutex};

pub type Obj = (i64, Network, i64);

pub fn network_of(id: i64, graph: &node::T) -> Obj {
    (id, network::of_geometry(&edit_graph::of_graph(graph)), node::id(graph))
}

pub fn clone_obj(o: &Obj) -> Obj {
    (o.0, o.1.clone(), o.2)
}

fn schema() -> Arc<param::Schema> {
    let kind = param::integer(0, 100, None, None).expect("kind");
    let field = param::field("value".into(), kind, Value::Int(0)).expect("field");
    Arc::new(param::schema("command-value".into(), vec![field]).expect("schema"))
}

pub fn settings_of(mode: i64) -> settings::T {
    thread_local! { static S: Arc<param::Schema> = schema(); }
    S.with(|s| settings::make(s, vec![Value::Int(mode)]))
}

fn mode_of(s: &settings::T) -> i64 {
    match settings::get(settings::schema_of(s), s).expect("values").first() {
        Some(Value::Int(i)) => *i,
        other => panic!("not an int: {other:?}"),
    }
}

fn still_frame() -> rays::frame::Frame {
    let mut f = frame_at((0., 0.), vec![], 0);
    f.input.dt = 0.;
    f
}

/// Empty settings compare through their schema, so retain one settings value across unchanged cook requests.
pub fn none_settings() -> settings::T {
    thread_local! { static NONE: settings::T = settings::none(); }
    NONE.with(|n| n.clone())
}

pub fn stopped() -> tl::Timeline {
    tl::stop(&tl::create()).0
}

/// The `with_cook` rig: a cook and the `step` / `finish` helpers over it.
pub struct Rig {
    pub cook: cook::Cook,
}

pub struct StepOptions {
    pub settings: settings::T,
    pub timeline: tl::Timeline,
    pub changes: Vec<tl::Change>,
    pub buttons: Vec<input::MouseButton>,
    pub probes: Vec<Key2>,
}

impl Default for StepOptions {
    fn default() -> StepOptions {
        StepOptions { settings: none_settings(), timeline: stopped(), changes: Vec::new(), buttons: Vec::new(), probes: Vec::new() }
    }
}

impl Rig {
    pub fn new(prepare: cook::PrepareFn, domains: i64) -> Rig {
        Rig { cook: cook::create(prepare, 42, 64, Some(domains), None, None, 16, 16 * 1024 * 1024).expect("cook") }
    }
    pub fn step(&mut self, o: &StepOptions, objects: &[Obj]) -> cook::UpdateResult {
        let mut frame = still_frame();
        frame.input.buttons = o.buttons.clone();
        let u = cook::update(None, None, false, &o.probes, &pick::Set::default(), None, &self.cook, &o.settings, &objects.iter().map(|(i, n, d)| (*i, n, *d)).collect::<Vec<_>>(), None, Effects::default(), &o.changes, &o.timeline, &frame, None);
        self.cook = u.cook.clone();
        u
    }
    /// Steps until a cook is published (the first step carries `changes`), returns that update.
    pub fn finish(&mut self, o: &StepOptions, objects: &[Obj]) -> cook::UpdateResult {
        let mut published = None;
        let mut first = true;
        await_until(|| {
            let step_o = StepOptions { settings: o.settings.clone(), timeline: o.timeline.clone(), changes: if first { o.changes.clone() } else { Vec::new() }, buttons: Vec::new(), probes: Vec::new() };
            let u = self.step(&step_o, objects);
            first = false;
            if u.prepared_changed && cook::status(&self.cook) == Status::Idle {
                published = Some(u);
            }
            published.is_some()
        });
        published.expect("published")
    }
    pub fn pieces(&self) -> &[cook::Piece] {
        cook::pieces(&self.cook)
    }
    pub fn piece(&self, id: i64) -> &cook::Piece {
        self.pieces().iter().find(|p| p.id == id).unwrap_or_else(|| panic!("no piece {id}"))
    }
}

impl Drop for Rig {
    fn drop(&mut self) {
        cook::close(&self.cook);
    }
}

/// A gate a worker thread waits at until released.
#[derive(Clone)]
pub struct Barrier {
    pub entered: Arc<AtomicBool>,
    state: Arc<(Mutex<bool>, Condvar)>,
}

impl Barrier {
    pub fn new() -> Barrier {
        Barrier { entered: Arc::new(AtomicBool::new(false)), state: Arc::new((Mutex::new(false), Condvar::new())) }
    }
    pub fn wait(&self) {
        let (m, c) = &*self.state;
        let mut released = m.lock().unwrap();
        self.entered.store(true, Ordering::SeqCst);
        while !*released {
            released = c.wait(released).unwrap();
        }
    }
    pub fn release(&self) {
        let (m, c) = &*self.state;
        *m.lock().unwrap() = true;
        c.notify_all();
    }
}

/// `source` wrapped in a node whose first cook waits at the barrier.
fn blocking(source: &node::T, barrier: &Barrier, first: &Arc<AtomicBool>, exited: Option<Arc<AtomicBool>>) -> node::T {
    let (barrier, first) = (barrier.clone(), first.clone());
    let args = sop::custom::PlainArgs { dependencies: deps::STATIC, ..sop::custom::plain_args("editor_cook_barrier") };
    sop::custom::plain(
        args,
        vec![source.clone()],
        Arc::new(move |_, inputs| {
            if first.swap(false, Ordering::SeqCst) {
                barrier.wait();
                if let Some(exited) = &exited {
                    exited.store(true, Ordering::SeqCst);
                }
            }
            Ok(inputs[0].clone())
        }),
    )
}

/// `(sop/transform (sop/ext_source) :mode "Matrix" :m03 x)` as a plain custom node: the reuse checks are
/// about the cook, and lowering a Lisp node costs seconds in a debug build.
pub fn moved(source: &node::T, x: f64) -> node::T {
    let args = sop::custom::PlainArgs { parameters: x.to_bits().to_le_bytes().to_vec(), ..sop::custom::plain_args("editor_cook_moved") };
    sop::custom::plain(
        args,
        vec![source.clone()],
        Arc::new(move |_, inputs| {
            let p = rdk_core::geometry::positions(&inputs[0]);
            let (xs, ys, zs): (Vec<f64>, Vec<f64>, Vec<f64>) = (0..rdk_core::packed::float3::length(p)).fold((vec![], vec![], vec![]), |mut a, i| {
                let (px, py, pz) = rdk_core::packed::float3::get(p, i).unwrap();
                a.0.push(px + x);
                a.1.push(py);
                a.2.push(pz);
                a
            });
            let positions = rdk_core::packed::float3::of_owned(xs, ys, zs).map_err(|e| format!("{e:?}"))?;
            rdk_core::geometry::with_positions(positions, &inputs[0]).map_err(|e| format!("{e:?}"))
        }),
    )
}

pub fn source_geometry() -> node::T {
    snapshot(&rdk_ops::line_geometry::points(&[(0., 0., 0.)]).expect("point"))
}

/// A published geometry keeps its CPU preparation consumer budget.
pub fn consumers() {
    let seen: Arc<Mutex<Option<Option<i64>>>> = Arc::new(Mutex::new(None));
    let seen2 = seen.clone();
    let published = build::make_geometry(
        build::Args { cook_mode: CookMode::Generator, dependencies: deps::STATIC, ..build::args("test_prepared_consumer") },
        None,
        Arc::new(move |node_id, context, _| {
            *seen2.lock().unwrap() = Some(sop::context::cpu_consumers(context, node_id));
            Ok(node::GeometryCooked { geometry: rdk_ops::line_geometry::points(&[(0., 0., 0.)]).unwrap(), instances: None })
        }),
    );
    let mut rig = Rig::new(prepare_empty(), 1);
    rig.finish(&StepOptions::default(), &[network_of(1, &published)]);
    assert!(*seen.lock().unwrap() == Some(Some(1)), "published geometry lost its CPU preparation consumer budget");
}

pub fn reuse() {
    let source = source_geometry();
    let objects = vec![network_of(1, &source), network_of(2, &source)];
    let prepares = Arc::new(AtomicI64::new(0));
    let counter = prepares.clone();
    let prepare: cook::PrepareFn = Rc::new(move |_, _| {
        counter.fetch_add(1, Ordering::SeqCst);
        empty_prepared()
    });
    let mut rig = Rig::new(prepare, 1);
    let (a, b) = (settings_of(0), settings_of(1));
    let opts = |s: &settings::T| StepOptions { settings: s.clone(), ..Default::default() };
    rig.finish(&opts(&a), &objects);
    let first_prepared = rig.piece(1).prepared.clone();
    let second = rig.piece(2).prepared.clone();
    let translation = 2.0;
    let changed = network_of(1, &moved(&source, translation));
    let changed_objects = vec![clone_obj(&changed), clone_obj(&objects[1])];
    rig.finish(&opts(&a), &changed_objects);
    assert!(prepares.load(Ordering::SeqCst) == 3, "one changed object re-prepared its static sibling: {}", prepares.load(Ordering::SeqCst));
    assert!(Rc::ptr_eq(&rig.piece(2).prepared, &second), "static prepared piece lost identity");
    rig.finish(&opts(&b), &changed_objects);
    assert!(prepares.load(Ordering::SeqCst) == 5 && rig.pieces().iter().all(|p| settings::same_values(&p.settings, &b)), "settings did not invalidate both prepares without a cook effect");
    rig.finish(&opts(&a), &objects);
    assert!(prepares.load(Ordering::SeqCst) == 5 && Rc::ptr_eq(&rig.piece(1).prepared, &first_prepared), "returning to a cached output rebuilt its prepared surface");
    rig.finish(&opts(&b), &changed_objects);
    assert!(prepares.load(Ordering::SeqCst) == 5, "returning to cached settings rebuilt their prepared surfaces");
    for _ in 0..5 {
        rig.step(&opts(&b), &changed_objects);
    }
    assert!(prepares.load(Ordering::SeqCst) == 5, "idle frames resubmitted static objects");
    // footers are counted by a job of their own after the display: the pieces stay, nothing is
    // prepared again, and the counts arrive
    let pieces: Vec<Rc<cook::Prepared>> = rig.pieces().iter().map(|p| p.prepared.clone()).collect();
    let node_id = objects[0].2;
    let probes = vec![(1, node_id)];
    await_until(|| {
        rig.step(&StepOptions { settings: b.clone(), probes: probes.clone(), ..Default::default() }, &changed_objects);
        cook::geometry(&rig.cook, 1, node_id).is_some()
    });
    assert!(
        pieces.iter().zip(rig.pieces()).all(|(a, p)| Rc::ptr_eq(a, &p.prepared)) && rig.pieces().len() == pieces.len() && prepares.load(Ordering::SeqCst) == 5,
        "counting a footer re-prepared the displayed objects"
    );
    // a scrub counts no footer: neither the edit's display cook nor the recook a live document makes
    // the next frame carries any; the counts arrive once, after the edits settle
    let scrubbed = |i: i64| network_of(1, &moved(&source, i as f64));
    let live_update = |rig: &mut Rig, recook: bool, objects: &[Obj]| {
        let u = cook::update(
            None,
            None,
            true,
            &probes,
            &pick::Set::default(),
            None,
            &rig.cook,
            &b,
            &objects.iter().map(|(i, n, d)| (*i, n, *d)).collect::<Vec<_>>(),
            None,
            Effects { cook: recook, ..Effects::default() },
            &[],
            &stopped(),
            &still_frame(),
            None,
        );
        rig.cook = u.cook.clone();
        u
    };
    let before = rig.cook.summaries.len();
    for i in 1..=5 {
        let objs = vec![scrubbed(i), clone_obj(&objects[1])];
            for recook_run in [false, true] {
                    let mut first = true;
            await_until(|| {
                if cook::status(&rig.cook) != Status::Idle {
                    return false;
                }
                let u = live_update(&mut rig, recook_run && first, &objs);
                first = false;
                assert!(rig.cook.summaries.len() == before, "a scrub step counted footers");
                u.prepared_changed && cook::status(&rig.cook) == Status::Idle
            });
        }
    }
    let objs = vec![scrubbed(0), clone_obj(&objects[1])];
    // Footer summaries arrive after the footer-only probe job finishes and edits settle.
    let mut counted = false;
    await_until(|| {
        live_update(&mut rig, false, &objs);
        counted |= rig.cook.probe_job;
        counted && !rig.cook.probe_job && cook::status(&rig.cook) == Status::Idle
    });
    assert!(!rig.cook.summaries.is_empty(), "the counts never arrived");
}

/// Time/Frame prepare, static siblings, seek back.
pub fn context_provenance() {
    let source = source_geometry();
    let dynamic = sop::custom::plain(
        sop::custom::PlainArgs { dependencies: deps::union(deps::one(deps::Fact::Time), deps::one(deps::Fact::Frame)), ..sop::custom::plain_args("editor_cook_clock") },
        vec![source.clone()],
        Arc::new(|context, inputs| {
            let positions = rdk_core::packed::float3::of_owned(vec![sop::context::time(context)], vec![sop::context::frame(context) as f64], vec![0.]).map_err(|e| format!("{e:?}"))?;
            rdk_core::geometry::with_positions(positions, &inputs[0]).map_err(|e| format!("{e:?}"))
        }),
    );
    let mut rig = Rig::new(prepare_surface(), 1);
    let objects = vec![network_of(1, &dynamic), network_of(2, &source)];
    let o = StepOptions::default();
    rig.finish(&o, &objects);
    let static_prepared = rig.piece(2).prepared.clone();
    let (timeline, changes) = tl::seek(&stopped(), 60);
    rig.finish(&StepOptions { timeline: timeline.clone(), changes, ..Default::default() }, &objects);
    assert!(first_point(&rig.piece(1).prepared) == (1., 60., 0.), "Time/Frame prepare reused stale output: {:?}", first_point(&rig.piece(1).prepared));
    assert!(Rc::ptr_eq(&rig.piece(2).prepared, &static_prepared), "dynamic context invalidated a static sibling");
    let (timeline, changes) = tl::seek(&timeline, 0);
    rig.finish(&StepOptions { timeline, changes, ..Default::default() }, &objects);
    assert!(first_point(&rig.pieces()[0].prepared) == (0., 0., 0.), "seek back kept stale context");
}

/// A live drive: the box's scale is the clock; the same bytes at one and three domains.
pub fn live_drive() {
    let doc = flow_syntax::syntax::parse("(workspace live (graph g :context sop (sop/box :uniform_scale t)))").unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let lowered = flow_sop::lower::workspace(None, editor_document::kinds::descriptors(), flow_sop::lower::Options::default(), &doc).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = lowered.graphs[0].clone();
    let root = graph.root.expect("root");
    let net = graph.network.clone();
    let target = flow_sop::port::Port { node: root, path: "uniform_scale".to_string() };
    let points_of = |p: &cook::Prepared| -> Vec<(u64, u64, u64)> {
        let pts = rdk_core::geometry::positions(&p.geometry);
        (0..rdk_core::packed::float3::length(pts)).map(|i| rdk_core::packed::float3::get(pts, i).map(|(x, y, z)| (x.to_bits(), y.to_bits(), z.to_bits())).unwrap()).collect()
    };
    let objects = vec![(1, net.clone(), root)];
    let applied = |rig: &Rig| rig.cook.applied.get(&1).expect("applied").applied.get(&target).cloned();
    let one_domain;
    {
        let mut rig = Rig::new(prepare_surface(), 1);
        let (timeline, changes) = tl::seek(&stopped(), 60);
        rig.finish(&StepOptions { timeline: timeline.clone(), changes, ..Default::default() }, &objects);
        let first = points_of(&rig.pieces()[0].prepared);
        one_domain = first.clone();
        assert!(format!("{:?}", applied(&rig)).contains("1.0"), "cook skipped the time drive at frame 60: {:?}", applied(&rig));
        let (timeline2, changes) = tl::seek(&timeline, 120);
        rig.finish(&StepOptions { timeline: timeline2.clone(), changes, ..Default::default() }, &objects);
        let second = points_of(&rig.pieces()[0].prepared);
        assert!(first != second && format!("{:?}", applied(&rig)).contains("2.0"), "timeline seek did not apply a new value before geometry cooking: {:?}", applied(&rig));
        let before = rig.pieces()[0].prepared.clone();
        for _ in 0..3 {
            rig.step(&StepOptions { timeline: timeline2.clone(), ..Default::default() }, &objects);
        }
        assert!(Rc::ptr_eq(&rig.pieces()[0].prepared, &before), "unchanged time drive resubmitted a cook");
    }
    let mut rig = Rig::new(prepare_surface(), 3);
    let (timeline, changes) = tl::seek(&stopped(), 60);
    rig.finish(&StepOptions { timeline, changes, ..Default::default() }, &objects);
    assert!(points_of(&rig.pieces()[0].prepared) == one_domain, "driven geometry differed between one and three domains");
}

/// Delayed completion retains submitted settings; superseded or emptied requests never publish.
pub fn barriers() {
    let source = source_geometry();
    let prepare_x: cook::PrepareFn = Rc::new(|s, _| prepared_at(mode_of(s) as f64));
    // 1. delayed completion
    {
        let barrier = Barrier::new();
        let first = Arc::new(AtomicBool::new(true));
        let wrapped = blocking(&source, &barrier, &first, None);
        let objects = vec![network_of(1, &wrapped), network_of(2, &wrapped)];
        let mut rig = Rig::new(prepare_x.clone(), 1);
        let (a, b) = (settings_of(0), settings_of(1));
        rig.step(&StepOptions { settings: a.clone(), ..Default::default() }, &objects);
        await_until(|| barrier.entered.load(Ordering::SeqCst));
        rig.step(&StepOptions { settings: b.clone(), buttons: vec![input::MouseButton::LeftButton], ..Default::default() }, &objects);
        barrier.release();
        await_until(|| cook::status(&rig.cook) == Status::Idle);
        rig.step(&StepOptions { settings: b.clone(), buttons: vec![input::MouseButton::LeftButton], ..Default::default() }, &objects);
        assert!(
            rig.pieces().iter().all(|p| x_of(&p.prepared) == 0. && settings::same_values(&p.settings, &a)) && rig.pieces().len() == 2,
            "delayed completion was stamped with the current settings: {:?}",
            rig.pieces().iter().map(|p| (x_of(&p.prepared), mode_of(&p.settings))).collect::<Vec<_>>()
        );
        rig.finish(&StepOptions { settings: b.clone(), ..Default::default() }, &objects);
        assert!(rig.pieces().iter().all(|p| x_of(&p.prepared) == 1.), "held settings change was not replaced after release");
        barrier.release();
    }
    // 2. superseded completion, with objects kept or removed
    for removed in [false, true] {
        let barrier = Barrier::new();
        let first = Arc::new(AtomicBool::new(true));
        let wrapped = blocking(&source, &barrier, &first, None);
        let objects = vec![network_of(1, &wrapped), network_of(2, &wrapped)];
        let mut rig = Rig::new(prepare_x.clone(), 1);
        rig.step(&StepOptions { settings: settings_of(0), ..Default::default() }, &objects);
        await_until(|| barrier.entered.load(Ordering::SeqCst));
        let latest: Vec<Obj> = if removed { Vec::new() } else { objects.iter().map(clone_obj).collect() };
        let b = settings_of(2);
        rig.step(&StepOptions { settings: b.clone(), ..Default::default() }, &latest);
        assert!(!removed || rig.pieces().is_empty(), "empty objects retained previews");
        barrier.release();
        rig.finish(&StepOptions { settings: b.clone(), ..Default::default() }, &latest);
        let ok = if removed { rig.pieces().is_empty() } else { rig.pieces().iter().all(|p| x_of(&p.prepared) == 2. && settings::same_values(&p.settings, &b)) };
        assert!(ok, "superseded completion republished old objects/settings (removed={removed})");
    }
}

/// close() joins the worker: it returns only after the blocked callback exited.
pub fn close_joins() {
    let source = source_geometry();
    let barrier = Barrier::new();
    let first = Arc::new(AtomicBool::new(true));
    let exited = Arc::new(AtomicBool::new(false));
    let wrapped = blocking(&source, &barrier, &first, Some(exited.clone()));
    let objects = vec![network_of(1, &wrapped), network_of(2, &wrapped)];
    let mut rig = Rig::new(prepare_empty(), 1);
    rig.step(&StepOptions::default(), &objects);
    await_until(|| barrier.entered.load(Ordering::SeqCst));
    let closing = Arc::new(AtomicBool::new(false));
    let (c2, b2) = (closing.clone(), barrier.clone());
    let unblock = std::thread::spawn(move || {
        await_until(|| c2.load(Ordering::SeqCst));
        b2.release();
    });
    closing.store(true, Ordering::SeqCst);
    cook::close(&rig.cook);
    unblock.join().expect("unblock thread");
    assert!(exited.load(Ordering::SeqCst), "close returned before the callback exited");
    barrier.release();
}
