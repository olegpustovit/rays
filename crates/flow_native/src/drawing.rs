//! Port of lib/flow_native/drawing.ml: native Scene lowering of Flow's typed deferred drawing plan.
//! Uses the workspace evaluator and the environment's fold state.
use crate::marks_scene::{self, attribute};
use flow_check::workspace::Paths;
use flow_eval as e;
use flow_ir::core_exec::{self as c, Buffers};
use flow_ir::core_draw;
use flow_ir::flow_ir::{Executor as I, Gpu as G, Profile};
use flow_syntax::ty;
use flow_syntax::value::{self as v, ResidualId, Value};
use input::Frame;
use rays::scene;
use rays_diagnostic::Diagnostic;
use std::cell::{Cell, OnceCell, RefCell};
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

type Packed = flow_ir::packed::T;

/// Numbers the GPU mark sets of different drawings apart in the backend's sources.
static DRAWINGS: AtomicI64 = AtomicI64::new(0);

/// A resident fold, and how a mark attribute over the arrays it kept after its loop is drawn
/// (`Workspace_gpu::resident`): one host does both.
pub type show = Rc<dyn Fn(&Packed, &Frame) -> Option<Option<G::Value>>>;
pub type resident = (Arc<c::Resident>, show);

type Core = (c::Prepared, Option<ResidualId>, Buffers);

pub struct prepared {
    id: i64,
    states: I::Program,
    plan: e::Plan,
    root: Value,
    approx: Paths,
    fast: bool,
    core: OnceCell<Result<Core, Diagnostic>>,
    whole: bool,
    cache: marks_scene::cache,
    display: Arc<I::RegionDisplay>,
    readback: Arc<I::RegionReadback>,
    /// the last scene's mark sets bind GPU outputs a later draw replaces
    kept: RefCell<Option<((i64, i64), scene::t)>>,
    borrows: Cell<bool>,
}

/// The optional arguments of [`prepare`].
#[derive(Clone, Default)]
pub struct PrepareOptions {
    pub gpu: Option<G::Config>,
    pub profile: Option<Arc<Profile::T>>,
    pub approx: Option<Paths>,
    pub states: Vec<Value>,
}

/// Prepare each drawing argument's IR program once for playback or export.
pub fn prepare(o: &PrepareOptions, plan: &e::Plan, root: &Value) -> Result<prepared, Diagnostic> {
    let gpu = o.gpu.clone().unwrap_or_else(G::default);
    let approx = o.approx.clone().unwrap_or_default();
    let states = I::compile(&Value::List(std::sync::Arc::new(o.states.clone())), &I::CompileOptions { profile: o.profile.clone(), ..Default::default() })?;
    let mut known: Vec<Option<bool>> = vec![None; plan.nodes.len()];
    fn static_(plan: &e::Plan, known: &mut Vec<Option<bool>>, value: &Value) -> bool {
        match value {
            Value::Deferred(t, id) if *t == ty::drawing() => node(plan, known, *id),
            Value::Deferred(_, _) => false,
            Value::List(values) => values.iter().all(|x| static_(plan, known, x)),
            v @ (Value::Record(_) | Value::Struct(_) | Value::Source(_)) => v.geo_v.geo_fields().unwrap()().unwrap().iter().all(|(_, x)| static_(plan, known, x)),
            other => !e::is_live(other),
        }
    }
    fn node(plan: &e::Plan, known: &mut Vec<Option<bool>>, id: i64) -> bool {
        match known.get(id as usize).copied().flatten() {
            Some(value) => value,
            None => {
                if id >= 0 && (id as usize) < known.len() {
                    known[id as usize] = Some(false);
                }
                let args = plan.nodes[id as usize].args.clone();
                let value = args.iter().all(|(_, x)| static_(plan, known, x));
                known[id as usize] = Some(value);
                value
            }
        }
    }
    let whole = static_(plan, &mut known, root);
    Ok(prepared {
        id: DRAWINGS.fetch_add(1, Ordering::SeqCst),
        states,
        plan: plan.clone(),
        root: root.clone(),
        approx: approx.clone(),
        fast: gpu.fast,
        core: OnceCell::new(),
        whole,
        cache: marks_scene::cache_new(),
        display: Arc::new(I::region_display(Some(gpu.clone()), o.profile.clone(), Some(approx))),
        readback: Arc::new(I::region_readback(Some(gpu))),
        kept: RefCell::new(None),
        borrows: Cell::new(false),
    })
}

fn core(p: &prepared) -> &Result<Core, Diagnostic> {
    p.core.get_or_init(|| match core_draw::lower(None, &p.plan, &p.root) {
        Ok((program, residual)) => Ok((c::prepare(&program, false, true, &p.approx, p.fast), residual, Buffers::create())),
        Err(name) => Err(v::fail("E_DRAW_KIND", &format!("The drawing cannot be lowered to the core path ({name})."))),
    })
}

/// The last rendered scene binds GPU outputs (a mark set's `Device` attributes) that the next
/// render replaces: after a failed render it is stale and must not be shown again.
pub fn borrows_gpu(p: &prepared) -> bool {
    p.borrows.get()
}

pub type image_resolver = Rc<dyn Fn(&Value) -> Result<rays::image::t, Diagnostic>>;

/// The optional arguments of [`render_prepared`].
#[derive(Clone, Default)]
pub struct RenderOptions {
    pub state: Option<e::State>,
    pub image: Option<image_resolver>,
    pub gpu: Option<Rc<marks_scene::gpu>>,
    pub resident: Option<resident>,
}

struct ImageFrame {
    resolve: image_resolver,
    images: Vec<rays::image::t>,
}

thread_local! {
    /// The image resolvers of the renders running on this thread, innermost last: the core's
    /// `ImageFn` must be `Send + Sync`, so it names its frame by depth and finds it here.
    static IMAGES: RefCell<Vec<ImageFrame>> = const { RefCell::new(Vec::new()) };
}

struct PopImages;
impl Drop for PopImages {
    fn drop(&mut self) {
        IMAGES.with(|s| s.borrow_mut().pop());
    }
}


/// The draw program runs into a mark table which `Marks_scene` adapts. Retention is whole-program:
/// a fully static draw graph is kept per size; the fold states still advance.
fn render_core(core: &Core, state: &e::State, p: &prepared, live: &Frame, size: (i64, i64), o: &RenderOptions) -> Result<scene::t, Diagnostic> {
    let (program, root, buffers) = core;
    let (resident, show) = match &o.resident {
        Some((r, s)) => (Some(r.clone()), Some(s.clone())),
        None => (None, None),
    };
    let mut result: Option<scene::t> = None;
    e::transaction(state, &mut || {
        I::force(&p.states, &I::ForceOptions { state: Some(state.clone()), ..Default::default() }, live)?;
        if let Some((kept, scene)) = &*p.kept.borrow() {
            if p.whole && *kept == size {
                result = Some(scene.clone());
                return Ok(());
            }
        }
        buffers.clear();
        // the images this draw resolves, numbered from 1
        let depth = o.image.as_ref().map(|resolve| {
            IMAGES.with(|s| {
                s.borrow_mut().push(ImageFrame { resolve: resolve.clone(), images: Vec::new() });
                s.borrow().len() - 1
            })
        });
        let _pop = depth.map(|_| PopImages);
        let image: Option<c::ImageFn> = depth.map(|depth| {
            let f: c::ImageFn = Arc::new(move |value: &Value| {
                let resolve = IMAGES.with(|s| s.borrow().get(depth).map(|f| f.resolve.clone()));
                let Some(resolve) = resolve else {
                    return Err(v::fail("E_IMAGE", "Drawing images must resolve on the render's own thread."));
                };
                let loaded = resolve(value)?;
                Ok(IMAGES.with(|s| {
                    let mut s = s.borrow_mut();
                    s[depth].images.push(loaded);
                    s[depth].images.len() as i64
                }))
            });
            f
        });
        // A region offered as a mark attribute may stay on the GPU; each runs at most once a
        // frame, since a second dispatch would overwrite the first's output.
        let id = p.id;
        let gpu_wrapped: Option<Box<marks_scene::gpu>> = o.gpu.clone().map(|gpu| {
            let f: Box<marks_scene::gpu> = Box::new(move |slot, kind, count, attributes: &[attribute]| gpu(slot | (id << 20), kind, count, attributes));
            f
        });
        let devices: Rc<RefCell<Vec<G::Value>>> = Rc::new(RefCell::new(Vec::new()));
        let ran: Rc<RefCell<Vec<usize>>> = Rc::new(RefCell::new(Vec::new()));
        let device: Option<c::DeviceFn> = gpu_wrapped.as_ref().map(|_| {
            let (devices, ran, show, display, live) = (devices.clone(), ran.clone(), show.clone(), p.display.clone(), live.clone());
            let f: c::DeviceFn = Arc::new(move |packed: &Packed| {
                let code = Arc::as_ptr(&packed.code) as usize;
                if ran.borrow().contains(&code) {
                    return Ok(None);
                }
                // over a resident fold's kept arrays, its host draws it (or the CPU does)
                let shown = match show.as_ref().and_then(|s| s(packed, &live)) {
                    Some(shown) => shown,
                    None => display.call(None, None, packed, &live)?,
                };
                Ok(shown.map(|value| {
                    ran.borrow_mut().push(code);
                    let view = flow_ir::packed::host::view(packed);
                    view.outputs
                        .iter()
                        .enumerate()
                        .map(|(output, registers)| {
                            let width = registers.len() as i64;
                            let mut devices = devices.borrow_mut();
                            devices.push(G::Value { output: output as i64, width, ..value.clone() });
                            (devices.len() as i64 - 1, value.count, width)
                        })
                        .collect()
                }))
            });
            f
        });
        // in fast precision a region the drawing computes with may run on the GPU; a refusal or a
        // device error leaves it to the CPU
        let force: Option<c::ForceFn> = gpu_wrapped.as_ref().map(|_| {
            let (readback, live) = (p.readback.clone(), live.clone());
            let f: c::ForceFn = Arc::new(move |packed: &Packed| match readback.call(packed, &live) {
                Ok(Some(value)) => Ok(value),
                Ok(None) | Err(_) => flow_ir::packed::force(packed, &live, &Default::default()),
            });
            f
        });
        let draw_options = c::DrawOptions { root: None, force, resolve: None, device, resident: resident.clone(), image };
        let marks = match root {
            None => c::draw(program, live, size, buffers, &draw_options)?,
            Some(root) => {
                // Bind the residual's evaluator state plane before the core draws directly.
                e::Residual::bind_live(*root, live, Some(state))?;
                c::draw(program, live, size, buffers, &c::DrawOptions { root: Some(*root), ..draw_options })?
            }
        };
        let table: Vec<rays::image::t> = match depth {
            Some(depth) => IMAGES.with(|s| s.borrow()[depth].images.clone()),
            None => Vec::new(),
        };
        let table_devices = devices.borrow().clone();
        let image_of = |id: i64| table[(id - 1) as usize].clone();
        let device_of = |id: i64| -> Result<G::Value, Diagnostic> {
            let (token, _, _) = buffers.device(id).ok_or_else(|| v::fail("E_DRAW_RANGE", "A device attribute is not bound."))?;
            Ok(table_devices[token as usize].clone())
        };
        let scene = marks_scene::scene(buffers, Some(&image_of), gpu_wrapped.as_deref(), Some(&p.cache), Some(&device_of), &marks)?;
        if p.whole && table_devices.is_empty() {
            *p.kept.borrow_mut() = Some((size, scene.clone()));
        }
        p.borrows.set(!table_devices.is_empty());
        result = Some(scene);
        Ok(())
    })?;
    result.ok_or_else(|| v::fail("E_DRAW_KIND", "The drawing produced no scene."))
}

/// Runs the core draw path (`core_exec::draw`) into a mark table and adapts it with
/// `marks_scene`; a lowering error is the diagnostic. With a configured GPU, a region that feeds
/// only the positions, sizes, radius, from or to of a mark set stays on the GPU when its backend
/// places it there, and that mark set is drawn by the `Marks` family with the region's buffer bound
/// directly. Without a configured GPU, or off the backend, nothing differs from the CPU path.
pub fn render_prepared(p: &prepared, live: &Frame, size: (i64, i64), o: &RenderOptions) -> Result<scene::t, Diagnostic> {
    let state = o.state.clone().unwrap_or_else(e::create_state);
    match core(p) {
        Ok(core) => render_core(core, &state, p, live, size, o),
        Err(d) => Err(d.clone()),
    }
}
