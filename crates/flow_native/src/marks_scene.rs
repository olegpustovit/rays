//! the adapter from the mark table
//! (`scene_command::marks`, specification/marks.md) to `rays::scene`. It builds the same
//! `ui_batch` records, geometry and transforms `Drawing` builds today, so the renderer sees the
//! same commands.
use flow_gpu::small_lru::List;
use flow_ir::core_exec::Buffers;
use flow_ir::flow_ir::gpu as G;
use flow_syntax::value::{self as v, Value};
use rays::scene;
use rays_diagnostic::Diagnostic;
use scene_command::marks::{self as m, ColorAttr, Entry, Kind, PairAttr, ScalarAttr};
use scene_command::render_ir::{Geometry, Transform};
use scene_command::ui_batch::{self, Builder};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

/// One attribute of a `Marks`-family draw: float32 elements (or `0xRRGGBBAA` words for a colour) of
/// `stride` elements per instance, 0 for one constant, or a GPU region's output.
#[derive(Clone)]
pub enum Source {
    /// shared: a kept conversion is the same allocation every frame, which the renderer recognises
    /// without comparing or uploading it
    Bytes(Rc<Vec<u8>>),
    Device(G::Value),
}

#[derive(Clone)]
pub struct Attribute {
    pub source: Source,
    pub stride: i64,
}

/// Registers one mark set drawn by the `Marks` family: the `slot` numbers the frame's GPU sets from
/// 0, `kind` is the `Ui_batch` kind word (rect 0, circle 4, line 5) and the attributes are
/// position, extent, fill, stroke and width.
pub type Gpu = dyn Fn(i64, i64, i64, &[Attribute]) -> Result<m::GpuToken, Diagnostic>;

type Kept = List<(i64, i64), (Arc<Vec<f64>>, Rc<Vec<u8>>)>;

/// The nodes built for the mark sets of the last frame, by position in the table: at most 1024
/// nodes and 4096 converted attribute arrays are kept, the least recently used going first.
pub struct Cache {
    nodes: RefCell<List<i64, (Entry, Vec<Arc<Vec<f64>>>, scene::Node)>>,
    converted: RefCell<Kept>,
}

pub fn cache_new() -> Cache {
    Cache { nodes: RefCell::new(List::new(1024, Box::new(|_, _| {}))), converted: RefCell::new(List::new(4096, Box::new(|_, _| {}))) }
}

/// The nodes held.
pub fn cache_size(c: &Cache) -> i64 {
    c.nodes.borrow().length() as i64
}

fn fail(code: &str, message: &str) -> Diagnostic {
    v::fail(code, message)
}

fn color(c: i32) -> rays_math::color::T {
    let channel = |shift: u32| (((c as u32) >> shift) & 0xff) as i64;
    rays_math::color::rgba(channel(24), channel(16), channel(8), channel(0))
}

fn rgba(c: &rays_math::color::T) -> i32 {
    ((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as i32
}

fn no_device() -> Diagnostic {
    fail("E_GPU", "A device attribute needs a GPU host.")
}

fn data(buffers: &Buffers, b: &m::Buffer) -> Result<Arc<Vec<f64>>, Diagnostic> {
    buffers.find(b.id).ok_or_else(|| fail("E_DRAW_RANGE", "A mark buffer is not bound."))
}

fn at(d: &[f64], i: usize) -> Result<f64, Diagnostic> {
    d.get(i).copied().ok_or_else(|| fail("E_DRAW_RANGE", "index out of bounds"))
}

/// An attribute as a function of the instance index; a buffer is looked up once.
enum Pairs {
    Constant((f64, f64)),
    Buffer(Arc<Vec<f64>>, usize),
}
impl Pairs {
    fn get(&self, i: usize) -> Result<(f64, f64), Diagnostic> {
        match self {
            Pairs::Constant(p) => Ok(*p),
            Pairs::Buffer(d, w) => Ok((at(d, w * i)?, at(d, w * i + 1)?)),
        }
    }
}
fn pairs(buffers: &Buffers, a: &PairAttr) -> Result<Pairs, Diagnostic> {
    match a {
        PairAttr::Constant(p) => Ok(Pairs::Constant(*p)),
        PairAttr::Buffer(b) => Ok(Pairs::Buffer(data(buffers, b)?, b.width as usize)),
        PairAttr::Device(_) => Err(no_device()),
    }
}

enum Floats {
    Constant(f64),
    Buffer(Arc<Vec<f64>>),
}
impl Floats {
    fn get(&self, i: usize) -> Result<f64, Diagnostic> {
        match self {
            Floats::Constant(x) => Ok(*x),
            Floats::Buffer(d) => at(d, i),
        }
    }
}
fn floats(buffers: &Buffers, a: &ScalarAttr) -> Result<Floats, Diagnostic> {
    match a {
        ScalarAttr::Constant(x) => Ok(Floats::Constant(*x)),
        ScalarAttr::Buffer(b) => Ok(Floats::Buffer(data(buffers, b)?)),
        ScalarAttr::Device(_) => Err(no_device()),
    }
}

enum Colours {
    Constant(i32),
    Buffer(Arc<Vec<f64>>),
}
impl Colours {
    fn get(&self, i: usize) -> Result<i32, Diagnostic> {
        match self {
            Colours::Constant(c) => Ok(*c),
            Colours::Buffer(d) => {
                let c = flow_check::op::drawing_color(&Value::Vec3(at(d, 3 * i)?, at(d, 3 * i + 1)?, at(d, 3 * i + 2)?))?;
                Ok(rgba(&c))
            }
        }
    }
}
fn colours(buffers: &Buffers, a: &ColorAttr) -> Result<Colours, Diagnostic> {
    match a {
        ColorAttr::Constant(c) => Ok(Colours::Constant(*c)),
        ColorAttr::Buffer(b) => Ok(Colours::Buffer(data(buffers, b)?)),
        ColorAttr::Device(_) => Err(no_device()),
    }
}

/// A host float becomes binary32: a nonfinite or over-large one is refused, as `Builder` does.
fn float_bytes(n: usize, f: &dyn Fn(usize) -> f64) -> Result<Vec<u8>, Diagnostic> {
    let mut b = Vec::with_capacity(4 * n);
    for i in 0..n {
        let x = f(i);
        if !x.is_finite() || x.abs() > 3.4028234663852886e38 {
            return Err(fail("E_DRAW_RANGE", "A drawing coordinate must be finite and fit a 32-bit float."));
        }
        b.extend_from_slice(&(x as f32).to_bits().to_le_bytes());
    }
    Ok(b)
}

fn word_bytes(n: usize, f: &dyn Fn(usize) -> i32) -> Vec<u8> {
    let mut b = Vec::with_capacity(4 * n);
    for i in 0..n {
        b.extend_from_slice(&f(i).to_le_bytes());
    }
    b
}

fn host(bytes: Vec<u8>, stride: i64) -> Attribute {
    shared(Rc::new(bytes), stride)
}

fn shared(bytes: Rc<Vec<u8>>, stride: i64) -> Attribute {
    Attribute { source: Source::Bytes(bytes), stride }
}

/// The bytes of a host array are kept while the array is physically the same: a static attribute
/// of a device set is converted once, and the unchanged bytes are not uploaded again. Keyed by the
/// set's slot and the attribute's place in it.
fn kept(converted: Option<&Cache>, slot: i64, place: i64, d: &Arc<Vec<f64>>, build: &dyn Fn() -> Result<Vec<u8>, Diagnostic>) -> Result<Rc<Vec<u8>>, Diagnostic> {
    match converted {
        None => build().map(Rc::new),
        Some(c) => {
            let key = (slot, place);
            if let Some((old, bytes)) = c.converted.borrow_mut().find(&|k, _| *k == key) {
                if Arc::ptr_eq(old, d) {
                    return Ok(bytes.clone());
                }
            }
            let bytes = Rc::new(build()?);
            c.converted.borrow_mut().remove(&|k, _| *k == key);
            c.converted.borrow_mut().add(key, (d.clone(), bytes.clone()));
            Ok(bytes)
        }
    }
}

type Device<'a> = &'a dyn Fn(i64) -> Result<G::Value, Diagnostic>;

/// The marks of a packed position array along a Z-order curve over its bounds: marks that are close
/// on the canvas become close in the draw. A tile-based GPU bins a million scattered small quads in
/// about 12 ms and the same quads in a coherent order in about 5 (M1, 2026-11-13); an affine
/// transform keeps the order coherent.
fn z_order(d: &[f64], width: usize, count: usize) -> Vec<u32> {
    let finite = |v: f64| if v.is_finite() { v } else { 0. };
    let (mut lo, mut hi) = ((f64::INFINITY, f64::INFINITY), (f64::NEG_INFINITY, f64::NEG_INFINITY));
    for i in 0..count {
        let (x, y) = (finite(d[i * width]), finite(d[i * width + 1]));
        lo = (lo.0.min(x), lo.1.min(y));
        hi = (hi.0.max(x), hi.1.max(y));
    }
    let scale = |span: f64| if span > 0. { 65535. / span } else { 0. };
    let (sx, sy) = (scale(hi.0 - lo.0), scale(hi.1 - lo.1));
    let spread = |v: u32| {
        let v = (v | (v << 8)) & 0x00ff00ff;
        let v = (v | (v << 4)) & 0x0f0f0f0f;
        let v = (v | (v << 2)) & 0x33333333;
        (v | (v << 1)) & 0x55555555
    };
    let mut keyed: Vec<u64> = (0..count)
        .map(|i| {
            let (x, y) = (((finite(d[i * width]) - lo.0) * sx) as u32, ((finite(d[i * width + 1]) - lo.1) * sy) as u32);
            (((spread(x) | (spread(y) << 1)) as u64) << 32) | i as u64
        })
        .collect();
    keyed.sort_unstable();
    keyed.into_iter().map(|k| k as u32).collect()
}

/// The positions of a host set whose every other attribute is one constant: its marks are one
/// colour and one size, so the picture does not depend on their order, and they are drawn in
/// [`z_order`]. The order is computed with the bytes, once while the array stays the same.
fn ordered(buffers: &Buffers, c: Option<&Cache>, slot: i64, count: i64, b: &m::Buffer) -> Result<Attribute, Diagnostic> {
    let d = data(buffers, b)?;
    let (width, n) = (b.width as usize, count as usize);
    if d.len() < width * n {
        return Err(fail("E_DRAW_LENGTH", "A position array is shorter than its mark count."));
    }
    let bytes = kept(c, slot, 0, &d, &|| {
        let order = z_order(&d, width, n);
        float_bytes(width * n, &|i| d[order[i / width] as usize * width + i % width])
    })?;
    Ok(shared(bytes, b.width))
}

/// Floats of `width` per instance: constant pairs and scalars take stride 0.
fn vectors(buffers: &Buffers, device: Device, c: Option<&Cache>, slot: i64, place: i64, count: i64, a: &PairAttr) -> Result<Attribute, Diagnostic> {
    match a {
        PairAttr::Constant((x, y)) => Ok(host(float_bytes(3, &|i| match i { 0 => *x, 1 => *y, _ => 0. })?, 0)),
        PairAttr::Buffer(b) => {
            let d = data(buffers, b)?;
            let n = (b.width * count) as usize;
            let bytes = kept(c, slot, place, &d, &|| float_bytes(n, &|i| d.get(i).copied().unwrap_or(f64::NAN)))?;
            Ok(shared(bytes, b.width))
        }
        PairAttr::Device(b) => Ok(Attribute { source: Source::Device(device(b.id)?), stride: b.width }),
    }
}

fn scalars(buffers: &Buffers, device: Device, c: Option<&Cache>, slot: i64, place: i64, count: i64, a: &ScalarAttr) -> Result<Attribute, Diagnostic> {
    match a {
        ScalarAttr::Constant(x) => Ok(host(float_bytes(1, &|_| *x)?, 0)),
        ScalarAttr::Buffer(b) => {
            let d = data(buffers, b)?;
            let bytes = kept(c, slot, place, &d, &|| float_bytes(count as usize, &|i| d.get(i).copied().unwrap_or(f64::NAN)))?;
            Ok(shared(bytes, 1))
        }
        ScalarAttr::Device(b) => Ok(Attribute { source: Source::Device(device(b.id)?), stride: 1 }),
    }
}

/// A device colour is float r, g, b (the shader's mode 1); a host one is packed words (mode 0).
fn words(buffers: &Buffers, device: Device, c: Option<&Cache>, slot: i64, place: i64, count: i64, a: &ColorAttr) -> Result<Attribute, Diagnostic> {
    match a {
        ColorAttr::Constant(x) => Ok(host(word_bytes(1, &|_| *x), 0)),
        ColorAttr::Buffer(b) => {
            let d = data(buffers, b)?;
            let colours = colours(buffers, a)?;
            let bytes = kept(c, slot, place, &d, &|| {
                let mut out = Vec::with_capacity(4 * count as usize);
                for i in 0..count as usize {
                    out.extend_from_slice(&colours.get(i)?.to_le_bytes());
                }
                Ok(out)
            })?;
            Ok(shared(bytes, 1))
        }
        ColorAttr::Device(b) => Ok(Attribute { source: Source::Device(device(b.id)?), stride: b.width }),
    }
}

/// Rect, circle and line sets with a device attribute: position, extent, fill, stroke, width.
fn gpu_set(buffers: &Buffers, device: Device, gpu_: &Gpu, c: Option<&Cache>, slot: i64, count: i64, kind: &Kind) -> Result<scene::Node, Diagnostic> {
    let (kind_word, attributes) = match kind {
        Kind::Rect { position: PairAttr::Buffer(b), size: PairAttr::Constant(size), fill: fill @ ColorAttr::Constant(_), stroke: stroke @ ColorAttr::Constant(_), .. } => (
            0,
            vec![
                ordered(buffers, c, slot, count, b)?,
                vectors(buffers, device, c, slot, 1, count, &PairAttr::Constant(*size))?,
                words(buffers, device, c, slot, 2, count, fill)?,
                words(buffers, device, c, slot, 3, count, stroke)?,
                host(float_bytes(1, &|_| 1.)?, 0),
            ],
        ),
        Kind::Circle { position: PairAttr::Buffer(b), radius: radius @ ScalarAttr::Constant(_), fill: fill @ ColorAttr::Constant(_), stroke: stroke @ ColorAttr::Constant(_), stroke_width: stroke_width @ ScalarAttr::Constant(_) } => (
            4,
            vec![
                ordered(buffers, c, slot, count, b)?,
                scalars(buffers, device, c, slot, 1, count, radius)?,
                words(buffers, device, c, slot, 2, count, fill)?,
                words(buffers, device, c, slot, 3, count, stroke)?,
                scalars(buffers, device, c, slot, 4, count, stroke_width)?,
            ],
        ),
        Kind::Point { position: PairAttr::Buffer(b), fill } => (
            0,
            vec![
                ordered(buffers, c, slot, count, b)?,
                host(float_bytes(2, &|_| 1.)?, 0),
                host(word_bytes(1, &|_| *fill), 0),
                host(word_bytes(1, &|_| 0), 0),
                host(float_bytes(1, &|_| 1.)?, 0),
            ],
        ),
        Kind::Rect { position, size, fill, stroke, .. } => (
            0,
            vec![
                vectors(buffers, device, c, slot, 0, count, position)?,
                vectors(buffers, device, c, slot, 1, count, size)?,
                words(buffers, device, c, slot, 2, count, fill)?,
                words(buffers, device, c, slot, 3, count, stroke)?,
                host(float_bytes(1, &|_| 1.)?, 0),
            ],
        ),
        Kind::Circle { position, radius, fill, stroke, stroke_width } => (
            4,
            vec![
                vectors(buffers, device, c, slot, 0, count, position)?,
                scalars(buffers, device, c, slot, 1, count, radius)?,
                words(buffers, device, c, slot, 2, count, fill)?,
                words(buffers, device, c, slot, 3, count, stroke)?,
                scalars(buffers, device, c, slot, 4, count, stroke_width)?,
            ],
        ),
        Kind::Line { position, end, stroke, stroke_width } => (
            5,
            vec![
                vectors(buffers, device, c, slot, 0, count, position)?,
                vectors(buffers, device, c, slot, 1, count, end)?,
                host(word_bytes(1, &|_| 0), 0),
                words(buffers, device, c, slot, 3, count, stroke)?,
                scalars(buffers, device, c, slot, 4, count, stroke_width)?,
            ],
        ),
        // a point is the one-point rect the host route draws (`points`)
        Kind::Point { position, fill } => (
            0,
            vec![
                vectors(buffers, device, c, slot, 0, count, position)?,
                host(float_bytes(2, &|_| 1.)?, 0),
                host(word_bytes(1, &|_| *fill), 0),
                host(word_bytes(1, &|_| 0), 0),
                host(float_bytes(1, &|_| 1.)?, 0),
            ],
        ),
    };
    let token = gpu_(slot, kind_word, count, &attributes)?;
    Ok(scene::shapes(ui_batch::of_marks(token)))
}

fn has_device(kind: &Kind) -> bool {
    let p = |a: &PairAttr| matches!(a, PairAttr::Device(_));
    let s = |a: &ScalarAttr| matches!(a, ScalarAttr::Device(_));
    let c = |a: &ColorAttr| matches!(a, ColorAttr::Device(_));
    match kind {
        Kind::Rect { position, size, fill, stroke, .. } => p(position) || p(size) || c(fill) || c(stroke),
        Kind::Circle { position, radius, fill, stroke, stroke_width } => p(position) || s(radius) || s(stroke_width) || c(fill) || c(stroke),
        Kind::Line { position, end, stroke, stroke_width } => p(position) || p(end) || s(stroke_width) || c(stroke),
        Kind::Point { position, .. } => p(position),
    }
}

/// A host-only set this large goes to the `Marks` family too when there is a GPU: its attributes are
/// 12 to 20 bytes a mark uploaded once, where a `Ui_batch` is 64 bytes a mark compared and hashed
/// every frame (a million static rects on an M1: 5.3 ms of CPU and 21 ms of GPU a frame as a batch).
const GPU_SET_MIN: i64 = 4096;
/// The frame's sets share the host's 64 sources (`Workspace_gpu.mark_sets`); device sets come first.
const GPU_SET_SLOTS: i64 = 32;

fn on_gpu(gpu: bool, slot: i64, count: i64, kind: &Kind) -> bool {
    has_device(kind) || (gpu && count >= GPU_SET_MIN && slot < GPU_SET_SLOTS)
}

fn int_coordinate(x: f64) -> Result<i64, Diagnostic> {
    if !x.is_finite() || x > i64::MAX as f64 || x < i64::MIN as f64 {
        Err(fail("E_DRAW_RANGE", "A drawing coordinate must fit an integer."))
    } else {
        Ok(x as i64)
    }
}

fn range(r: Result<(), String>) -> Result<(), Diagnostic> {
    r.map_err(|message| fail("E_DRAW_RANGE", &message))
}

/// An attribute as the builder's packed floats: a constant is its one element with stride 0.
fn packed2(buffers: &Buffers, a: &PairAttr) -> Result<(Arc<Vec<f64>>, i64), Diagnostic> {
    match a {
        PairAttr::Constant((x, y)) => Ok((Arc::new(vec![*x, *y]), 0)),
        PairAttr::Buffer(b) => Ok((data(buffers, b)?, b.width)),
        PairAttr::Device(_) => Err(no_device()),
    }
}

fn packed1(buffers: &Buffers, a: &ScalarAttr) -> Result<(Arc<Vec<f64>>, i64), Diagnostic> {
    match a {
        ScalarAttr::Constant(x) => Ok((Arc::new(vec![*x]), 0)),
        ScalarAttr::Buffer(b) => Ok((data(buffers, b)?, 1)),
        ScalarAttr::Device(_) => Err(no_device()),
    }
}

/// A set with constant colours is written by the builder from the arrays themselves, with no
/// allocation per mark; a per-instance colour goes mark by mark through `drawing_color`.
fn shapes(buffers: &Buffers, count: i64, kind: &Kind) -> Result<scene::Node, Diagnostic> {
    let mut b = Builder::with_capacity(count);
    match kind {
        Kind::Rect { position, size, fill: ColorAttr::Constant(fill), stroke: ColorAttr::Constant(stroke), .. } => {
            let (p, ps) = packed2(buffers, position)?;
            let (s, ss) = packed2(buffers, size)?;
            range(b.rects(count, &p, ps, &s, ss, *fill, *stroke))?;
        }
        Kind::Circle { position, radius, fill: ColorAttr::Constant(fill), stroke: ColorAttr::Constant(stroke), stroke_width } => {
            let (p, ps) = packed2(buffers, position)?;
            let (r, rs) = packed1(buffers, radius)?;
            let (w, ws) = packed1(buffers, stroke_width)?;
            range(b.circles(count, &p, ps, &r, rs, *fill, *stroke, &w, ws))?;
        }
        Kind::Line { position, end, stroke: ColorAttr::Constant(color), stroke_width } => {
            let (p, ps) = packed2(buffers, position)?;
            let (e, es) = packed2(buffers, end)?;
            let (w, ws) = packed1(buffers, stroke_width)?;
            range(b.lines(count, &p, ps, &e, es, *color, &w, ws))?;
        }
        Kind::Rect { position, size, fill, stroke, .. } => {
            let (position, size, fill, stroke) = (pairs(buffers, position)?, pairs(buffers, size)?, colours(buffers, fill)?, colours(buffers, stroke)?);
            for i in 0..count as usize {
                let (x, y) = position.get(i)?;
                let (width, height) = size.get(i)?;
                range(b.shape_rect(x, y, width, height, fill.get(i)?, stroke.get(i)?))?;
            }
        }
        Kind::Circle { position, radius, fill, stroke, stroke_width } => {
            let (position, radius, fill, stroke, stroke_width) =
                (pairs(buffers, position)?, floats(buffers, radius)?, colours(buffers, fill)?, colours(buffers, stroke)?, floats(buffers, stroke_width)?);
            for i in 0..count as usize {
                let (x, y) = position.get(i)?;
                range(b.circle(x, y, radius.get(i)?, Some(fill.get(i)?), Some(stroke.get(i)?), stroke_width.get(i)?))?;
            }
        }
        Kind::Line { position, end, stroke, stroke_width } => {
            let (position, end, color, width) = (pairs(buffers, position)?, pairs(buffers, end)?, colours(buffers, stroke)?, floats(buffers, stroke_width)?);
            for i in 0..count as usize {
                let (x0, y0) = position.get(i)?;
                let (x1, y1) = end.get(i)?;
                range(b.line(x0, y0, x1, y1, color.get(i)?, width.get(i)?))?;
            }
        }
        Kind::Point { .. } => unreachable!("assert false"),
    }
    Ok(scene::shapes(b.publish().map_err(|message| fail("E_DRAW_RANGE", &message))?))
}

/// Point instances retain their logical subpixel positions in `Ui_batch`.
fn points(buffers: &Buffers, count: i64, position: &PairAttr, fill: i32) -> Result<Option<scene::Node>, Diagnostic> {
    let position = pairs(buffers, position)?;
    let ink = rgba(&color(fill));
    let mut batch = Builder::with_capacity(count);
    for i in 0..count as usize {
        let (x, y) = position.get(i)?;
        range(batch.shape_rect(x, y, 1., 1., ink, 0))?;
    }
    if count == 0 {
        return Ok(None);
    }
    Ok(Some(scene::shapes(batch.publish().map_err(|message| fail("E_DRAW_RANGE", &message))?)))
}

/// The buffers a kind reads, in a fixed order (the cache compares them by identity).
fn arrays(buffers: &Buffers, kind: &Kind) -> Result<Vec<Arc<Vec<f64>>>, Diagnostic> {
    let mut out: Vec<Arc<Vec<f64>>> = Vec::new();
    let mut add = |b: Option<&m::Buffer>| -> Result<(), Diagnostic> {
        if let Some(b) = b { out.push(data(buffers, b)?); }
        Ok(())
    };
    let p = |a: &PairAttr| if let PairAttr::Buffer(b) = a { Some(*b) } else { None };
    let s = |a: &ScalarAttr| if let ScalarAttr::Buffer(b) = a { Some(*b) } else { None };
    let c = |a: &ColorAttr| if let ColorAttr::Buffer(b) = a { Some(*b) } else { None };
    match kind {
        Kind::Rect { position, size, fill, stroke, stroke_width } => {
            add(p(position).as_ref())?; add(p(size).as_ref())?; add(c(fill).as_ref())?; add(c(stroke).as_ref())?; add(s(stroke_width).as_ref())?;
        }
        Kind::Circle { position, radius, fill, stroke, stroke_width } => {
            add(p(position).as_ref())?; add(s(radius).as_ref())?; add(c(fill).as_ref())?; add(c(stroke).as_ref())?; add(s(stroke_width).as_ref())?;
        }
        Kind::Line { position, end, stroke, stroke_width } => {
            add(p(position).as_ref())?; add(p(end).as_ref())?; add(c(stroke).as_ref())?; add(s(stroke_width).as_ref())?;
        }
        Kind::Point { position, .. } => add(p(position).as_ref())?,
    }
    Ok(out)
}

/// The scene node of a mark set, kept while its description and the physical attribute arrays are
/// the same as the previous frame's: a batch whose data did not change is not built again.
fn cached(c: Option<&Cache>, buffers: &Buffers, ordinal: i64, entry: &Entry, kind: &Kind, build: &dyn Fn() -> Result<Option<scene::Node>, Diagnostic>) -> Result<Option<scene::Node>, Diagnostic> {
    let Some(c) = c else { return build() };
    let data = arrays(buffers, kind)?;
    if let Some((previous, old, node)) = c.nodes.borrow_mut().find(&|k, _| *k == ordinal) {
        if previous == entry && old.len() == data.len() && old.iter().zip(data.iter()).all(|(a, b)| Arc::ptr_eq(a, b)) {
            return Ok(Some(node.clone()));
        }
    }
    let node = build()?;
    if let Some(node) = &node {
        c.nodes.borrow_mut().remove(&|k, _| *k == ordinal);
        c.nodes.borrow_mut().add(ordinal, (entry.clone(), data, node.clone()));
    }
    Ok(node)
}

enum Close {
    Transform(Transform),
    Clip(i64, i64, i64, i64),
}

/// `buffers` is the table the marks' buffer ids refer to; `image` maps an `Image` entry's resource
/// id to the host image (`E_IMAGE` when absent). A buffer value or constant a builder rejects is
/// `E_DRAW_RANGE`; a nonfinite colour channel `E_NONFINITE`.
///
/// A mark set with a `Device` attribute (its buffer id resolved by `device`) goes to `gpu` and
/// becomes one `Marks`-family draw; its CPU attributes are uploaded in the same layout. Every other
/// mark set stays a `Ui_batch`, so a CPU-only frame is unchanged. Without `gpu` a `Device`
/// attribute is `E_GPU`.
pub fn scene(
    buffers: &Buffers,
    image: Option<&dyn Fn(i64) -> rays::image::Image>,
    gpu_: Option<&Gpu>,
    cache_: Option<&Cache>,
    device: Option<Device>,
    marks: &[Entry],
) -> Result<scene::Scene, Diagnostic> {
    if let Err(message) = m::validate(marks) {
        return Err(fail("E_DRAW_RANGE", &message));
    }
    let no_dev = |_: i64| -> Result<G::Value, Diagnostic> { Err(no_device()) };
    let device: Device = device.unwrap_or(&no_dev);
    // Open pushes, innermost first, each with the nodes drawn inside it.
    let mut stack: Vec<(Close, Vec<scene::Node>)> = Vec::new();
    let mut top: Vec<scene::Node> = Vec::new();
    let (mut slot, mut sets) = (0i64, 0i64);
    for entry in marks {
        match entry {
            Entry::MarkSet { count, kind: kind @ Kind::Point { position, fill } } if !on_gpu(gpu_.is_some(), slot, *count, kind) => {
                sets += 1;
                // A point set with no points has no node; it is never kept.
                if let Some(node) = cached(cache_, buffers, sets, entry, kind, &|| points(buffers, *count, position, *fill))? {
                    top.push(node);
                }
            }
            Entry::MarkSet { count, kind } if on_gpu(gpu_.is_some(), slot, *count, kind) => {
                let Some(gpu_) = gpu_ else { return Err(no_device()) };
                top.push(gpu_set(buffers, device, gpu_, cache_, slot, *count, kind)?);
                slot += 1;
            }
            Entry::MarkSet { count, kind } => {
                sets += 1;
                if let Some(node) = cached(cache_, buffers, sets, entry, kind, &|| shapes(buffers, *count, kind).map(Some))? {
                    top.push(node);
                }
            }
            Entry::Geometry { vertices, indices, color } => {
                top.push(scene::geometry(Geometry { vertices: vertices.clone(), indices: indices.clone(), color: *color }));
            }
            Entry::Image { resource, x, y, scale, angle } => match image {
                Some(resolve) => top.push(scene::image(&resolve(*resource), (*x, *y), Some(*scale), Some(*angle)).map_err(|message| fail("E_DRAW_RANGE", &message))?),
                None => return Err(fail("E_IMAGE", "Drawing images need an environment-owned image resolver.")),
            },
            Entry::Text { x, y, size, color: c, text } => {
                top.push(scene::text((*x, *y), Some(color(*c)), Some(*size), &String::from_utf8_lossy(text)));
            }
            Entry::PushTransform { xx, xy, yx, yy, tx, ty } => {
                stack.push((Close::Transform(Transform { xx: *xx, xy: *xy, yx: *yx, yy: *yy, tx: *tx, ty: *ty }), std::mem::take(&mut top)));
            }
            Entry::PushClip { x, y, width, height } => {
                let (x, y, w, h) = (int_coordinate(*x)?, int_coordinate(*y)?, int_coordinate(*width)?, int_coordinate(*height)?);
                stack.push((Close::Clip(x, y, w, h), std::mem::take(&mut top)));
            }
            Entry::PopTransform | Entry::PopClip => {
                let (close, outer) = stack.pop().expect("assert false");
                let inner = std::mem::replace(&mut top, outer);
                top.push(match close {
                    Close::Transform(t) => scene::transform(t, inner),
                    Close::Clip(x, y, w, h) => scene::clip((x, y), w, h, inner),
                });
            }
        }
    }
    Ok(top)
}

#[cfg(test)]
mod tests {
    use super::z_order;

    /// The order is a permutation, walks the four quadrants as a Z and keeps a quadrant's marks together.
    #[test]
    fn z_order_is_a_permutation_that_keeps_neighbours_together() {
        // two marks in each quadrant of a 100 by 100 square, interleaved in the array; width 3 (z ignored)
        let corners = [(90., 90.), (10., 10.), (90., 10.), (10., 90.), (95., 95.), (5., 5.), (95., 5.), (5., 95.)];
        let d: Vec<f64> = corners.iter().flat_map(|(x, y)| [*x, *y, 7.]).collect();
        let order = z_order(&d, 3, corners.len());
        let mut seen = order.clone();
        seen.sort_unstable();
        assert_eq!(seen, (0..8).collect::<Vec<u32>>());
        let quadrant = |i: u32| (corners[i as usize].0 > 50.) as u32 + 2 * (corners[i as usize].1 > 50.) as u32;
        assert_eq!(order.iter().map(|i| quadrant(*i)).collect::<Vec<u32>>(), [0, 0, 1, 1, 2, 2, 3, 3]);
        // one point, equal points and a nonfinite coordinate do not divide by a zero span or panic
        assert_eq!(z_order(&[1., 2.], 2, 1), [0]);
        assert_eq!(z_order(&[1., 2., 1., 2.], 2, 2), [0, 1]);
        assert_eq!(z_order(&[f64::NAN, 0., 3., f64::INFINITY], 2, 2).len(), 2);
    }
}
