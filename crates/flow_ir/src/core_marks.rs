//! Execute draw statements into the mark table. Buffer lookup misses return None; tessellation failures become E_DRAW_RANGE diagnostics.
use flow_syntax::symbol::Symbol;
use crate::core_ir::{Draw, Mark, PathKind, Transform};
use flow_check::op::drawing_color;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as v, Value};
use flow_syntax::Diagnostic;
use scene_command::marks::{self as m, Buffer, ColorAttr, Entry, Kind, PairAttr, ScalarAttr};
use scene_command::shape2;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Debug, Default)]
struct BuffersInner {
    next: i64,
    table: HashMap<i64, Arc<Vec<f64>>>,
    devices: HashMap<i64, (i64, i64, i64)>,
}

/// The per-frame table of CPU attribute arrays that mark sets refer to by id. The caller creates
/// it, reads from it while it adapts the marks, and clears it each frame. A clone shares the table.
#[derive(Clone, Debug)]
pub struct Buffers {
    inner: Arc<Mutex<BuffersInner>>,
}

impl Buffers {
    pub fn create() -> Buffers {
        Buffers { inner: Arc::new(Mutex::new(BuffersInner { next: 1, ..Default::default() })) }
    }
    pub fn clear(&self) {
        let mut b = self.inner.lock().unwrap();
        b.next = 1;
        b.table.clear();
        b.devices.clear();
    }
    /// The data of a buffer id of the current frame.
    pub fn find(&self, id: i64) -> Option<Arc<Vec<f64>>> {
        self.inner.lock().unwrap().table.get(&id).cloned()
    }
    fn add(&self, data: Arc<Vec<f64>>) -> i64 {
        let mut b = self.inner.lock().unwrap();
        let id = b.next;
        b.next = id + 1;
        b.table.insert(id, data);
        id
    }
    fn add_device(&self, token: i64, count: i64, width: i64) -> i64 {
        let mut b = self.inner.lock().unwrap();
        let id = b.next;
        b.next = id + 1;
        b.devices.insert(id, (token, count, width));
        id
    }
    /// A `Device` buffer id of the current frame: the host's token, the element count and the width.
    pub fn device(&self, id: i64) -> Option<(i64, i64, i64)> {
        self.inner.lock().unwrap().devices.get(&id).copied()
    }
}

pub type ImageResolver = Arc<dyn Fn(&Value) -> Result<i64, Diagnostic> + Send + Sync>;

pub struct Sink {
    buffers: Buffers,
    size: (i64, i64),
    image: Option<ImageResolver>,
    entries: Vec<Entry>,
}

/// `size` is the canvas (for `draw/background`); `image` resolves an image node to a host resource
/// id (positive) or fails.
pub fn sink(image: Option<ImageResolver>, size: (i64, i64), buffers: &Buffers) -> Sink {
    Sink { buffers: buffers.clone(), size, image, entries: Vec::new() }
}

fn push(sink: &mut Sink, entry: Entry) -> Result<(), Diagnostic> {
    if sink.entries.len() as i64 >= m::MAX_ENTRIES {
        return Err(v::fail("E_DRAW_RANGE", "A drawing has too many entries."));
    }
    sink.entries.push(entry);
    Ok(())
}

/// The entries in painter order, checked by `Marks::validate` (`E_DRAW_RANGE` otherwise).
pub fn finish(sink: &Sink) -> Result<Vec<Entry>, Diagnostic> {
    match m::validate(&sink.entries) {
        Ok(()) => Ok(sink.entries.clone()),
        Err(message) => Err(v::fail("E_DRAW_RANGE", &message)),
    }
}

fn color(value: &Value) -> Result<i32, Diagnostic> {
    let c = drawing_color(value)?;
    Ok(rgba(c.r, c.g, c.b, c.a))
}
fn rgba(r: i64, g: i64, b: i64, a: i64) -> i32 {
    ((r << 24) | (g << 16) | (b << 8) | a) as i32
}
const WHITE: i32 = -1;
fn ink(value: &Value) -> Result<i32, Diagnostic> {
    match value {
        Value::Floats(3, _) => Ok(WHITE),
        other => color(other),
    }
}
fn valid_coordinate(x: f64) -> bool {
    x.is_finite() && x < 4.611686018427388e18 && x >= -4.611686018427388e18
}

fn float_point(value: &Value) -> Result<(f64, f64), Diagnostic> {
    let (x, y, _) = v::comps(value)?;
    if !(x.is_finite() && y.is_finite()) || x.abs() > 3.4028234663852886e38 || y.abs() > 3.4028234663852886e38 {
        return Err(v::fail("E_DRAW_RANGE", "A drawing coordinate must be finite and fit a 32-bit float."));
    }
    Ok((x, y))
}

fn point(value: &Value) -> Result<(i64, i64), Diagnostic> {
    let (x, y, _) = v::comps(value)?;
    let int = |x: f64| {
        if !valid_coordinate(x) {
            Err(v::fail("E_DRAW_RANGE", "A drawing coordinate must fit an integer."))
        } else {
            Ok(x as i64)
        }
    };
    let x = int(x)?;
    let y = int(y)?;
    Ok((x, y))
}

fn pair(value: &Value) -> Result<(f64, f64), Diagnostic> {
    let (x, y, _) = v::comps(value)?;
    Ok((x, y))
}

fn geometry(sink: &mut Sink, geometries: Result<Vec<scene_command::render_ir::Geometry>, String>) -> Result<(), Diagnostic> {
    let geometries = geometries.map_err(|message| v::fail("E_DRAW_RANGE", &message))?;
    for g in geometries {
        push(sink, Entry::Geometry { vertices: g.vertices, indices: g.indices, color: g.color })?;
    }
    Ok(())
}

/// A region the host ran on the GPU stands in the locals as a placeholder naming its buffer id.
fn device_type() -> Ty {
    Ty::Named("device_array".to_owned())
}

/// The placeholder for a region the host ran on the GPU. `token` is the host's own handle, returned
/// by `Buffers::device`.
pub fn device_value(sink: &Sink, token: i64, count: i64, width: i64) -> Value {
    Value::Deferred(device_type(), sink.buffers.add_device(token, count, width))
}

/// A `device_value`: it names an entry of one draw's buffers.
pub fn is_device(value: &Value) -> bool {
    matches!(value, Value::Deferred(ty, _) if *ty == device_type())
}

fn device(sink: &Sink, value: &Value) -> Option<(i64, i64, i64)> {
    match value {
        Value::Deferred(ty, id) if *ty == device_type() => sink.buffers.device(*id).map(|(_, count, width)| (*id, count, width)),
        _ => None,
    }
}

fn colors_length() -> Diagnostic {
    v::fail("E_DRAW_LENGTH", "Colors and positions have different lengths.")
}

/// A packed colour array is per instance, on the host or left on the GPU; anything else one constant.
fn colour_attr(sink: &Sink, count: i64, value: &Value) -> Result<ColorAttr, Diagnostic> {
    if let Some((id, n, width)) = device(sink, value) {
        if width != 3 {
            return Err(v::fail("E_TYPE", "A color is hex text, vec3 or (list r g b a)."));
        }
        if n != count {
            return Err(colors_length());
        }
        return Ok(ColorAttr::Device(Buffer { id, count, width }));
    }
    match value {
        Value::Floats(3, xs) => {
            if xs.len() as i64 != 3 * count {
                return Err(colors_length());
            }
            Ok(ColorAttr::Buffer(Buffer { id: sink.buffers.add(xs.clone()), count, width: 3 }))
        }
        other => Ok(ColorAttr::Constant(color(other)?)),
    }
}

fn assoc<'a>(args: &'a [(Symbol, &'a Value)], key: &str) -> Option<&'a Value> {
    args.iter().find(|(k, _)| k == key).map(|(_, v)| *v)
}

fn check_colors(count: i64, args: &[(Symbol, &Value)]) -> Result<(), Diagnostic> {
    for key in ["fill", "stroke", "color"] {
        if let Some(Value::Floats(3, xs)) = assoc(args, key) {
            if xs.len() as i64 != 3 * count {
                return Err(colors_length());
            }
        }
    }
    Ok(())
}

/// Packed positions keep their storage; any other array is read into pairs.
fn points_attr(sink: &mut Sink, value: &Value) -> Result<(i64, PairAttr), Diagnostic> {
    if let Some((id, count, width)) = device(sink, value) {
        if width != 2 && width != 3 {
            return Err(v::fail("E_TYPE", "Positions are packed vec2 or vec3 arrays."));
        }
        return Ok((count, PairAttr::Device(Buffer { id, count, width })));
    }
    match value {
        Value::Floats(3, xs) => {
            let n = xs.len() as i64 / 3;
            Ok((n, PairAttr::Buffer(Buffer { id: sink.buffers.add(xs.clone()), count: n, width: 3 })))
        }
        Value::Floats(2, xs) => {
            let n = xs.len() as i64 / 2;
            Ok((n, PairAttr::Buffer(Buffer { id: sink.buffers.add(xs.clone()), count: n, width: 2 })))
        }
        _ => {
            let n = v::array_length(value)?;
            let mut data = vec![0.0; 2 * n];
            for i in 0..n {
                let (x, y) = pair(&v::array_get(value, i as i64)?)?;
                data[2 * i] = x;
                data[2 * i + 1] = y;
            }
            Ok((n as i64, PairAttr::Buffer(Buffer { id: sink.buffers.add(Arc::new(data)), count: n as i64, width: 2 })))
        }
    }
}

fn resolve_args<'a>(args: &[(Symbol, i64)], values: &'a [Value]) -> Vec<(Symbol, &'a Value)> {
    args.iter().map(|(k, i)| (k.clone(), &values[*i as usize])).collect()
}

fn length_error(message: &str) -> Diagnostic {
    v::fail("E_DRAW_LENGTH", message)
}

fn mark(sink: &mut Sink, batch: bool, kind: Mark, raw: &[(Symbol, i64)], values: &[Value]) -> Result<(), Diagnostic> {
    let args = resolve_args(raw, values);
    let arg = |key: &str| -> Result<&Value, Diagnostic> {
        assoc(&args, key).ok_or_else(|| v::failf("E_DRAW_INPUT", &format!("draw needs {}.", key)))
    };
    let style = |sink: &Sink, count: i64| -> Result<(ColorAttr, ColorAttr), Diagnostic> {
        let get = |key: &str| match assoc(&args, key) {
            Some(value) => colour_attr(sink, count, value),
            None => Ok(ColorAttr::Constant(0)),
        };
        let fill = get("fill")?;
        let stroke = get("stroke")?;
        Ok((fill, stroke))
    };
    let n_of_positions = |sink: &mut Sink, key: &str| -> Result<(i64, PairAttr), Diagnostic> {
        if batch {
            points_attr(sink, arg(key)?)
        } else {
            Ok((1, PairAttr::Constant(pair(arg(key)?)?)))
        }
    };
    match kind {
        Mark::Circle => {
            let (n, position) = n_of_positions(sink, if batch { "positions" } else { "at" })?;
            let radius = if !batch {
                ScalarAttr::Constant(v::num(arg("radius")?)?)
            } else {
                let value = arg("radius")?;
                if let Some((id, count, width)) = device(sink, value) {
                    if width != 1 {
                        return Err(v::fail("E_TYPE", "Radius is a number or packed float array."));
                    }
                    if count != n {
                        return Err(length_error("Radii and positions have different lengths."));
                    }
                    ScalarAttr::Device(Buffer { id, count, width })
                } else {
                    match value {
                        Value::Floats(1, xs) => {
                            if xs.len() as i64 != n {
                                return Err(length_error("Radii and positions have different lengths."));
                            }
                            ScalarAttr::Buffer(Buffer { id: sink.buffers.add(xs.clone()), count: n, width: 1 })
                        }
                        Value::IntArray(xs) => {
                            if xs.len() as i64 != n {
                                return Err(length_error("Radii and positions have different lengths."));
                            }
                            let data: Vec<f64> = xs.iter().map(|x| *x as f64).collect();
                            ScalarAttr::Buffer(Buffer { id: sink.buffers.add(Arc::new(data)), count: n, width: 1 })
                        }
                        Value::Int(_) | Value::Float(_) => ScalarAttr::Constant(v::num(value)?),
                        _ => return Err(v::fail("E_TYPE", "Radius is a number or packed float array.")),
                    }
                }
            };
            check_colors(n, &args)?;
            let (fill, stroke) = style(sink, n)?;
            push(sink, Entry::MarkSet { count: n, kind: Kind::Circle { position, radius, fill, stroke, stroke_width: ScalarAttr::Constant(1.0) } })
        }
        Mark::Rect => {
            let (n, position) = n_of_positions(sink, if batch { "positions" } else { "at" })?;
            let size = if !batch {
                PairAttr::Constant(pair(arg("size")?)?)
            } else {
                let value = arg("sizes")?;
                if let Some((id, count, width)) = device(sink, value) {
                    if width != 3 {
                        return Err(v::fail("E_TYPE", "Sizes are a vec3 or packed vec3 array."));
                    }
                    if count != n {
                        return Err(length_error("Sizes and positions have different lengths."));
                    }
                    PairAttr::Device(Buffer { id, count, width })
                } else {
                    match value {
                        Value::Floats(3, xs) => {
                            if xs.len() as i64 != 3 * n {
                                return Err(length_error("Sizes and positions have different lengths."));
                            }
                            PairAttr::Buffer(Buffer { id: sink.buffers.add(xs.clone()), count: n, width: 3 })
                        }
                        Value::Vec3(..) => PairAttr::Constant(pair(value)?),
                        _ => return Err(v::fail("E_TYPE", "Sizes are a vec3 or packed vec3 array.")),
                    }
                }
            };
            check_colors(n, &args)?;
            let (fill, stroke) = style(sink, n)?;
            push(sink, Entry::MarkSet { count: n, kind: Kind::Rect { position, size, fill, stroke, stroke_width: ScalarAttr::Constant(1.0) } })
        }
        Mark::Line => {
            let (n, position) = n_of_positions(sink, "from")?;
            let end = if batch {
                let (n2, end) = points_attr(sink, arg("to")?)?;
                if n2 != n {
                    return Err(length_error("Line endpoints have different lengths."));
                }
                end
            } else {
                PairAttr::Constant(pair(arg("to")?)?)
            };
            check_colors(n, &args)?;
            let stroke = colour_attr(sink, n, arg("color")?)?;
            let stroke_width = ScalarAttr::Constant(v::num(arg("width")?)?);
            push(sink, Entry::MarkSet { count: n, kind: Kind::Line { position, end, stroke, stroke_width } })
        }
    }
}

fn path(sink: &mut Sink, kind: PathKind, raw: &[(Symbol, i64)], values: &[Value]) -> Result<(), Diagnostic> {
    let args = resolve_args(raw, values);
    let arg = |key: &str| assoc(&args, key).expect("Not_found");
    let opt = |key: &str| -> Result<Option<i32>, Diagnostic> {
        match assoc(&args, key) {
            Some(value) => Ok(Some(color(value)?)),
            None => Ok(None),
        }
    };
    let points = || -> Result<Vec<(f64, f64)>, Diagnostic> {
        let xs = arg("points");
        let n = v::array_length(xs)?;
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            out.push(float_point(&v::array_get(xs, i as i64)?)?);
        }
        Ok(out)
    };
    match kind {
        PathKind::Polygon => {
            let points = points()?;
            let fill = opt("fill")?;
            let stroke = opt("stroke")?;
            geometry(sink, shape2::polygon(&points, fill, stroke))
        }
        PathKind::Polyline => {
            let points = points()?;
            let c = ink(arg("color"))?;
            geometry(sink, shape2::polyline(&points, c).map(|g| vec![g]))
        }
        PathKind::RoundedRect => {
            let (width, height) = float_point(arg("size"))?;
            let (x, y) = float_point(arg("at"))?;
            let radius = v::num(arg("radius"))?;
            push(sink, Entry::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: x, ty: y })?;
            let stroke = opt("stroke")?;
            let fill = opt("fill")?;
            geometry(sink, shape2::rounded_rect(width, height, radius, fill, stroke))?;
            push(sink, Entry::PopTransform)
        }
    }
}

/// A statement resolved once; run it on the locals.
pub type Compiled = Box<dyn Fn(&[Value], &mut Sink) -> Result<(), Diagnostic> + Send + Sync>;

pub fn compile(draw: &Draw) -> Compiled {
    match draw.clone() {
        Draw::Background(c) => Box::new(move |vals, sink| {
            let (w, h) = sink.size;
            let (w, h) = (w as f64, h as f64);
            let fill = color(&vals[c as usize])?;
            geometry(sink, shape2::polygon(&[(0.0, 0.0), (w, 0.0), (w, h), (0.0, h)], Some(fill), None))
        }),
        Draw::Point { at, ink: i } => Box::new(move |vals, sink| {
            let (x, y) = float_point(&vals[at as usize])?;
            let fill = ink(&vals[i as usize])?;
            geometry(sink, shape2::polygon(&[(x, y), (x + 1.0, y), (x + 1.0, y + 1.0), (x, y + 1.0)], Some(fill), None))
        }),
        Draw::Mark { mark: kind, args } => Box::new(move |vals, sink| self::mark(sink, false, kind, &args, vals)),
        Draw::Batch { mark: kind, args } => Box::new(move |vals, sink| self::mark(sink, true, kind, &args, vals)),
        Draw::Points { positions, ink: i } => Box::new(move |vals, sink| {
            let (count, position) = points_attr(sink, &vals[positions as usize])?;
            let fill = ink(&vals[i as usize])?;
            push(sink, Entry::MarkSet { count, kind: Kind::Point { position, fill } })
        }),
        Draw::Path { path: p, args } => Box::new(move |vals, sink| self::path(sink, p, &args, vals)),
        Draw::TextRun { at, text, ink: i, size } => Box::new(move |vals, sink| {
            let text = match &vals[text as usize] {
                Value::Text(s) => s.to_vec(),
                _ => return Err(v::fail("E_TYPE", "Drawing text is text.")),
            };
            if !text.is_empty() {
                let (x, y) = point(&vals[at as usize])?;
                let size = v::int_of(&vals[size as usize])?;
                let color = ink(&vals[i as usize])?;
                push(sink, Entry::Text { x, y, size, color, text })?;
            }
            Ok(())
        }),
        Draw::Image { source: (ty, id), at, scale, angle } => Box::new(move |vals, sink| {
            let resolve = match &sink.image {
                Some(resolve) => resolve.clone(),
                None => return Err(v::fail("E_IMAGE", "Drawing images need an environment-owned image resolver.")),
            };
            let resource = resolve(&Value::Deferred(ty.clone(), id))?;
            let (x, y) = point(&vals[at as usize])?;
            let scale = v::num(&vals[scale as usize])?;
            let angle = v::num(&vals[angle as usize])?;
            if !(scale.is_finite() && angle.is_finite()) || scale <= 0.0 {
                return Err(v::fail("E_DRAW_RANGE", "Scene.image: invalid transform"));
            }
            push(sink, Entry::Image { resource, x, y, scale, angle })
        }),
        Draw::Push(Transform::Translate, i) => Box::new(move |vals, sink| {
            let (x, y) = point(&vals[i as usize])?;
            push(sink, Entry::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: x as f64, ty: y as f64 })
        }),
        Draw::Push(Transform::Rotate, i) => Box::new(move |vals, sink| {
            let a = v::fin("angle", v::num(&vals[i as usize])?)?;
            let (c, s) = (rays_math::libm::cos(a), rays_math::libm::sin(a));
            push(sink, Entry::PushTransform { xx: c, xy: s, yx: -s, yy: c, tx: 0.0, ty: 0.0 })
        }),
        Draw::Push(Transform::Scale, i) => Box::new(move |vals, sink| {
            let (x, y) = match &vals[i as usize] {
                Value::Vec3(x, y, _) => (*x, *y),
                other => {
                    let n = v::num(other)?;
                    (n, n)
                }
            };
            let x = v::fin("scale", x)?;
            let y = v::fin("scale", y)?;
            push(sink, Entry::PushTransform { xx: x, xy: 0.0, yx: 0.0, yy: y, tx: 0.0, ty: 0.0 })
        }),
        Draw::Pop => Box::new(|_, sink| push(sink, Entry::PopTransform)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn text(s: &str) -> Value { Value::Text(s.as_bytes().into()) }
    fn run(draw: Draw, values: Vec<Value>, size: (i64, i64)) -> Result<Vec<Entry>, Diagnostic> {
        let buffers = Buffers::create();
        let mut s = sink(None, size, &buffers);
        compile(&draw)(&values, &mut s)?;
        finish(&s)
    }
    fn args(names: &[&str]) -> Vec<(Symbol, i64)> { names.iter().enumerate().map(|(i, n)| ((*n).into(), i as i64)).collect() }

    #[test]
    fn background_fills_the_canvas() {
        // picture 0: (draw/background "#14161c") on 200x100
        let entries = run(Draw::Background(0), vec![text("#14161c")], (200, 100)).unwrap();
        let Entry::Geometry { vertices, indices, color } = &entries[0] else { panic!("geometry") };
        assert_eq!((vertices.clone(), indices.len(), *color), (vec![0.0, 0.0, 200.0, 0.0, 200.0, 100.0, 0.0, 100.0], 6, 0x14161cff));
    }

    #[test]
    fn a_circle_is_one_mark() {
        // picture 2: (draw/circle [10.5 20 0] 4)
        let values = vec![Value::Vec3(10.5, 20.0, 0.0), Value::Int(4), text("#fff")];
        let entries = run(Draw::Mark { mark: Mark::Circle, args: args(&["at", "radius", "fill"]) }, values, (200, 100)).unwrap();
        assert_eq!(
            entries,
            vec![Entry::MarkSet {
                count: 1,
                kind: Kind::Circle {
                    position: PairAttr::Constant((10.5, 20.0)),
                    radius: ScalarAttr::Constant(4.0),
                    fill: ColorAttr::Constant(-1),
                    stroke: ColorAttr::Constant(0),
                    stroke_width: ScalarAttr::Constant(1.0),
                }
            }]
        );
    }

    #[test]
    fn a_batch_keeps_packed_storage_and_checks_lengths() {
        let positions = Value::Floats(3, Arc::new(vec![0.0, 0.0, 0.0, 10.0, 0.0, 0.0, 20.0, 0.0, 0.0]));
        let radii = Value::Floats(1, Arc::new(vec![1.0, 2.0, 3.0]));
        let buffers = Buffers::create();
        let mut s = sink(None, (200, 100), &buffers);
        let draw = Draw::Batch { mark: Mark::Circle, args: args(&["positions", "radius"]) };
        compile(&draw)(&[positions.clone(), radii], &mut s).unwrap();
        let entries = finish(&s).unwrap();
        assert!(matches!(&entries[0], Entry::MarkSet { count: 3, kind: Kind::Circle { position: PairAttr::Buffer(Buffer { id: 1, count: 3, width: 3 }), radius: ScalarAttr::Buffer(Buffer { id: 2, .. }), .. } }));
        assert_eq!(buffers.find(1).unwrap().len(), 9);
        // picture error: radii of a different length
        let short = Value::Floats(1, Arc::new(vec![1.0, 1.0]));
        let e = run(draw.clone(), vec![positions, short], (200, 100)).unwrap_err();
        assert_eq!(e.code, "E_DRAW_LENGTH");
    }

    #[test]
    fn range_errors_keep_their_codes() {
        // (draw/point [1e40 0 0]) and (draw/rect [0 0 0] [-1 1 0])
        let e = run(Draw::Point { at: 0, ink: 1 }, vec![Value::Vec3(1e40, 0.0, 0.0), text("#fff")], (10, 10)).unwrap_err();
        assert_eq!(e.code, "E_DRAW_RANGE");
        let values = vec![Value::Vec3(0.0, 0.0, 0.0), Value::Vec3(-1.0, 1.0, 0.0), text("#fff")];
        let e = run(Draw::Mark { mark: Mark::Rect, args: args(&["at", "size", "fill"]) }, values, (10, 10)).unwrap_err();
        assert_eq!(e.code, "E_DRAW_RANGE");
    }

    #[test]
    fn transforms_and_text_round_trip() {
        let buffers = Buffers::create();
        let mut s = sink(None, (10, 10), &buffers);
        let values = vec![Value::Vec3(1.9, -2.5, 0.0), text("hi"), text("#abc"), Value::Int(20), Value::Vec3(12.7, 12.0, 0.0)];
        compile(&Draw::Push(Transform::Translate, 0))(&values, &mut s).unwrap();
        compile(&Draw::TextRun { at: 4, text: 1, ink: 2, size: 3 })(&values, &mut s).unwrap();
        compile(&Draw::Pop)(&values, &mut s).unwrap();
        let entries = finish(&s).unwrap();
        assert_eq!(entries[0], Entry::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 1.0, ty: -2.0 });
        assert_eq!(entries[1], Entry::Text { x: 12, y: 12, size: 20, color: 0xaabbccffu32 as i32, text: b"hi".to_vec() });
        assert_eq!(entries[2], Entry::PopTransform);
        assert!(is_device(&device_value(&s, 7, 3, 1)) && !is_device(&Value::Int(0)));
    }
}
