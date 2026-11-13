#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Buffer {
    pub id: i64,
    pub count: i64,
    pub width: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PairAttr { Constant((f64, f64)), Buffer(Buffer), Device(Buffer) }
#[derive(Clone, Debug, PartialEq)]
pub enum ScalarAttr { Constant(f64), Buffer(Buffer), Device(Buffer) }
#[derive(Clone, Debug, PartialEq)]
pub enum ColorAttr { Constant(i32), Buffer(Buffer), Device(Buffer) }

#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    Rect {
        position: PairAttr,
        size: PairAttr,
        fill: ColorAttr,
        stroke: ColorAttr,
        stroke_width: ScalarAttr,
    },
    Circle {
        position: PairAttr,
        radius: ScalarAttr,
        fill: ColorAttr,
        stroke: ColorAttr,
        stroke_width: ScalarAttr,
    },
    Line {
        position: PairAttr,
        end: PairAttr,
        stroke: ColorAttr,
        stroke_width: ScalarAttr,
    },
    Point { position: PairAttr, fill: i32 },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Entry {
    MarkSet { count: i64, kind: Kind },
    Geometry { vertices: Vec<f64>, indices: Vec<i64>, color: i32 },
    Image { resource: i64, x: i64, y: i64, scale: f64, angle: f64 },
    /// `style` is `text_style` of the face, alignment and tracking; 0 is the default run.
    Text { x: i64, y: i64, size: i64, color: i32, text: Vec<u8>, style: i64 },
    PushTransform { xx: f64, xy: f64, yx: f64, yy: f64, tx: f64, ty: f64 },
    PopTransform,
    PushClip { x: f64, y: f64, width: f64, height: f64 },
    PopClip,
}

pub type Table = Vec<Entry>;
pub const MAX_COUNT: i64 = 1_000_000;
pub const MAX_ENTRIES: i64 = 1_048_576;
const MAX_F32: f64 = 3.4028234663852886e38;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GpuToken { identity: i64, count: i64, stamp: i64 }

pub fn gpu_token(identity: i64, count: i64, stamp: i64) -> Result<GpuToken, String> {
    if identity <= 0 || !(0..=MAX_COUNT).contains(&count) || stamp < 0 {
        return Err("Marks.gpu_token: invalid identity, count or stamp".into());
    }
    Ok(GpuToken { identity, count, stamp })
}
pub fn gpu_identity(value: GpuToken) -> i64 { value.identity }
pub fn gpu_count(value: GpuToken) -> i64 { value.count }
pub fn gpu_stamp(value: GpuToken) -> i64 { value.stamp }
pub const fn max_entries() -> i64 { MAX_ENTRIES }

fn bounded(value: f64) -> bool { value.is_finite() && value.abs() <= MAX_F32 }
fn bad(i: usize, message: &str) -> String { format!("entry {i}: {message}") }

fn check_buffer(i: usize, name: &str, count: i64, widths: &[i64], b: Buffer) -> Result<(), String> {
    if b.id <= 0 { return Err(bad(i, &format!("{name} buffer id must be positive"))); }
    if b.count != count { return Err(bad(i, &format!("{name} buffer count differs from {count}"))); }
    if !widths.contains(&b.width) { return Err(bad(i, &format!("{name} buffer width {}", b.width))); }
    Ok(())
}

fn check_pair(i: usize, name: &str, count: i64, widths: &[i64], nonneg: bool, attr: &PairAttr) -> Result<(), String> {
    match attr {
        PairAttr::Buffer(b) | PairAttr::Device(b) => check_buffer(i, name, count, widths, *b),
        PairAttr::Constant((x, y)) => {
            if !(bounded(*x) && bounded(*y)) { return Err(bad(i, &format!("{name} not finite"))); }
            if nonneg && (*x < 0.0 || *y < 0.0) { return Err(bad(i, &format!("{name} negative"))); }
            Ok(())
        }
    }
}
fn check_scalar(i: usize, name: &str, count: i64, attr: &ScalarAttr) -> Result<(), String> {
    match attr {
        ScalarAttr::Buffer(b) | ScalarAttr::Device(b) => check_buffer(i, name, count, &[1], *b),
        ScalarAttr::Constant(v) => {
            if !bounded(*v) { return Err(bad(i, &format!("{name} not finite"))); }
            if *v < 0.0 { return Err(bad(i, &format!("{name} negative"))); }
            Ok(())
        }
    }
}
fn check_color(i: usize, name: &str, count: i64, attr: &ColorAttr) -> Result<(), String> {
    match attr { ColorAttr::Buffer(b) | ColorAttr::Device(b) => check_buffer(i, name, count, &[3], *b), ColorAttr::Constant(_) => Ok(()) }
}

fn check_entry(i: usize, entry: &Entry) -> Result<(), String> {
    match entry {
        Entry::MarkSet { count, kind } => {
            if *count < 0 || *count > MAX_COUNT { return Err(bad(i, &format!("count {count} out of range"))); }
            match kind {
                Kind::Rect { position, size, fill, stroke, stroke_width } => {
                    check_pair(i, "position", *count, &[2, 3], false, position)?;
                    check_pair(i, "size", *count, &[2, 3], true, size)?;
                    check_color(i, "fill", *count, fill)?;
                    check_color(i, "stroke", *count, stroke)?;
                    check_scalar(i, "stroke_width", *count, stroke_width)
                }
                Kind::Circle { position, radius, fill, stroke, stroke_width } => {
                    check_pair(i, "position", *count, &[2, 3], false, position)?;
                    check_scalar(i, "radius", *count, radius)?;
                    check_color(i, "fill", *count, fill)?;
                    check_color(i, "stroke", *count, stroke)?;
                    check_scalar(i, "stroke_width", *count, stroke_width)
                }
                Kind::Line { position, end, stroke, stroke_width } => {
                    check_pair(i, "position", *count, &[2, 3], false, position)?;
                    check_pair(i, "end", *count, &[2, 3], false, end)?;
                    check_color(i, "stroke", *count, stroke)?;
                    check_scalar(i, "stroke_width", *count, stroke_width)
                }
                Kind::Point { position, .. } => check_pair(i, "position", *count, &[2, 3], false, position),
            }
        }
        Entry::Geometry { vertices, indices, .. } => {
            if vertices.len() & 1 != 0 { return Err(bad(i, "odd vertex float count")); }
            if indices.len() % 3 != 0 { return Err(bad(i, "index count not a multiple of three")); }
            if vertices.iter().any(|v| !v.is_finite()) { return Err(bad(i, "nonfinite vertex")); }
            for &index in indices {
                if index < 0 || usize::try_from(index).map_or(true, |value| value >= vertices.len() / 2) { return Err(bad(i, &format!("index {index} out of range"))); }
            }
            Ok(())
        }
        Entry::Image { resource, scale, angle, .. } => {
            if *resource <= 0 { return Err(bad(i, "image resource must be positive")); }
            if !scale.is_finite() || *scale <= 0.0 { return Err(bad(i, "image scale")); }
            if !angle.is_finite() { return Err(bad(i, "image angle")); }
            Ok(())
        }
        Entry::Text { .. } => Ok(()),
        Entry::PushTransform { xx, xy, yx, yy, tx, ty } => {
            if [xx, xy, yx, yy, tx, ty].iter().any(|v| !v.is_finite()) { Err(bad(i, "nonfinite transform")) } else { Ok(()) }
        }
        Entry::PushClip { x, y, width, height } => {
            if [x, y, width, height].iter().any(|v| !v.is_finite()) { return Err(bad(i, "nonfinite clip")); }
            if *width < 0.0 || *height < 0.0 { return Err(bad(i, "negative clip extent")); }
            Ok(())
        }
        Entry::PopTransform | Entry::PopClip => Ok(()),
    }
}

/// The faces a text run can name, numbered in this order; the first is the default.
pub const FONTS: [&str; 12] = ["pragmasevka", "departure", "archivo", "archivo-semibold", "archivo-condensed-bold", "archivo-expanded-extrabold",
    "newsreader", "newsreader-semibold", "newsreader-display", "plex-condensed", "plex-condensed-semibold", "plex-mono"];
/// Where a text run's `at` is on its line: its left end, its middle or its right end.
pub const ALIGNS: [&str; 3] = ["left", "center", "right"];

/// A text run's style as one integer: the face, the alignment times 256, and the tracking in
/// thousandths of the size (0 to 2000) times 65536. The message names what was not understood.
pub fn text_style(font: &str, align: &str, tracking: f64) -> Result<i64, String> {
    let font = FONTS.iter().position(|name| *name == font).ok_or_else(|| format!("Unknown font {font:?}; the fonts are {}.", FONTS.join(", ")))?;
    let align = ALIGNS.iter().position(|name| *name == align).ok_or_else(|| format!("Unknown alignment {align:?}; text aligns left, center or right."))?;
    if !(0.0..=2.0).contains(&tracking) { return Err("Text tracking is 0 to 2, in sizes.".to_owned()); }
    Ok(font as i64 + 256 * align as i64 + 65536 * (tracking * 1000.0).round() as i64)
}
/// (face, alignment, tracking in sizes) of a style.
pub fn text_style_parts(style: i64) -> (usize, usize, f64) { ((style & 255) as usize, ((style >> 8) & 255) as usize, (style >> 16) as f64 / 1000.0) }

pub fn validate(table: &[Entry]) -> Result<(), String> {
    if table.len() > MAX_ENTRIES as usize { return Err(format!("more than {MAX_ENTRIES} entries")); }
    let (mut transforms, mut clips) = (0usize, 0usize);
    for (i, entry) in table.iter().enumerate() {
        check_entry(i, entry)?;
        match entry {
            Entry::PushTransform { .. } => transforms += 1,
            Entry::PushClip { .. } => clips += 1,
            Entry::PopTransform => { if transforms == 0 { return Err(format!("entry {i}: pop_transform without push")); } transforms -= 1; }
            Entry::PopClip => { if clips == 0 { return Err(format!("entry {i}: pop_clip without push")); } clips -= 1; }
            _ => (),
        }
    }
    if transforms != 0 || clips != 0 { return Err("unbalanced push at end of table".into()); }
    Ok(())
}

fn put_u8(out: &mut Vec<u8>, value: u8) { out.push(value); }
fn put_i32(out: &mut Vec<u8>, value: i32) { out.extend_from_slice(&value.to_le_bytes()); }
fn put_int(out: &mut Vec<u8>, value: i64) { out.extend_from_slice(&value.to_le_bytes()); }
fn put_float(out: &mut Vec<u8>, value: f64) { out.extend_from_slice(&value.to_bits().to_le_bytes()); }
fn put_pair(out: &mut Vec<u8>, (x, y): (f64, f64)) { put_float(out, x); put_float(out, y); }
fn put_buffer(out: &mut Vec<u8>, b: Buffer) { put_int(out, b.id); put_int(out, b.count); put_int(out, b.width); }
fn put_pair_attr(out: &mut Vec<u8>, value: &PairAttr) {
    match value {
        PairAttr::Constant(v) => { put_u8(out, 0); put_pair(out, *v); }
        PairAttr::Buffer(b) => { put_u8(out, 1); put_buffer(out, *b); }
        PairAttr::Device(b) => { put_u8(out, 2); put_buffer(out, *b); }
    }
}
fn put_scalar_attr(out: &mut Vec<u8>, value: &ScalarAttr) {
    match value {
        ScalarAttr::Constant(v) => { put_u8(out, 0); put_float(out, *v); }
        ScalarAttr::Buffer(b) => { put_u8(out, 1); put_buffer(out, *b); }
        ScalarAttr::Device(b) => { put_u8(out, 2); put_buffer(out, *b); }
    }
}
fn put_color_attr(out: &mut Vec<u8>, value: &ColorAttr) {
    match value {
        ColorAttr::Constant(v) => { put_u8(out, 0); put_i32(out, *v); }
        ColorAttr::Buffer(b) => { put_u8(out, 1); put_buffer(out, *b); }
        ColorAttr::Device(b) => { put_u8(out, 2); put_buffer(out, *b); }
    }
}

pub fn serialize(table: &[Entry]) -> Vec<u8> {
    let mut out = Vec::with_capacity(256);
    put_i32(&mut out, 0x524d4b31);
    put_int(&mut out, table.len() as i64);
    for entry in table {
        match entry {
            Entry::MarkSet { count, kind } => {
                put_u8(&mut out, 0);
                match kind {
                    Kind::Rect { position, size, fill, stroke, stroke_width } => {
                        put_u8(&mut out, 0); put_int(&mut out, *count);
                        put_pair_attr(&mut out, position); put_pair_attr(&mut out, size);
                        put_color_attr(&mut out, fill); put_color_attr(&mut out, stroke); put_scalar_attr(&mut out, stroke_width);
                    }
                    Kind::Circle { position, radius, fill, stroke, stroke_width } => {
                        put_u8(&mut out, 1); put_int(&mut out, *count);
                        put_pair_attr(&mut out, position); put_scalar_attr(&mut out, radius);
                        put_color_attr(&mut out, fill); put_color_attr(&mut out, stroke); put_scalar_attr(&mut out, stroke_width);
                    }
                    Kind::Line { position, end, stroke, stroke_width } => {
                        put_u8(&mut out, 2); put_int(&mut out, *count);
                        put_pair_attr(&mut out, position); put_pair_attr(&mut out, end);
                        put_color_attr(&mut out, stroke); put_scalar_attr(&mut out, stroke_width);
                    }
                    Kind::Point { position, fill } => {
                        put_u8(&mut out, 3); put_int(&mut out, *count);
                        put_pair_attr(&mut out, position); put_u8(&mut out, 0); put_i32(&mut out, *fill);
                    }
                }
            }
            Entry::Geometry { vertices, indices, color } => {
                put_u8(&mut out, 1); put_i32(&mut out, *color);
                put_int(&mut out, vertices.len() as i64);
                for &v in vertices { put_float(&mut out, v); }
                put_int(&mut out, indices.len() as i64);
                for &i in indices { put_int(&mut out, i); }
            }
            Entry::Image { resource, x, y, scale, angle } => {
                put_u8(&mut out, 2); put_int(&mut out, *resource); put_int(&mut out, *x); put_int(&mut out, *y); put_float(&mut out, *scale); put_float(&mut out, *angle);
            }
            Entry::Text { x, y, size, color, text, style } => {
                // a styled run has its own tag, so a default run serializes as it always did
                put_u8(&mut out, if *style == 0 { 3 } else { 8 }); put_int(&mut out, *x); put_int(&mut out, *y); put_int(&mut out, *size); put_i32(&mut out, *color); put_int(&mut out, text.len() as i64); out.extend_from_slice(text);
                if *style != 0 { put_int(&mut out, *style); }
            }
            Entry::PushTransform { xx, xy, yx, yy, tx, ty } => {
                put_u8(&mut out, 4); for &v in [xx, xy, yx, yy, tx, ty] { put_float(&mut out, v); }
            }
            Entry::PopTransform => put_u8(&mut out, 5),
            Entry::PushClip { x, y, width, height } => {
                put_u8(&mut out, 6); for &v in [x, y, width, height] { put_float(&mut out, v); }
            }
            Entry::PopClip => put_u8(&mut out, 7),
        }
    }
    out
}
