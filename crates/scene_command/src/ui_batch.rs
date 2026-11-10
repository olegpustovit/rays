use crate::marks::{self, GpuToken};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind { Rect, Textured, Wire, Grid }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Clip { pub x: f64, pub y: f64, pub width: f64, pub height: f64 }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Batch { pub first: i64, pub count: i64, pub clip: Option<Clip>, pub texture: i64 }

const INSTANCE_BYTES: usize = 64;
#[derive(Debug)]
pub struct Table {
    bytes: Arc<Vec<u8>>,
    count: i64,
    batches: Arc<Vec<Batch>>,
    marks: Option<GpuToken>,
    shapes_checked: Arc<AtomicBool>,
}
impl Clone for Table {
    fn clone(&self) -> Self {
        Table { bytes: self.bytes.clone(), count: self.count, batches: self.batches.clone(), marks: self.marks, shapes_checked: self.shapes_checked.clone() }
    }
}
impl Table {
    pub fn instances(&self) -> &[u8] { &self.bytes }
    /// Immutable published storage, retained without copying by native rendering.
    pub fn instances_shared(&self) -> Arc<Vec<u8>> { self.bytes.clone() }
    pub fn count(&self) -> i64 { self.count }
    pub fn batches(&self) -> &[Batch] { &self.batches }
    pub fn marks(&self) -> Option<GpuToken> { self.marks }
    pub fn textures(&self) -> Vec<i64> {
        let mut ids = Vec::new();
        for batch in self.batches.iter() {
            if batch.texture > 0 && !ids.contains(&batch.texture) { ids.push(batch.texture); }
        }
        ids
    }
    pub fn float(&self, instance: i64, word: i64) -> Result<f64, String> {
        let instance = usize::try_from(instance).map_err(|_| String::from("index out of bounds"))?;
        let word = usize::try_from(word).map_err(|_| String::from("index out of bounds"))?;
        if instance >= usize::try_from(self.count).unwrap_or(0) || word >= INSTANCE_BYTES / 4 {
            return Err("index out of bounds".into());
        }
        let offset = instance.checked_mul(INSTANCE_BYTES).and_then(|v| word.checked_mul(4).and_then(|w| v.checked_add(w))).ok_or_else(|| String::from("index out of bounds"))?;
        let bits = get_i32(&self.bytes, offset).ok_or_else(|| String::from("index out of bounds"))? as u32;
        Ok(f32::from_bits(bits) as f64)
    }
    pub fn copy(&self) -> Table {
        Table { bytes: Arc::new(self.bytes.as_ref().clone()), count: self.count, batches: self.batches.clone(), marks: self.marks, shapes_checked: Arc::new(AtomicBool::new(false)) }
    }
    pub fn valid_shapes(&self) -> bool {
        if let Some(token) = self.marks {
            return self.count == marks::gpu_count(token) && self.bytes.is_empty();
        }
        if self.shapes_checked.load(Ordering::Acquire) { return true; }
        let Some(count) = usize::try_from(self.count).ok().and_then(|n| n.checked_mul(INSTANCE_BYTES)) else { return false; };
        if count != self.bytes.len() { return false; }
        for instance in 0..self.count as usize {
            let base = instance * INSTANCE_BYTES;
            let Some(kind) = get_i32(&self.bytes, base + 40) else { return false; };
            let f = |word: usize| get_i32(&self.bytes, base + word * 4).map(|bits| f32::from_bits(bits as u32) as f64);
            if !(kind == 0 || kind == 4 || kind == 5)
                || !(0..=3).all(|word| f(word).map_or(false, f64::is_finite))
                || !f(11).map_or(false, f64::is_finite) || f(11).map_or(true, |v| v < 0.0)
                || !f(12).map_or(false, f64::is_finite) || f(12).map_or(true, |v| v < 0.0)
                || !f(13).map_or(false, f64::is_finite)
                || (kind != 5 && (f(2).map_or(true, |v| f(0).map_or(true, |x| v < x)) || f(3).map_or(true, |v| f(1).map_or(true, |y| v < y))))
            { return false; }
        }
        self.shapes_checked.store(true, Ordering::Release);
        true
    }
}

fn get_i32(bytes: &[u8], offset: usize) -> Option<i32> {
    let end = offset.checked_add(4)?;
    Some(i32::from_le_bytes(bytes.get(offset..end)?.try_into().ok()?))
}
fn set_i32(bytes: &mut Vec<u8>, offset: usize, value: i32) -> Result<(), String> {
    let end = offset.checked_add(4).ok_or_else(|| String::from("Ui_batch: invalid instance offset"))?;
    let Some(target) = bytes.get_mut(offset..end) else { return Err("Ui_batch: invalid instance offset".into()); };
    target.copy_from_slice(&value.to_le_bytes());
    Ok(())
}
fn set_float(bytes: &mut Vec<u8>, offset: usize, word: usize, value: f64) -> Result<(), String> { set_i32(bytes, offset + word * 4, (value as f32).to_bits() as i32) }
fn set_word(bytes: &mut Vec<u8>, offset: usize, word: usize, value: i32) -> Result<(), String> { set_i32(bytes, offset + word * 4, value) }
fn kind_code(kind: Kind) -> i32 { match kind { Kind::Rect => 0, Kind::Textured => 1, Kind::Wire => 2, Kind::Grid => 3 } }

pub fn empty() -> Table {
    Table { bytes: Arc::new(Vec::new()), count: 0, batches: Arc::new(Vec::new()), marks: None, shapes_checked: Arc::new(AtomicBool::new(false)) }
}
pub fn of_marks(token: GpuToken) -> Table {
    let count = marks::gpu_count(token);
    Table { bytes: Arc::new(Vec::new()), count, batches: Arc::new(vec![Batch { first: 0, count, clip: None, texture: 0 }]), marks: Some(token), shapes_checked: Arc::new(AtomicBool::new(false)) }
}
pub fn copy(value: &Table) -> Table { value.copy() }
pub fn valid_shapes(value: &Table) -> bool { value.valid_shapes() }
pub const fn instance_bytes() -> i64 { INSTANCE_BYTES as i64 }

#[derive(Debug)]
pub struct Builder {
    bytes: Arc<Vec<u8>>,
    length: usize,
    batches: Arc<Vec<Batch>>,
    open_first: usize,
    open_texture: i64,
    clip: Option<Clip>,
    render_ir_valid: bool,
    /// Storage of published tables (a frame publishes one per layer): reused once dropped.
    spares: Vec<(Arc<Vec<u8>>, Arc<Vec<Batch>>, Arc<AtomicBool>)>,
    shapes_checked: Arc<AtomicBool>,
}
impl Builder {
    pub fn create() -> Builder { Self::with_capacity(256) }
    pub fn with_capacity(capacity: i64) -> Builder {
        let capacity = capacity.max(1) as usize;
        Builder { bytes: Arc::new(vec![0; capacity.saturating_mul(INSTANCE_BYTES)]), length: 0, batches: Arc::new(Vec::new()), open_first: 0, open_texture: 0, clip: None, render_ir_valid: true, spares: Vec::new(), shapes_checked: Arc::new(AtomicBool::new(false)) }
    }
    pub fn reset(&mut self) {
        self.length = 0;
        self.open_first = 0;
        self.open_texture = 0;
        self.clip = None;
        self.render_ir_valid = true;
        // a published table still holds the storage (publishing shares it, not a copy): write a
        // spare its table dropped, or a new one, and keep this one as a spare
        if Arc::strong_count(&self.bytes) != 1 || Arc::strong_count(&self.batches) != 1 || Arc::strong_count(&self.shapes_checked) != 1 {
            let (bytes, batches, checked) = match self.spares.iter().position(|(bytes, batches, checked)| Arc::strong_count(bytes) == 1 && Arc::strong_count(batches) == 1 && Arc::strong_count(checked) == 1) {
                Some(i) => self.spares.swap_remove(i),
                None => (Arc::new(Vec::with_capacity(self.bytes.capacity())), Arc::new(Vec::with_capacity(self.batches.capacity())), Arc::new(AtomicBool::new(false))),
            };
            self.spares.push((std::mem::replace(&mut self.bytes, bytes), std::mem::replace(&mut self.batches, batches), std::mem::replace(&mut self.shapes_checked, checked)));
            if self.spares.len() > 64 { self.spares.retain(|(bytes, batches, checked)| Arc::strong_count(bytes) != 1 || Arc::strong_count(batches) != 1 || Arc::strong_count(checked) != 1); }
        }
        Arc::get_mut(&mut self.batches).expect("unique batches").clear();
        self.shapes_checked.store(false, Ordering::Release);
        let bytes = Arc::get_mut(&mut self.bytes).expect("unique storage");
        bytes.resize(bytes.capacity(), 0);
    }
    fn close(&mut self) {
        if self.length > self.open_first {
            Arc::make_mut(&mut self.batches).push(Batch { first: self.open_first as i64, count: (self.length - self.open_first) as i64, clip: self.clip, texture: self.open_texture });
        }
        self.open_first = self.length;
        self.open_texture = 0;
    }
    pub fn set_clip(&mut self, clip: Option<Clip>) {
        if self.clip != clip { self.close(); self.clip = clip; }
    }
    fn use_texture(&mut self, texture: i64) {
        if self.open_texture != 0 && self.open_texture != texture { self.close(); }
        self.open_texture = texture;
    }
    fn reserve(&mut self) -> Result<usize, String> {
        if Arc::strong_count(&self.shapes_checked) != 1 {
            self.shapes_checked = Arc::new(AtomicBool::new(false));
        }
        let needed = self.length.checked_add(1).and_then(|n| n.checked_mul(INSTANCE_BYTES)).ok_or_else(|| String::from("Ui_batch capacity overflow"))?;
        if needed > self.bytes.len() {
            if let Some(bytes) = Arc::get_mut(&mut self.bytes).filter(|b| b.capacity() >= needed) {
                bytes.resize(bytes.capacity(), 0);
            }
        }
        let current = self.bytes.len();
        if needed > current {
            let grown = needed.max(current.saturating_mul(2));
            let mut bytes = vec![0; grown];
            let used = self.length * INSTANCE_BYTES;
            let Some(source) = self.bytes.get(..used) else { return Err("Ui_batch: invalid instance storage".into()); };
            bytes[..used].copy_from_slice(source);
            self.bytes = Arc::new(bytes);
        } else {
            let start = self.length * INSTANCE_BYTES;
            let Some(target) = Arc::make_mut(&mut self.bytes).get_mut(start..needed) else { return Err("Ui_batch: invalid instance storage".into()); };
            target.fill(0);
        }
        let offset = self.length * INSTANCE_BYTES;
        self.length += 1;
        Ok(offset)
    }
    fn quad(bytes: &mut Vec<u8>, offset: usize, x0: f64, y0: f64, x1: f64, y1: f64) -> Result<(), String> {
        set_float(bytes, offset, 0, x0)?; set_float(bytes, offset, 1, y0)?; set_float(bytes, offset, 2, x1)?; set_float(bytes, offset, 3, y1)
    }
    pub fn rect(&mut self, x: f64, y: f64, width: f64, height: f64, color: i32, border_color: i32, border: f64, radius: f64, anti_alias: bool) -> Result<(), String> {
        self.render_ir_valid = false;
        if width > 0.0 && height > 0.0 {
            let offset = self.reserve()?;
            let bytes = Arc::make_mut(&mut self.bytes);
            Self::quad(bytes, offset, x, y, x + width, y + height)?;
            set_word(bytes, offset, 8, color)?; set_word(bytes, offset, 9, border_color)?; set_word(bytes, offset, 10, kind_code(Kind::Rect))?;
            set_float(bytes, offset, 11, radius)?; set_float(bytes, offset, 12, border)?; set_float(bytes, offset, 13, if anti_alias || radius > 0.0 { 1.0 } else { 0.0 })?;
        }
        Ok(())
    }
    pub fn textured(&mut self, texture: i64, x: f64, y: f64, width: f64, height: f64, u0: f64, v0: f64, u1: f64, v1: f64, color: i32) -> Result<(), String> {
        self.render_ir_valid = false;
        if texture <= 0 { return Err("Ui_batch.textured: texture must be positive".into()); }
        if width > 0.0 && height > 0.0 {
            self.use_texture(texture);
            let offset = self.reserve()?;
            let bytes = Arc::make_mut(&mut self.bytes);
            Self::quad(bytes, offset, x, y, x + width, y + height)?;
            for (word, value) in [(4, u0), (5, v0), (6, u1), (7, v1)] { set_float(bytes, offset, word, value)?; }
            set_word(bytes, offset, 8, color)?; set_word(bytes, offset, 10, kind_code(Kind::Textured))?;
        }
        Ok(())
    }
    pub fn wire(&mut self, points: [(f64, f64); 4], width: f64, color: i32) -> Result<(), String> {
        self.render_ir_valid = false;
        let distance = |a: (f64, f64), b: (f64, f64)| (b.0 - a.0).hypot(b.1 - a.1);
        let length = distance(points[0], points[1]) + distance(points[1], points[2]) + distance(points[2], points[3]);
        if !length.is_finite() { return Err("Ui_batch: invalid coordinates".into()); }
        let segments = ((length / 16.0).ceil() as usize).clamp(4, 48);
        let point = |t: f64| {
            let s = 1.0 - t;
            let a = s * s * s; let b = 3.0 * s * s * t; let c = 3.0 * s * t * t; let d = t * t * t;
            (a * points[0].0 + b * points[1].0 + c * points[2].0 + d * points[3].0,
             a * points[0].1 + b * points[1].1 + c * points[2].1 + d * points[3].1)
        };
        let clip = self.clip;
        let visible = |a: (f64, f64), b: (f64, f64)| match clip {
            None => true,
            Some(clip) => {
                let pad = width + 2.0;
                a.0.min(b.0) - pad <= clip.x + clip.width && a.0.max(b.0) + pad >= clip.x
                    && a.1.min(b.1) - pad <= clip.y + clip.height && a.1.max(b.1) + pad >= clip.y
            }
        };
        for segment in 0..segments {
            let t0 = segment as f64 / segments as f64;
            let t1 = (segment + 1) as f64 / segments as f64;
            if visible(point(t0), point(t1)) {
                let offset = self.reserve()?;
                let bytes = Arc::make_mut(&mut self.bytes);
                Self::quad(bytes, offset, points[0].0, points[0].1, points[1].0, points[1].1)?;
                set_float(bytes, offset, 4, points[2].0)?; set_float(bytes, offset, 5, points[2].1)?; set_float(bytes, offset, 6, points[3].0)?; set_float(bytes, offset, 7, points[3].1)?;
                set_word(bytes, offset, 8, color)?; set_word(bytes, offset, 10, kind_code(Kind::Wire))?; set_float(bytes, offset, 12, width)?; set_float(bytes, offset, 14, t0)?; set_float(bytes, offset, 15, t1)?;
            }
        }
        Ok(())
    }
    pub fn grid(&mut self, x: f64, y: f64, width: f64, height: f64, origin_x: f64, origin_y: f64, spacing: f64, dot: f64, color: i32) -> Result<(), String> {
        self.render_ir_valid = false;
        if width > 0.0 && height > 0.0 && spacing > 0.0 {
            let offset = self.reserve()?;
            let bytes = Arc::make_mut(&mut self.bytes);
            Self::quad(bytes, offset, x, y, x + width, y + height)?;
            for (word, value) in [(4, origin_x), (5, origin_y), (6, spacing), (7, dot)] { set_float(bytes, offset, word, value)?; }
            set_word(bytes, offset, 8, color)?; set_word(bytes, offset, 10, kind_code(Kind::Grid))?;
        }
        Ok(())
    }
    pub fn publish(&mut self) -> Result<Table, String> {
        self.close();
        let len = self.length * INSTANCE_BYTES;
        if let Some(bytes) = Arc::get_mut(&mut self.bytes) { bytes.truncate(len); }
        let bytes = if len == self.bytes.len() { self.bytes.clone() } else {
            let Some(source) = self.bytes.get(..len) else { return Err("Ui_batch: invalid instance storage".into()); };
            Arc::new(source.to_vec())
        };
        if Arc::strong_count(&self.shapes_checked) == 1 {
            self.shapes_checked.store(self.render_ir_valid, Ordering::Release);
        }
        Ok(Table { bytes, count: self.length as i64, batches: self.batches.clone(), marks: None, shapes_checked: self.shapes_checked.clone() })
    }
    fn shape_append(&mut self, kind: i32, x0: f64, y0: f64, x1: f64, y1: f64, fill: i32, stroke: i32, stroke_width: f64) -> Result<(), String> {
        if ![x0, y0, x1, y1, stroke_width].iter().all(|v| v.is_finite() && v.abs() <= 3.4028234663852886e38) || stroke_width < 0.0 {
            return Err("Ui_batch: invalid coordinates".into());
        }
        if self.length >= 1_000_000 { return Err("Ui_batch: instance limit".into()); }
        let offset = self.reserve()?;
        let bytes = Arc::make_mut(&mut self.bytes);
        for (word, value) in [(0, x0), (1, y0), (2, x1), (3, y1)] { set_float(bytes, offset, word, value)?; }
        set_word(bytes, offset, 8, fill)?; set_word(bytes, offset, 9, stroke)?; set_word(bytes, offset, 10, kind)?; set_float(bytes, offset, 12, stroke_width)?; set_float(bytes, offset, 13, 1.0)?;
        Ok(())
    }
    pub fn shape_rect(&mut self, x: f64, y: f64, width: f64, height: f64, fill: i32, stroke: i32) -> Result<(), String> {
        if !width.is_finite() || !height.is_finite() || width < 0.0 || height < 0.0 { return Err("Ui_batch extent".into()); }
        self.shape_append(0, x, y, x + width, y + height, fill, stroke, if stroke == 0 { 0.0 } else { 1.0 })
    }
    pub fn circle(&mut self, x: f64, y: f64, radius: f64, fill: Option<i32>, stroke: Option<i32>, stroke_width: f64) -> Result<(), String> {
        if !radius.is_finite() || radius < 0.0 { return Err("Ui_batch radius".into()); }
        let fill = fill.unwrap_or(-1);
        let stroke = stroke.unwrap_or(0);
        self.shape_append(4, x - radius, y - radius, x + radius, y + radius, fill, stroke, if stroke == 0 { 0.0 } else { stroke_width })
    }
    pub fn line(&mut self, x0: f64, y0: f64, x1: f64, y1: f64, color: i32, width: f64) -> Result<(), String> {
        self.shape_append(5, x0, y0, x1, y1, color, 0, width)
    }
    fn at(data: &[f64], stride: i64, i: i64, component: i64) -> Result<f64, String> {
        let index = stride.checked_mul(i).and_then(|v| v.checked_add(component)).and_then(|v| usize::try_from(v).ok()).ok_or_else(|| String::from("index out of bounds"))?;
        data.get(index).copied().ok_or_else(|| String::from("index out of bounds"))
    }
    pub fn circles(&mut self, count: i64, position: &[f64], position_stride: i64, radius: &[f64], radius_stride: i64, fill: i32, stroke: i32, stroke_width: &[f64], width_stride: i64) -> Result<(), String> {
        for i in 0..count.max(0) {
            let (x, y, r, w) = (Self::at(position, position_stride, i, 0)?, Self::at(position, position_stride, i, 1)?, Self::at(radius, radius_stride, i, 0)?, Self::at(stroke_width, width_stride, i, 0)?);
            if !r.is_finite() || r < 0.0 { return Err("Ui_batch radius".into()); }
            self.shape_append(4, x - r, y - r, x + r, y + r, fill, stroke, if stroke == 0 { 0.0 } else { w })?;
        }
        Ok(())
    }
    pub fn rects(&mut self, count: i64, position: &[f64], position_stride: i64, size: &[f64], size_stride: i64, fill: i32, stroke: i32) -> Result<(), String> {
        for i in 0..count.max(0) {
            let (x, y, width, height) = (Self::at(position, position_stride, i, 0)?, Self::at(position, position_stride, i, 1)?, Self::at(size, size_stride, i, 0)?, Self::at(size, size_stride, i, 1)?);
            if !width.is_finite() || !height.is_finite() || width < 0.0 || height < 0.0 { return Err("Ui_batch extent".into()); }
            self.shape_append(0, x, y, x + width, y + height, fill, stroke, if stroke == 0 { 0.0 } else { 1.0 })?;
        }
        Ok(())
    }
    pub fn lines(&mut self, count: i64, from: &[f64], from_stride: i64, to: &[f64], to_stride: i64, color: i32, width: &[f64], width_stride: i64) -> Result<(), String> {
        for i in 0..count.max(0) {
            self.shape_append(5, Self::at(from, from_stride, i, 0)?, Self::at(from, from_stride, i, 1)?, Self::at(to, to_stride, i, 0)?, Self::at(to, to_stride, i, 1)?, color, 0, Self::at(width, width_stride, i, 0)?)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // lib/scene_command/test_ui_batch.ml: packed coordinates, clipping, batches and reset.
    #[test]
    fn logical_coordinates_clipped_chords_and_immutable_reset() {
        let mut builder = Builder::with_capacity(1);
        builder.rect(1.25, -2.5, 3.75, 4.5, 0x12345678, 0, 0.0, 0.0, false).unwrap();
        let clip = Clip { x: 0.0, y: 0.0, width: 10.0, height: 10.0 };
        builder.set_clip(Some(clip));
        builder.wire([(100.0, 100.0), (120.0, 100.0), (140.0, 100.0), (160.0, 100.0)], 1.0, 0x01020304).unwrap();
        builder.wire([(1.0, 2.0), (3.0, 2.0), (5.0, 2.0), (7.0, 2.0)], 1.0, 0x01020304).unwrap();
        for texture in [2, 3] {
            builder.textured(texture, 0.0, 0.0, 2.0, 2.0, 3.0, 4.0, 5.0, 6.0, -1).unwrap();
        }
        let packed = builder.publish().unwrap();
        assert_eq!(packed.count(), 7);
        for (word, expected) in [1.25, -2.5, 5.0, 2.0].into_iter().enumerate() {
            assert_eq!(packed.float(0, word as i64).unwrap(), expected);
        }
        assert_eq!(get_i32(packed.instances(), 32), Some(0x12345678));
        for i in 0..4 {
            for (word, expected) in [1.0, 2.0, 3.0, 2.0, 5.0, 2.0, 7.0, 2.0].into_iter().enumerate() {
                assert_eq!(packed.float(i + 1, word as i64).unwrap(), expected);
            }
            assert_eq!(packed.float(i + 1, 14).unwrap(), i as f64 / 4.0);
            assert_eq!(packed.float(i + 1, 15).unwrap(), (i + 1) as f64 / 4.0);
        }
        assert_eq!(packed.batches(), &[
            Batch { first: 0, count: 1, texture: 0, clip: None },
            Batch { first: 1, count: 5, texture: 2, clip: Some(clip) },
            Batch { first: 6, count: 1, texture: 3, clip: Some(clip) },
        ]);
        assert_eq!(packed.textures(), vec![2, 3]);
        let bytes = packed.instances().to_vec();
        builder.reset();
        builder.rect(50.0, 60.0, 1.0, 1.0, -1, 0, 0.0, 0.0, false).unwrap();
        let reset = builder.publish().unwrap();
        assert_eq!(reset.count(), 1);
        assert_eq!(reset.batches().len(), 1);
        assert_eq!(reset.batches()[0].clip, None);
        assert_eq!(packed.instances(), bytes);
        assert_eq!(packed.batches().len(), 3);
    }
    fn published(builder: &mut Builder) -> Table {
        match builder.publish() {
            Ok(table) => table,
            Err(error) => { assert!(false, "{error}"); empty() }
        }
    }

    #[test]
    fn shape_validation_is_cached_but_a_copy_is_checked_again() {
        let mut builder = Builder::with_capacity(1);
        assert!(builder.rect(1.0, 1.0, 2.0, 2.0, 0, 0, 0.0, 0.0, false).is_ok());
        let good = published(&mut builder);
        let shared = good.clone();
        assert!(!good.shapes_checked.load(Ordering::Acquire));
        assert!(good.valid_shapes());
        assert!(good.shapes_checked.load(Ordering::Acquire));
        assert!(shared.shapes_checked.load(Ordering::Acquire));
        assert!(Arc::ptr_eq(&good.batches, &shared.batches));
        builder.textured(1, 0., 0., 2., 2., 0., 0., 1., 1., -1).unwrap();
        let changed = published(&mut builder);
        assert!(!Arc::ptr_eq(&good.shapes_checked, &changed.shapes_checked));
        assert!(good.valid_shapes());
        assert!(!changed.valid_shapes(), "a previous publication's validation must not authorize new texture data");
        for (offset, value) in [(0, 0x7fc00000u32), (40, 1), (48, (-1.0f32).to_bits())] {
            let mut bad = good.copy();
            Arc::make_mut(&mut bad.bytes)[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
            assert!(!bad.valid_shapes());
        }
        let mut rect_builder = Builder::with_capacity(1);
        assert!(rect_builder.shape_rect(1.0, 1.0, 2.0, 2.0, -1, 0).is_ok());
        let rect = published(&mut rect_builder);
        let mut tampered = rect.copy();
        Arc::make_mut(&mut tampered.bytes)[8..12].copy_from_slice(&0.0f32.to_bits().to_le_bytes());
        assert!(!tampered.valid_shapes());
    }

    #[test]
    fn publish_shares_storage() {
        let mut full = Builder::with_capacity(1);
        assert!(full.shape_rect(0.0, 0.0, 1.0, 1.0, -1, 0).is_ok());
        let full_snapshot = published(&mut full);
        assert!(Arc::ptr_eq(&full.bytes, &full_snapshot.bytes));
        let crate::render_ir::Command::Shapes(command_snapshot) = crate::render_ir::Command::Shapes(full_snapshot.clone()).clone() else { unreachable!() };
        assert!(Arc::ptr_eq(&full_snapshot.bytes, &command_snapshot.bytes), "commands retain immutable published storage");
        assert!(Arc::ptr_eq(&full_snapshot.shapes_checked, &command_snapshot.shapes_checked));
        assert!(full.shape_rect(2.0, 0.0, 1.0, 1.0, -1, 0).is_ok());
        assert!(!Arc::ptr_eq(&full.bytes, &full_snapshot.bytes));
        assert_eq!(full_snapshot.count(), 1);
        assert_eq!(command_snapshot.count(), 1);
        assert_eq!(command_snapshot.instances(), full_snapshot.instances());
        let mut partial = Builder::with_capacity(2);
        assert!(partial.shape_rect(0.0, 0.0, 1.0, 1.0, -1, 0).is_ok());
        let first = published(&mut partial);
        assert!(Arc::ptr_eq(&partial.bytes, &first.bytes) && first.instances().len() == INSTANCE_BYTES);
        // the next frame writes a spare; the one after reuses this storage once its table is gone
        partial.reset();
        assert!(!Arc::ptr_eq(&partial.bytes, &first.bytes));
        let storage = Arc::as_ptr(&first.bytes);
        let batches = Arc::as_ptr(&first.batches);
        let checked = Arc::as_ptr(&first.shapes_checked);
        drop(first);
        assert!(partial.shape_rect(0.0, 0.0, 1.0, 1.0, -1, 0).is_ok());
        let second = published(&mut partial);
        partial.reset();
        assert!(Arc::as_ptr(&partial.bytes) == storage, "the storage two frames back is reused");
        assert_eq!(Arc::as_ptr(&partial.batches), batches, "batch metadata storage is reused with its bytes");
        assert_eq!(Arc::as_ptr(&partial.shapes_checked), checked, "the dropped publication's validation owner is reused");
        drop(second);
    }

    #[test]
    fn repeated_publish_keeps_closed_batches() {
        let mut builder = Builder::with_capacity(4);
        assert!(builder.rect(0.0, 0.0, 1.0, 1.0, 0, 0, 0.0, 0.0, false).is_ok());
        builder.set_clip(Some(Clip { x: 0.0, y: 0.0, width: 4.0, height: 4.0 }));
        assert!(builder.rect(1.0, 1.0, 1.0, 1.0, 0, 0, 0.0, 0.0, false).is_ok());
        let first = published(&mut builder);
        assert_eq!(first.batches().len(), 2);
        assert!(builder.rect(2.0, 2.0, 1.0, 1.0, 0, 0, 0.0, 0.0, false).is_ok());
        let second = published(&mut builder);
        assert_eq!(first.batches().len(), 2);
        assert_eq!(second.batches().len(), 3);
    }
}
