use crate::render_ir::{self, Command, Geometry, Image, Rect, RenderIr};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

#[derive(Debug)]
pub struct Segment { id: i64, version: i64, ir: RenderIr, source_bytes: i64 }
static NEXT_ID: AtomicI64 = AtomicI64::new(1);

pub fn fresh_id() -> Result<i64, String> {
    loop {
        let value = NEXT_ID.load(Ordering::Relaxed);
        if value == i64::MAX { return Err("Display_list.fresh_id: identity space exhausted".into()); }
        if NEXT_ID.compare_exchange_weak(value, value + 1, Ordering::Relaxed, Ordering::Relaxed).is_ok() { return Ok(value); }
    }
}
pub fn id(value: &Segment) -> i64 { value.id }
pub fn version(value: &Segment) -> i64 { value.version }
pub fn source_bytes(value: &Segment) -> i64 { value.source_bytes }
pub fn render_ir(value: &Segment) -> &RenderIr { &value.ir }

#[derive(Debug)]
pub struct Builder {
    opcodes: Vec<u8>,
    values: Vec<f64>,
    colors: Vec<i32>,
    integers: Vec<i64>,
    geometries: Vec<Option<Geometry>>,
    glyph_runs: Vec<Option<Vec<render_ir::Glyph>>>,
    length: usize,
    published: Option<(i64, i64, Arc<Segment>)>,
}

impl Builder {
    pub fn create() -> Builder {
        let capacity = 16;
        Builder { opcodes: vec![0; capacity], values: vec![0.0; capacity * 8], colors: vec![0; capacity], integers: vec![0; capacity], geometries: vec![None; capacity], glyph_runs: vec![None; capacity], length: 0, published: None }
    }
    pub fn reset(&mut self) {
        self.geometries[..self.length].fill(None);
        self.glyph_runs[..self.length].fill(None);
        self.length = 0;
        self.published = None;
    }
    fn grow(&mut self, capacity: usize) {
        self.opcodes.resize(capacity, 0);
        self.values.resize(capacity * 8, 0.0);
        self.colors.resize(capacity, 0);
        self.integers.resize(capacity, 0);
        self.geometries.resize(capacity, None);
        self.glyph_runs.resize(capacity, None);
    }
    fn reserve(&mut self, required: usize) -> Result<(), String> {
        if required > 1_048_576 { return Err("Display_list.reserve: capacity exceeds command limit".into()); }
        if required > self.opcodes.len() {
            let mut capacity = self.opcodes.len();
            while capacity < required { capacity *= 2; }
            self.grow(capacity);
        }
        Ok(())
    }
    fn append(&mut self, opcode: u8, values: [f64; 8], color: i32, integer: i64, geometry: Option<Geometry>, glyphs: Option<Vec<render_ir::Glyph>>) -> Result<(), String> {
        self.reserve(self.length + 1)?;
        let i = self.length;
        self.opcodes[i] = opcode;
        self.values[i * 8..i * 8 + 8].copy_from_slice(&values);
        self.colors[i] = color;
        self.integers[i] = integer;
        self.geometries[i] = geometry;
        self.glyph_runs[i] = glyphs;
        self.length += 1;
        self.published = None;
        Ok(())
    }
    pub fn push_clip(&mut self, x: f64, y: f64, width: f64, height: f64) -> Result<(), String> {
        if ![x, y, width, height].iter().all(|v| v.is_finite()) || width < 0.0 || height < 0.0 {
            return Err("Display_list.push_clip: invalid extent".into());
        }
        self.append(1, [x, y, width, height, 0.0, 0.0, 0.0, 0.0], 0, 0, None, None)
    }
    pub fn pop_clip(&mut self) -> Result<(), String> { self.append(2, [0.0; 8], 0, 0, None, None) }
    pub fn geometry(&mut self, geometry: &Geometry) -> Result<(), String> { self.append(8, [0.0; 8], 0, 0, Some(geometry.clone()), None) }
    fn valid_rect(rect: Rect) -> bool { [rect.x, rect.y, rect.width, rect.height].iter().all(|v| v.is_finite()) && rect.width >= 0.0 && rect.height >= 0.0 }
    pub fn image(&mut self, resource_id: i64, source: Rect, destination: Rect) -> Result<(), String> {
        if resource_id <= 0 { return Err("Display_list.image: resource ID must be positive".into()); }
        if !Self::valid_rect(source) || !Self::valid_rect(destination) { return Err("Display_list.image: invalid rectangle".into()); }
        self.append(9, [source.x, source.y, source.width, source.height, destination.x, destination.y, destination.width, destination.height], 0, resource_id, None, None)
    }
    fn command(&self, index: usize) -> Result<Command, render_ir::Error> {
        let Some(values) = self.values.get(index * 8..index * 8 + 8) else { return Err(render_ir::Error::CorruptBuilder("Display_list: command storage is incomplete")); };
        let Some(&opcode) = self.opcodes.get(index) else { return Err(render_ir::Error::CorruptBuilder("Display_list: command storage is incomplete")); };
        let Some(&color) = self.colors.get(index) else { return Err(render_ir::Error::CorruptBuilder("Display_list: command storage is incomplete")); };
        let Some(&integer) = self.integers.get(index) else { return Err(render_ir::Error::CorruptBuilder("Display_list: command storage is incomplete")); };
        let (x, y, width, height) = (values[0], values[1], values[2], values[3]);
        match opcode {
            0 => Ok(Command::Geometry(Geometry { vertices: vec![x, y, x + width, y, x + width, y + height, x, y + height], indices: vec![0, 1, 2, 0, 2, 3], color })),
            1 => Ok(Command::PushClip(Rect { x, y, width, height })),
            2 => Ok(Command::PopClip),
            4 => Ok(Command::Clear(color)),
            5 => Ok(Command::SetBlend(match integer { 0 => render_ir::Blend::SourceOver, 1 => render_ir::Blend::Copy, 2 => render_ir::Blend::Replace, 3 => render_ir::Blend::Alpha, 4 => render_ir::Blend::Add, 5 => render_ir::Blend::Multiply, 6 => render_ir::Blend::Screen, 7 => render_ir::Blend::Subtract, _ => return Err(render_ir::Error::CorruptBuilder("Display_list: invalid blend opcode")) })),
            6 => Ok(Command::PushTransform(render_ir::Transform { xx: x, xy: y, yx: width, yy: height, tx: values[4], ty: values[5] })),
            7 => Ok(Command::PopTransform),
            8 => self.geometries.get(index).and_then(Option::as_ref).cloned().map(Command::Geometry).ok_or(render_ir::Error::CorruptBuilder("Display_list: geometry payload is missing")),
            9 => Ok(Command::Image(Image { resource_id: integer, source: Rect { x, y, width, height }, destination: Rect { x: values[4], y: values[5], width: values[6], height: values[7] } })),
            10 => self.glyph_runs.get(index).and_then(Option::as_ref).cloned().map(|glyphs| Command::Glyphs(render_ir::Glyphs { resource_id: integer, color, glyphs })).ok_or(render_ir::Error::CorruptBuilder("Display_list: glyph payload is missing")),
            _ => Err(render_ir::Error::CorruptBuilder("Display_list: invalid command opcode")),
        }
    }
    pub fn publish(&mut self, id: i64, version: i64) -> Result<Arc<Segment>, render_ir::Error> {
        if id <= 0 { return Err(render_ir::Error::InvalidArgument("Display_list.publish: ID must be positive")); }
        if version < 0 { return Err(render_ir::Error::InvalidArgument("Display_list.publish: version must be non-negative")); }
        if let Some((published_id, published_version, published)) = &self.published {
            if *published_id == id && *published_version == version { return Ok(published.clone()); }
        }
        let mut commands = Vec::with_capacity(self.length);
        for index in 0..self.length { commands.push(self.command(index)?); }
        let ir = render_ir::create_owned(commands)?;
        let source_bytes = self.opcodes.len()
            + self.values.len() * std::mem::size_of::<f64>()
            + self.colors.len() * std::mem::size_of::<i32>()
            + self.integers.len() * std::mem::size_of::<i64>()
            + self.geometries.iter().flatten().map(|g| (g.vertices.len() + g.indices.len()) * std::mem::size_of::<usize>()).sum::<usize>()
            + self.glyph_runs.iter().flatten().map(|g| g.len() * 24).sum::<usize>();
        let segment = Arc::new(Segment { id, version, ir, source_bytes: source_bytes as i64 });
        self.published = Some((id, version, segment.clone()));
        Ok(segment)
    }
}
