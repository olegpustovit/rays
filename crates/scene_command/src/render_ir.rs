use crate::ui_batch::{self, Table as Shapes};
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Blend { SourceOver, Copy, Replace, Alpha, Add, Multiply, Screen, Subtract }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rect { pub x: f64, pub y: f64, pub width: f64, pub height: f64 }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transform { pub xx: f64, pub xy: f64, pub yx: f64, pub yy: f64, pub tx: f64, pub ty: f64 }
#[derive(Clone, Debug, PartialEq)]
pub struct Geometry { pub vertices: Vec<f64>, pub indices: Vec<i64>, pub color: i32 }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Image { pub resource_id: i64, pub source: Rect, pub destination: Rect }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Glyph { pub glyph_id: i64, pub x: f64, pub y: f64 }
#[derive(Clone, Debug, PartialEq)]
pub struct Glyphs { pub resource_id: i64, pub color: i32, pub glyphs: Vec<Glyph> }
#[derive(Debug)]
pub enum Command {
    Clear(i32), SetBlend(Blend), PushClip(Rect), PopClip,
    PushTransform(Transform), PopTransform, Geometry(Geometry), Shapes(Shapes), Image(Image), Glyphs(Glyphs),
}

impl Clone for Command {
    fn clone(&self) -> Self {
        match self {
            Self::Clear(v) => Self::Clear(*v), Self::SetBlend(v) => Self::SetBlend(*v),
            Self::PushClip(v) => Self::PushClip(*v), Self::PopClip => Self::PopClip,
            Self::PushTransform(v) => Self::PushTransform(*v), Self::PopTransform => Self::PopTransform,
            Self::Geometry(v) => Self::Geometry(v.clone()), Self::Shapes(v) => Self::Shapes(v.copy()),
            Self::Image(v) => Self::Image(*v), Self::Glyphs(v) => Self::Glyphs(v.clone()),
        }
    }
}

#[derive(Debug)]
pub struct RenderIr { id: i64, commands: Vec<Command> }
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    NonFinite, InvalidExtent, InvalidCardinality, InvalidIndex(i64), InvalidResourceId(i64),
    InvalidGlyphId(i64), UnbalancedClip, UnbalancedTransform, ComplexityLimit,
    InvalidArgument(&'static str), CorruptBuilder(&'static str),
}
static NEXT_ID: AtomicI64 = AtomicI64::new(1);

fn finite_rect(r: Rect) -> bool { r.x.is_finite() && r.y.is_finite() && r.width.is_finite() && r.height.is_finite() }
fn valid_rect(r: Rect) -> bool { finite_rect(r) && r.width >= 0.0 && r.height >= 0.0 }
fn finite_transform(t: Transform) -> bool { [t.xx, t.xy, t.yx, t.yy, t.tx, t.ty].iter().all(|v| v.is_finite()) }

fn create_internal(input: Vec<Command>, copy: bool) -> Result<RenderIr, Error> {
    if input.len() > 1_048_576 { return Err(Error::ComplexityLimit); }
    let (mut clips, mut transforms) = (0usize, 0usize);
    let mut failure = None;
    let fail = |failure: &mut Option<Error>, error| { if failure.is_none() { *failure = Some(error); } };
    for command in &input {
        match command {
            Command::Clear(_) | Command::SetBlend(_) => (),
            Command::Shapes(shapes) => if !ui_batch::valid_shapes(shapes) { fail(&mut failure, Error::InvalidCardinality); },
            Command::PushClip(r) => {
                if valid_rect(*r) { clips += 1; }
                else { fail(&mut failure, if finite_rect(*r) { Error::InvalidExtent } else { Error::NonFinite }); }
            }
            Command::PopClip => if clips == 0 { fail(&mut failure, Error::UnbalancedClip); } else { clips -= 1; },
            Command::PushTransform(t) => if finite_transform(*t) { transforms += 1; } else { fail(&mut failure, Error::NonFinite); },
            Command::PopTransform => if transforms == 0 { fail(&mut failure, Error::UnbalancedTransform); } else { transforms -= 1; },
            Command::Geometry(g) => {
                if g.vertices.len() % 2 != 0 || g.indices.len() % 3 != 0 { fail(&mut failure, Error::InvalidCardinality); }
                else if g.vertices.iter().any(|v| !v.is_finite()) { fail(&mut failure, Error::NonFinite); }
                else if let Some(&index) = g.indices.iter().find(|&&i| usize::try_from(i).map_or(true, |index| index >= g.vertices.len() / 2)) { fail(&mut failure, Error::InvalidIndex(index)); }
            }
            Command::Image(i) => {
                if i.resource_id <= 0 { fail(&mut failure, Error::InvalidResourceId(i.resource_id)); }
                else if !valid_rect(i.source) || !valid_rect(i.destination) { fail(&mut failure, if finite_rect(i.source) && finite_rect(i.destination) { Error::InvalidExtent } else { Error::NonFinite }); }
            }
            Command::Glyphs(g) => {
                if g.resource_id <= 0 { fail(&mut failure, Error::InvalidResourceId(g.resource_id)); }
                else {
                    for p in &g.glyphs {
                        if p.glyph_id < 0 { fail(&mut failure, Error::InvalidGlyphId(p.glyph_id)); }
                        else if !p.x.is_finite() || !p.y.is_finite() { fail(&mut failure, Error::NonFinite); }
                    }
                }
            }
        }
    }
    if failure.is_none() && clips != 0 { failure = Some(Error::UnbalancedClip); }
    if failure.is_none() && transforms != 0 { failure = Some(Error::UnbalancedTransform); }
    if let Some(error) = failure { return Err(error); }
    let commands = if copy { input.iter().map(Command::clone).collect() } else { input };
    Ok(RenderIr { id: NEXT_ID.fetch_add(1, Ordering::Relaxed), commands })
}

pub fn create(input: Vec<Command>) -> Result<RenderIr, Error> { create_internal(input, true) }
pub fn create_owned(input: Vec<Command>) -> Result<RenderIr, Error> { create_internal(input, false) }
pub fn identity(value: &RenderIr) -> i64 { value.id }
pub fn commands_readonly(value: &RenderIr) -> &[Command] { &value.commands }

fn put_u8(out: &mut Vec<u8>, value: u8) { out.push(value); }
fn put_i32(out: &mut Vec<u8>, value: i32) { out.extend_from_slice(&value.to_le_bytes()); }
fn put_int(out: &mut Vec<u8>, value: i64) { out.extend_from_slice(&value.to_le_bytes()); }
fn put_float(out: &mut Vec<u8>, value: f64) { out.extend_from_slice(&value.to_bits().to_le_bytes()); }
fn put_rect(out: &mut Vec<u8>, r: Rect) { for v in [r.x, r.y, r.width, r.height] { put_float(out, v); } }
fn put_transform(out: &mut Vec<u8>, t: Transform) { for v in [t.xx, t.xy, t.yx, t.yy, t.tx, t.ty] { put_float(out, v); } }

pub fn serialize(value: &RenderIr) -> Vec<u8> {
    let mut out = Vec::with_capacity(256);
    put_i32(&mut out, 0x52324931);
    put_int(&mut out, value.commands.len() as i64);
    for command in &value.commands {
        match command {
            Command::Clear(color) => { put_u8(&mut out, 0); put_i32(&mut out, *color); }
            Command::SetBlend(blend) => { put_u8(&mut out, 8); put_u8(&mut out, match blend { Blend::SourceOver => 0, Blend::Copy => 1, Blend::Replace => 2, Blend::Alpha => 3, Blend::Add => 4, Blend::Multiply => 5, Blend::Screen => 6, Blend::Subtract => 7 }); }
            Command::PushClip(r) => { put_u8(&mut out, 1); put_rect(&mut out, *r); }
            Command::PopClip => put_u8(&mut out, 2),
            Command::PushTransform(t) => { put_u8(&mut out, 3); put_transform(&mut out, *t); }
            Command::PopTransform => put_u8(&mut out, 4),
            Command::Geometry(g) => {
                put_u8(&mut out, 5); put_i32(&mut out, g.color); put_int(&mut out, g.vertices.len() as i64);
                for &v in &g.vertices { put_float(&mut out, v); }
                put_int(&mut out, g.indices.len() as i64);
                for &i in &g.indices { put_int(&mut out, i); }
            }
            Command::Shapes(shapes) => {
                put_u8(&mut out, 9); put_int(&mut out, shapes.count());
                match shapes.marks() {
                    None => put_u8(&mut out, 0),
                    Some(gpu) => { put_u8(&mut out, 1); put_int(&mut out, crate::marks::gpu_identity(gpu)); out.extend_from_slice(&crate::marks::gpu_stamp(gpu).to_le_bytes()); }
                }
                put_int(&mut out, shapes.instances().len() as i64); out.extend_from_slice(shapes.instances());
            }
            Command::Image(i) => { put_u8(&mut out, 6); put_int(&mut out, i.resource_id); put_rect(&mut out, i.source); put_rect(&mut out, i.destination); }
            Command::Glyphs(g) => {
                put_u8(&mut out, 7); put_int(&mut out, g.resource_id); put_i32(&mut out, g.color); put_int(&mut out, g.glyphs.len() as i64);
                for glyph in &g.glyphs { put_int(&mut out, glyph.glyph_id); put_float(&mut out, glyph.x); put_float(&mut out, glyph.y); }
            }
        }
    }
    out
}
