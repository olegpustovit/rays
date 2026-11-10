//! pxui_render: the PXUI painter. One instanced pipeline draws a whole UI layer: a unit quad drawn
//! N times, one 64-byte [`Instance`] each, into a render pass the caller opened. Nothing is
//! tessellated on the CPU; the fragment shader evaluates distance fields and samples two atlases.
//!
//! ```text
//! let mut painter = Painter::new(&device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, (1024, 2048), (1024, 1024))?;
//! text.drain_uploads(&mut |kind, x, y, w, h, bytes, stride| painter.upload(map(kind), x, y, w, h, bytes, stride).expect("atlas"));
//! painter.begin_frame();                                  // once per frame, before its first draw
//! painter.set_view(1, View { offset: [0.0, -scroll], ..View::IDENTITY })?;
//! /* open the pass (or reuse the scene pass) */
//! painter.draw(&encoder, [w_px, h_px], scale, &instances, &[])?;   // one draw call when no breaks
//! ```
//!
//! # Conventions (same as the Lisp `ui` shader this replaces)
//! - Coordinates are logical points, origin top-left, y down. `draw` takes the target size in pixels
//!   and the points-to-pixels `scale`; the pass viewport must cover the target. No pixel snapping
//!   happens here: an edge on a whole pixel is exactly hard, anything else is anti-aliased.
//! - Colours are `[r, g, b, a]` bytes, straight (non-premultiplied) alpha, no sRGB conversion. The
//!   shader outputs straight alpha blended with `Blend::Alpha` (src·a + dst·(1−a)). Today's packed
//!   `0xRRGGBBAA` words convert with `u32::to_be_bytes`.
//! - Borders are inside the rect (CSS `box-sizing: border-box`) and composite over the fill. Today's
//!   kit strokes centre a border of width w on the edge: emit the rect grown by w/2 (radius + w/2).
//! - Mask glyphs: colour with alpha × R8 coverage. Images: colour (tint) × RGBA texel, straight alpha.
//!   uv are normalised (0..1) for both atlases. The sampler is nearest/clamp, as today.
//!
//! # Instance layout (64 bytes, little-endian; f16 = IEEE half bits, see [`f16`])
//! | bytes | field         | Rect (0)                  | Mask (1) / Image (2) | Shadow (3)                   | Line (4)              | Arc (5)                   | Curve (6)          |
//! |-------|---------------|---------------------------|----------------------|------------------------------|-----------------------|---------------------------|--------------------|
//! | 0–15  | `rect` f32×4  | x0 y0 x1 y1               | x0 y0 x1 y1          | caster x0 y0 x1 y1           | ax ay bx by           | cx cy radius –            | ax ay bx by (ends) |
//! | 16–23 | `clip` i16×4  | clip x0 y0 x1 y1, whole points in view space; [`NO_CLIP`] = none                                                                           |||||||
//! | 24–27 | `color` rgba8 | fill (gradient stop 0)    | colour / tint        | shadow colour                | stroke                | stroke                    | stroke             |
//! | 28–31 | `color2` rgba8| border colour             | –                    | –                            | –                     | –                         | –                  |
//! | 32–39 | `aux` u32×2   | [0] gradient stop 1 rgba8 | uv u0v0, u1v1 unorm16×2 each | –                    | dash on, off, phase f16 | dash on, off, phase f16 | –                  |
//! | 40–47 | `radii` f16×4 | corners tl tr br bl       | image corners        | caster corners               | –                     | –                         | –                  |
//! | 48–49 | `width` f16   | border width              | –                    | –                            | stroke width          | stroke width              | stroke width       |
//! | 50    | `kind` u8     | see [`kind`]                                                                                                                              |||||||
//! | 51    | `flags` u8    | bits 0–1 gradient (1 linear, 2 radial, 3 conic) | –   | [`flags::INNER`]           | [`flags::ROUND`] caps | [`flags::ROUND`] caps     | (always round)     |
//! | 52–53 | `view` u16    | index into the view table; 0 = identity                                                                                                   |||||||
//! | 54–55 | `clip_radius` f16 | corner radius of `clip`                                                                                                               |||||||
//! | 56–63 | `extra`       | gradient geometry f16×4   | –                    | offset x y, sigma, spread f16 | –                    | start, sweep f32 (radians, + = clockwise) | control x y f32 |
//!
//! Gradient geometry is in rect-normalised units (0..1 across the rect): linear = from xy, to xy;
//! radial = centre xy, radius xy; conic = centre xy, start angle in turns (0 = +x, clockwise).
//! Stops interpolate premultiplied. Shadow sigma is the gaussian deviation (CSS blur radius / 2);
//! an outer shadow is not cut out under its caster (draw the caster on top).
//!
//! # View table
//! Every instance names a [`View`]: an offset added to its rect, clip and control points, an opacity
//! that scales its alpha, and a clip in target points. Scrolling or fading a cached view is one
//! `set_view`; index 0 is the identity and cannot be changed.
//!
//! # Batching
//! A layer is one draw call. [`Break`]s are the only splits: from `at_instance` on, image instances
//! sample the given external texture instead of the colour atlas.

use ogpu_core::backend::{self, Buffer, Device, Pipeline, RenderEncoder, Sampler, ShaderStage, Texture};
use ogpu_core::binding::{self, Kind as LayoutKind, LayoutEntry, Stage as LayoutStage};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor};
use ogpu_core::render_pass::{Cull, Primitive};
use ogpu_core::shader::{self, BindingKind, EntryPoint, Stage};
use ogpu_core::types::{AddressMode, BufferDescriptor, BufferUsage, Extent, MipFilter, Origin, SamplerDescriptor, SamplerFilter, TextureDescriptor, TextureFormat, TextureUsage};

const MSL: &str = include_str!("../shaders/ui.metal");
// Regenerated by shaders/build.sh from ui.vert / ui.frag.
const VERT_SPV: &[u8] = include_bytes!("../shaders/ui.vert.spv");
const FRAG_SPV: &[u8] = include_bytes!("../shaders/ui.frag.spv");

/// Instance kinds (byte 50).
pub mod kind {
    pub const RECT: u8 = 0;
    pub const MASK: u8 = 1;
    pub const IMAGE: u8 = 2;
    pub const SHADOW: u8 = 3;
    pub const LINE: u8 = 4;
    pub const ARC: u8 = 5;
    pub const CURVE: u8 = 6;
}
/// Instance flags (byte 51).
pub mod flags {
    pub const LINEAR: u8 = 1;
    pub const RADIAL: u8 = 2;
    pub const CONIC: u8 = 3;
    pub const INNER: u8 = 4;
    pub const ROUND: u8 = 8;
}
pub const NO_CLIP: [i16; 4] = [i16::MIN, i16::MIN, i16::MAX, i16::MAX];
pub const MAX_VIEWS: usize = 256;

/// One quad. Plain data: build with the constructors below or write the fields per the layout table.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instance {
    pub rect: [f32; 4],
    pub clip: [i16; 4],
    pub color: [u8; 4],
    pub color2: [u8; 4],
    pub aux: [u32; 2],
    pub radii: [u16; 4],
    pub width: u16,
    pub kind: u8,
    pub flags: u8,
    pub view: u16,
    pub clip_radius: u16,
    pub extra: [u32; 2],
}
const _: () = assert!(size_of::<Instance>() == 64);

/// IEEE half bits of `x` (round to nearest even).
pub fn f16(x: f32) -> u16 { ogpu_core::types::half_of_float(x as f64) as u16 }
fn halves(a: f32, b: f32) -> u32 { f16(a) as u32 | (f16(b) as u32) << 16 }
fn unorms(a: f32, b: f32) -> u32 {
    let u = |x: f32| (x.clamp(0.0, 1.0) * 65535.0).round() as u32;
    u(a) | u(b) << 16
}

impl Instance {
    pub const DEFAULT: Instance = Instance {
        rect: [0.0; 4], clip: NO_CLIP, color: [0; 4], color2: [0; 4], aux: [0; 2], radii: [0; 4],
        width: 0, kind: kind::RECT, flags: 0, view: 0, clip_radius: 0, extra: [0; 2],
    };
    /// A filled rect `[x0, y0, x1, y1]`.
    pub fn rect(rect: [f32; 4], color: [u8; 4]) -> Self { Instance { rect, color, ..Self::DEFAULT } }
    /// A glyph from the R8 mask atlas; `uv` is `[u0, v0, u1, v1]` normalised.
    pub fn mask(rect: [f32; 4], uv: [f32; 4], color: [u8; 4]) -> Self {
        Instance { rect, color, kind: kind::MASK, aux: [unorms(uv[0], uv[1]), unorms(uv[2], uv[3])], ..Self::DEFAULT }
    }
    /// A colour glyph or image from the RGBA atlas (or the texture of the enclosing [`Break`]).
    pub fn image(rect: [f32; 4], uv: [f32; 4], tint: [u8; 4]) -> Self { Instance { kind: kind::IMAGE, ..Self::mask(rect, uv, tint) } }
    /// An analytic gaussian shadow of the rounded rect `rect`/`radii`.
    pub fn shadow(rect: [f32; 4], radii: [f32; 4], color: [u8; 4], offset: [f32; 2], sigma: f32, spread: f32, inner: bool) -> Self {
        Instance {
            rect, color, kind: kind::SHADOW, flags: if inner { flags::INNER } else { 0 },
            extra: [halves(offset[0], offset[1]), halves(sigma, spread)], ..Self::DEFAULT
        }.radii(radii)
    }
    pub fn line(a: [f32; 2], b: [f32; 2], width: f32, color: [u8; 4]) -> Self {
        Instance { rect: [a[0], a[1], b[0], b[1]], color, width: f16(width), kind: kind::LINE, ..Self::DEFAULT }
    }
    /// A circular arc from angle `start` sweeping `sweep` radians (positive is clockwise on screen).
    pub fn arc(center: [f32; 2], radius: f32, start: f32, sweep: f32, width: f32, color: [u8; 4]) -> Self {
        Instance {
            rect: [center[0], center[1], radius, 0.0], color, width: f16(width), kind: kind::ARC,
            extra: [start.to_bits(), sweep.to_bits()], ..Self::DEFAULT
        }
    }
    /// A quadratic bezier `a`–`control`–`b`, round caps.
    pub fn curve(a: [f32; 2], control: [f32; 2], b: [f32; 2], width: f32, color: [u8; 4]) -> Self {
        Instance {
            rect: [a[0], a[1], b[0], b[1]], color, width: f16(width), kind: kind::CURVE,
            extra: [control[0].to_bits(), control[1].to_bits()], ..Self::DEFAULT
        }
    }
    /// Corner radii top-left, top-right, bottom-right, bottom-left.
    pub fn radii(self, radii: [f32; 4]) -> Self { Instance { radii: radii.map(f16), ..self } }
    pub fn border(self, width: f32, color: [u8; 4]) -> Self { Instance { width: f16(width), color2: color, ..self } }
    fn gradient(self, kind: u8, geometry: [f32; 4], to: [u8; 4]) -> Self {
        Instance { flags: self.flags & !3 | kind, aux: [u32::from_le_bytes(to), 0], extra: [halves(geometry[0], geometry[1]), halves(geometry[2], geometry[3])], ..self }
    }
    pub fn linear(self, from: [f32; 2], to: [f32; 2], color: [u8; 4]) -> Self { self.gradient(flags::LINEAR, [from[0], from[1], to[0], to[1]], color) }
    pub fn radial(self, center: [f32; 2], radius: [f32; 2], color: [u8; 4]) -> Self { self.gradient(flags::RADIAL, [center[0], center[1], radius[0], radius[1]], color) }
    pub fn conic(self, center: [f32; 2], start_turns: f32, color: [u8; 4]) -> Self { self.gradient(flags::CONIC, [center[0], center[1], start_turns, 0.0], color) }
    /// Dashes for lines and arcs, in points along the stroke.
    // ponytail: curves have no dashes (needs arc length of a quadratic); emit short curve pieces if needed.
    pub fn dash(self, on: f32, off: f32, phase: f32) -> Self { Instance { aux: [halves(on, off), halves(phase, 0.0)], ..self } }
    pub fn round_caps(self) -> Self { Instance { flags: self.flags | flags::ROUND, ..self } }
    pub fn clip(self, rect: [i16; 4], radius: f32) -> Self { Instance { clip: rect, clip_radius: f16(radius), ..self } }
    pub fn view(self, view: u16) -> Self { Instance { view, ..self } }
}

/// A per-view record: `offset` moves the view's instances, `opacity` scales their alpha, `clip`
/// (`[x0, y0, x1, y1]` in target points) bounds them.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct View { pub offset: [f32; 2], pub opacity: f32, pub clip: [f32; 4] }
impl View {
    pub const IDENTITY: View = View { offset: [0.0; 2], opacity: 1.0, clip: [-32768.0, -32768.0, 32767.0, 32767.0] };
}

/// From `at_instance` on, [`kind::IMAGE`] instances sample `texture` (`None`: the colour atlas).
#[derive(Clone, Copy)]
pub struct Break<'a> { pub at_instance: u32, pub texture: Option<&'a Texture> }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AtlasKind { Mask, Color }

/// Frames whose ring regions stay untouched: at most `FRAMES - 1` frames may be in flight on the GPU.
const FRAMES: usize = 3;
/// Storage-buffer offset alignment that every Vulkan device satisfies.
const ALIGN: usize = 256;
fn align(n: usize) -> usize { (n + ALIGN - 1) & !(ALIGN - 1) }
fn invalid(operation: &str, message: &str) -> Error { error::make(operation, ErrorKind::InvalidArgument, message) }

pub struct Painter {
    device: Device,
    pipeline: Pipeline,
    sampler: Sampler,
    mask: Texture,
    color: Texture,
    depth: bool,
    ring: Buffer,
    ring_ptr: *mut u8,
    region_size: usize,
    region: usize,
    cursor: usize,
    /// Rings replaced by growth, kept alive until the frames that used them are done.
    retired: Vec<(Buffer, usize)>,
    views: [View; MAX_VIEWS],
    views_len: usize,
    draw_calls: u64,
}

fn host_ring(device: &Device, region_size: usize) -> Result<(Buffer, *mut u8), Error> {
    let descriptor = BufferDescriptor { label: Some("pxui_render.instances".into()), size: (region_size * FRAMES) as i64, usage: vec![BufferUsage::Storage] };
    let ring = backend::create_host_buffer(device, descriptor)?;
    let pointer = backend::host_ptr(&ring)?.0.as_ptr();
    Ok((ring, pointer))
}

fn atlas(device: &Device, label: &str, format: TextureFormat, (width, height): (u32, u32)) -> Result<Texture, Error> {
    let texture = backend::create_texture(device, TextureDescriptor {
        label: Some(label.into()), width: width as i64, height: height as i64, depth: 1, mip_levels: 1, sample_count: 1,
        format, usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopyDst],
    })?;
    let texel = if format == TextureFormat::R8Unorm { 1 } else { 4 };
    let zeros = vec![0u8; width as usize * height as usize * texel];
    backend::write_texture(&texture, None, Origin { x: 0, y: 0, z: 0 }, Extent { width: width as i64, height: height as i64, depth: 1 }, (width as usize * texel) as i64, &zeros)?;
    Ok(texture)
}

fn pipeline(device: &Device, format: ColorFormat, depth: DepthFormat, samples: i64) -> Result<Pipeline, Error> {
    let name = ogpu_core::impl_::backend()?;
    // (binding, shader kind, layout kind, stage): two vertex storage buffers, two textures and a sampler.
    let slots = [
        (0, BindingKind::StorageBuffer, LayoutKind::Buffer, Stage::Vertex),
        (1, BindingKind::StorageBuffer, LayoutKind::Buffer, Stage::Vertex),
        (2, BindingKind::SampledTexture, LayoutKind::Texture, Stage::Fragment),
        (3, BindingKind::SampledTexture, LayoutKind::Texture, Stage::Fragment),
        (4, BindingKind::Sampler, LayoutKind::Sampler, Stage::Fragment),
    ];
    let artifact = |stage: Stage| shader::create(shader::Descriptor {
        backend: name.clone(),
        label: Some("pxui_render".into()),
        bytes: if name == "metal" { MSL.as_bytes().to_vec() } else if stage == Stage::Vertex { VERT_SPV.to_vec() } else { FRAG_SPV.to_vec() },
        entry_points: vec![EntryPoint { name: "ui_vertex".into(), stage: Stage::Vertex }, EntryPoint { name: "ui_fragment".into(), stage: Stage::Fragment }],
        bindings: slots.iter().filter(|s| s.3 == stage).map(|s| shader::Binding { group: 0, binding: s.0, kind: s.1, visibility: vec![s.3] }).collect(),
    });
    let entries: Vec<LayoutEntry> = slots.iter().map(|s| LayoutEntry {
        binding: s.0, kind: s.2, visibility: vec![if s.3 == Stage::Vertex { LayoutStage::Vertex } else { LayoutStage::Fragment }],
    }).collect();
    let layout = binding::create_pipeline_layout(backend::device_handle(device), &backend::capabilities(device), &[(0, binding::create_layout(&entries)?)])?;
    backend::create_render_pipeline(device, &RenderDescriptor {
        backend: name.clone(), label: Some("pxui_render".into()), layout,
        vertex: artifact(Stage::Vertex)?, vertex_entry: "ui_vertex".into(),
        fragment: Some(artifact(Stage::Fragment)?), fragment_entry: Some("ui_fragment".into()),
        color_format: format, depth_format: depth, sample_count: samples,
    }, Some(Blend::Alpha), Some(Primitive::TriangleList), Some(false))
}

impl Painter {
    /// A painter for passes of `format`/`depth`/`samples`, owning a `mask_size` R8 atlas and a
    /// `color_size` RGBA8 atlas (both zeroed).
    pub fn new(device: &Device, format: ColorFormat, depth: DepthFormat, samples: i64, mask_size: (u32, u32), color_size: (u32, u32)) -> Result<Painter, Error> {
        let pipeline = pipeline(device, format, depth, samples)?;
        let sampler = backend::create_sampler(device, SamplerDescriptor {
            label: Some("pxui_render".into()), min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest,
            mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge,
            lod_min: 0.0, lod_max: 0.0, max_anisotropy: 1,
        })?;
        let mask = atlas(device, "pxui_render.mask", TextureFormat::R8Unorm, mask_size)?;
        let color = atlas(device, "pxui_render.color", TextureFormat::Rgba8Unorm, color_size)?;
        let region_size = 4096 * 64;
        let (ring, ring_ptr) = host_ring(device, region_size)?;
        Ok(Painter {
            device: device.clone(), pipeline, sampler, mask, color, depth: depth != DepthFormat::NoDepth,
            ring, ring_ptr, region_size, region: 0, cursor: 0, retired: Vec::new(),
            views: [View::IDENTITY; MAX_VIEWS], views_len: 1, draw_calls: 0,
        })
    }

    /// Copy a `w`×`h` rectangle (rows `stride` bytes apart) into an atlas at `x`,`y`. Takes effect
    /// immediately: do not overwrite texels a frame still in flight samples.
    // ponytail: no atlas double-buffering; eviction must wait for the frame using the old glyph if that races.
    pub fn upload(&mut self, atlas: AtlasKind, x: u32, y: u32, w: u32, h: u32, bytes: &[u8], stride: usize) -> Result<(), Error> {
        let texture = if atlas == AtlasKind::Mask { &self.mask } else { &self.color };
        backend::write_texture(texture, None, Origin { x: x as i64, y: y as i64, z: 0 }, Extent { width: w as i64, height: h as i64, depth: 1 }, stride as i64, bytes)
    }

    /// Set view `index` (1..MAX_VIEWS) for the following draws.
    pub fn set_view(&mut self, index: u16, view: View) -> Result<(), Error> {
        let index = index as usize;
        if index == 0 || index >= MAX_VIEWS { return Err(invalid("Painter.set_view", "view index must be in 1..MAX_VIEWS")); }
        self.views[index] = view;
        self.views_len = self.views_len.max(index + 1);
        Ok(())
    }

    /// Start a frame: rotate to the ring region the GPU finished `FRAMES - 1` frames ago.
    pub fn begin_frame(&mut self) {
        self.region = (self.region + 1) % FRAMES;
        self.cursor = self.region * self.region_size;
        for (buffer, frames) in &mut self.retired {
            *frames -= 1;
            if *frames == 0 { let _ = backend::destroy_buffer(buffer); }
        }
        self.retired.retain(|(_, frames)| *frames > 0);
    }

    /// Draw calls issued since the painter was made.
    pub fn draw_calls(&self) -> u64 { self.draw_calls }
    pub fn atlas(&self, atlas: AtlasKind) -> &Texture { if atlas == AtlasKind::Mask { &self.mask } else { &self.color } }

    /// Reserve `need` bytes in this frame's region, growing the ring (a new allocation) if it is full.
    fn reserve(&mut self, need: usize) -> Result<usize, Error> {
        let end = (self.region + 1) * self.region_size;
        if self.cursor + need > end {
            let used = self.cursor - self.region * self.region_size;
            let region_size = align((used + need).max(self.region_size * 2));
            let (ring, pointer) = host_ring(&self.device, region_size)?;
            self.retired.push((std::mem::replace(&mut self.ring, ring), FRAMES));
            self.ring_ptr = pointer;
            self.region_size = region_size;
            self.cursor = self.region * region_size;
        }
        let at = self.cursor;
        self.cursor += need;
        Ok(at)
    }

    fn write(&self, at: usize, bytes: &[u8]) {
        // SAFETY: `reserve` returned `at` with room for these bytes in the live, host-mapped ring.
        unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), self.ring_ptr.add(at), bytes.len()) }
    }

    /// Draw `instances` into `pass` (opened by the caller on a `size_px` target, `scale` pixels per
    /// point). One draw call, plus one per non-empty segment after each [`Break`]. Leaves the pass
    /// with this painter's pipeline and bindings, cull off and (with depth) the depth test off.
    /// Call [`Painter::begin_frame`] once per frame: without it every frame appends and the ring grows.
    pub fn draw(&mut self, pass: &RenderEncoder, size_px: [u32; 2], scale: f32, instances: &[Instance], breaks: &[Break]) -> Result<(), Error> {
        const OP: &str = "Painter.draw";
        if !(scale > 0.0) || size_px[0] == 0 || size_px[1] == 0 { return Err(invalid(OP, "target size and scale must be positive")); }
        let mut previous = 0;
        for at in breaks.iter().map(|b| b.at_instance as usize) {
            if at < previous || at > instances.len() { return Err(invalid(OP, "breaks must be ascending and within the instances")); }
            previous = at;
        }
        if let Some(bad) = instances.iter().find(|i| i.kind > kind::CURVE || i.view as usize >= self.views_len) {
            return Err(invalid(OP, if bad.kind > kind::CURVE { "unknown instance kind" } else { "instance names an unset view" }));
        }
        if instances.is_empty() { return Ok(()); }
        let segment = |k: usize| {
            let from = if k == 0 { 0 } else { breaks[k - 1].at_instance as usize };
            let to = breaks.get(k).map_or(instances.len(), |b| b.at_instance as usize);
            (&instances[from..to], if k == 0 { None } else { breaks[k - 1].texture })
        };
        let header = align(16 + self.views_len * 32);
        let need = header + (0..=breaks.len()).map(|k| align(segment(k).0.len() * 64)).sum::<usize>();
        let start = self.reserve(need)?;
        self.write(start, &as_bytes(&[size_px[0] as f32 / scale, size_px[1] as f32 / scale])[..16]);
        for (i, v) in self.views[..self.views_len].iter().enumerate() {
            let record = [v.offset[0], v.offset[1], v.opacity, 0.0, v.clip[0], v.clip[1], v.clip[2], v.clip[3]];
            self.write(start + 16 + i * 32, &as_bytes(&record));
        }
        backend::set_render_pipeline(pass, &self.pipeline)?;
        backend::set_cull(pass, Cull::CullNone)?;
        if self.depth { backend::set_depth_state(pass, None)?; }
        backend::set_stage_buffer(pass, ShaderStage::Vertex, 1, Some(start as i64), &self.ring)?;
        backend::set_stage_texture(pass, ShaderStage::Fragment, 2, &self.mask)?;
        backend::set_stage_sampler(pass, ShaderStage::Fragment, 4, &self.sampler)?;
        let mut at = start + header;
        let mut bound: Option<*const Texture> = None;
        for k in 0..=breaks.len() {
            let (slice, texture) = segment(k);
            if slice.is_empty() { continue; }
            // SAFETY: Instance is repr(C) plain data with no padding (64 bytes, asserted above).
            let bytes = unsafe { std::slice::from_raw_parts(slice.as_ptr().cast::<u8>(), slice.len() * 64) };
            self.write(at, bytes);
            let texture = texture.unwrap_or(&self.color);
            if bound != Some(texture as *const Texture) {
                backend::set_stage_texture(pass, ShaderStage::Fragment, 3, texture)?;
                bound = Some(texture);
            }
            backend::set_stage_buffer(pass, ShaderStage::Vertex, 0, Some(at as i64), &self.ring)?;
            backend::draw(pass, Primitive::TriangleList, 0, 6, Some(slice.len() as i64))?;
            self.draw_calls += 1;
            at += align(bytes.len());
        }
        backend::flush_host_range(&self.ring, start as i64, at - start)
    }
}

fn as_bytes(floats: &[f32]) -> [u8; 32] {
    let mut out = [0u8; 32];
    for (chunk, f) in out.chunks_exact_mut(4).zip(floats) { chunk.copy_from_slice(&f.to_le_bytes()); }
    out
}

impl Drop for Painter {
    fn drop(&mut self) {
        let _ = backend::destroy_pipeline(&self.pipeline);
        let _ = backend::destroy_sampler(&self.sampler);
        let _ = backend::destroy_texture(&self.mask);
        let _ = backend::destroy_texture(&self.color);
        let _ = backend::destroy_buffer(&self.ring);
        for (buffer, _) in &self.retired { let _ = backend::destroy_buffer(buffer); }
    }
}
