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
//! // a cached layer: only the instances that changed since its last frame are written
//! painter.draw_retained(&encoder, [w_px, h_px], scale, layer, &instances, &dirty_ranges, &[])?;
//! // backdrop blur: end the pass that drew the backdrop, blur, then draw the result as an image
//! let blurred = painter.blur(&queue, &backdrop, [x0, y0, x1, y1], sigma_px)?.clone();
//! painter.draw(&encoder, .., &[Instance::image(rect, [0.0, 0.0, 1.0, 1.0], tint)], &[Break { at_instance: 0, texture: Some(&blurred) }])?;
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
//!   uv are normalised (0..1) for both atlases. Sampling is nearest/clamp; an image with
//!   [`flags::SMOOTH`] samples linear/clamp (a scaled picture: no texel ties between GPUs, no blocks).
//!   At 1:1 on whole pixels a sample lands on a texel centre, where linear returns the texel.
//!
//! # Instance layout (64 bytes, little-endian; f16 = IEEE half bits, see [`f16`])
//! | bytes | field         | Rect (0)                  | Mask (1) / Image (2) | Shadow (3)                   | Line (4)              | Arc (5)                   | Curve (6)          |
//! |-------|---------------|---------------------------|----------------------|------------------------------|-----------------------|---------------------------|--------------------|
//! | 0–15  | `rect` f32×4  | x0 y0 x1 y1               | x0 y0 x1 y1          | caster x0 y0 x1 y1           | ax ay bx by           | cx cy radius –            | ax ay bx by (ends) |
//! | 16–23 | `clip` i16×4  | clip x0 y0 x1 y1, whole points in view space; [`NO_CLIP`] = none                                                                           |||||||
//! | 24–27 | `color` rgba8 | fill (gradient stop 0)    | colour / tint        | shadow colour                | stroke                | stroke                    | stroke             |
//! | 28–31 | `color2` rgba8| border colour             | –                    | –                            | –                     | –                         | –                  |
//! | 32–39 | `aux` u32×2   | [0] gradient stop 1 rgba8, [1] ramp row + 1 (0 = none) | uv u0v0, u1v1 unorm16×2 each | –                    | dash on, off, phase f16 | dash on, off, phase f16 | –                  |
//! | 40–47 | `radii` f16×4 | corners tl tr br bl       | image corners        | caster corners               | –                     | –                         | –                  |
//! | 48–49 | `width` f16   | border width              | –                    | –                            | stroke width          | stroke width              | stroke width       |
//! | 50    | `kind` u8     | see [`kind`]                                                                                                                              |||||||
//! | 51    | `flags` u8    | bits 0–1 gradient (1 linear, 2 radial, 3 conic); bits 4–7 border sides [`flags::TOP`]… | image: [`flags::SMOOTH`] | [`flags::INNER`]           | [`flags::ROUND`] caps | [`flags::ROUND`] caps     | (always round)     |
//! | 52–53 | `view` u16    | index into the view table; 0 = identity                                                                                                   |||||||
//! | 54–55 | `clip_radius` f16 | corner radius of `clip`                                                                                                               |||||||
//! | 56–63 | `extra`       | gradient geometry f16×4   | –                    | offset x y, sigma, spread f16 | –                    | start, sweep f32 (radians, + = clockwise) | control x y f32 |
//!
//! Gradient geometry is in rect-normalised units (0..1 across the rect): linear = from xy, to xy;
//! radial = centre xy, radius xy; conic = centre xy, start angle in turns (0 = +x, clockwise).
//! Stops interpolate premultiplied. Gradients with more than two stops sample a ramp: a 256×1 row of the
//! colour atlas at x = 0 (the caller fills it with [`ramp`] and [`Painter::upload`], then names it with
//! `Instance::ramp(row)`; the atlas must be ≥ 256 wide). `t` picks texel `round(t·255)`; the inline stops are ignored.
//! Shadow sigma is the gaussian deviation (CSS blur radius / 2);
//! an outer shadow is not cut out under its caster (draw the caster on top).
//!
//! # View table
//! Every instance names a [`View`]: an offset added to its rect, clip and control points, an opacity
//! that scales its alpha, a 2D affine (uniform `scale`, 2x2 `transform`, `pivot`) applied after the
//! offset, and a clip in target points (after the transform). Scrolling, fading, scaling or rotating a
//! cached view is one `set_view`; index 0 is the identity and cannot be changed. Shapes stay in view
//! space and the fragment shader measures pixel size from derivatives, so distance-field anti-aliasing
//! is correct at any scale or rotation; quads are padded by one target pixel when a view shrinks.
//! Instance clips live in view space and are transformed with the instances.
//!
//! # Batching
//! A layer is one draw call. [`Break`]s are the only splits: from `at_instance` on, image instances
//! sample the given external texture instead of the colour atlas. Gradient ramps always read the atlas.
//!
//! # Retained layers, blur, and what is left out
//! [`Painter::draw_retained`] keeps a layer's instances in persistent GPU storage and writes only the
//! dirty ranges (its doc states the frames-in-flight rule). [`Painter::blur`] is the only extra pass: a
//! second small pipeline (`blur.*` shaders) runs a downsampling separable gaussian into scratch targets.
//! Pure helpers in `pxui::draw` place images (`fit`, `nine_slice`) and emit per-side borders.
//! Not built, opt-in later and only if measured: group opacity (render a view to a texture and composite
//! it once), texture-cached views and content filters on them, dashed curves.

use ogpu_core::backend::{self, Buffer, ColorAttachment, Commands, Device, Pipeline, Queue, RenderEncoder, RenderTarget, Sampler, ShaderStage, Texture};
use ogpu_core::binding::{self, Kind as LayoutKind, LayoutEntry, Stage as LayoutStage};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor};
use ogpu_core::render_pass::{Cull, Load, Primitive, Store};
use ogpu_core::shader::{self, BindingKind, EntryPoint, Stage};
use ogpu_core::types::{AddressMode, BufferDescriptor, BufferUsage, Extent, MipFilter, Origin, SamplerDescriptor, SamplerFilter, TextureDescriptor, TextureFormat, TextureUsage};

const MSL: &str = include_str!("../shaders/ui.metal");
// Regenerated by shaders/build.sh from ui.vert / ui.frag.
const VERT_SPV: &[u8] = include_bytes!("../shaders/ui.vert.spv");
const FRAG_SPV: &[u8] = include_bytes!("../shaders/ui.frag.spv");
const BLUR_MSL: &str = include_str!("../shaders/blur.metal");
const BLUR_VERT_SPV: &[u8] = include_bytes!("../shaders/blur.vert.spv");
const BLUR_FRAG_SPV: &[u8] = include_bytes!("../shaders/blur.frag.spv");

pub use pxui::draw::{f16, flags, kind, Instance, NO_CLIP};
pub const MAX_VIEWS: usize = 256;
/// Bytes of one view record in the globals buffer: four float4s (see the vertex shader).
const VIEW_BYTES: usize = 64;

/// A per-view record. `offset` moves the view's instances, `opacity` scales their alpha, `clip`
/// (`[x0, y0, x1, y1]` in target points, after the transform) bounds them. Then a 2D affine about
/// `pivot` (in the offset view's own points) maps them to the target: `p' = pivot + scale * M * (p - pivot)`
/// with `transform` = `M` row-major `[m00, m01, m10, m11]` (rotation, skew, flip) and `scale` a uniform factor.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct View { pub offset: [f32; 2], pub opacity: f32, pub clip: [f32; 4], pub scale: f32, pub transform: [f32; 4], pub pivot: [f32; 2] }
impl View {
    pub const IDENTITY: View = View {
        offset: [0.0; 2], opacity: 1.0, clip: [-32768.0, -32768.0, 32767.0, 32767.0],
        scale: 1.0, transform: [1.0, 0.0, 0.0, 1.0], pivot: [0.0; 2],
    };
    /// Rotation by `radians` (clockwise on screen, y down) about `pivot`.
    pub fn rotation(radians: f32, pivot: [f32; 2]) -> View {
        let (s, c) = radians.sin_cos();
        View { transform: [c, -s, s, c], pivot, ..View::IDENTITY }
    }
}

/// Texels in a gradient ramp row.
pub const RAMP_TEXELS: usize = 256;

/// Rasterise gradient `stops` (`(position 0..=1, straight rgba)`, ascending) into `row`: texel `i` holds
/// the colour at `t = i / 255`, interpolated premultiplied like the inline two-stop gradient, clamped
/// outside the first and last stop. Upload it with `painter.upload(AtlasKind::Color, 0, y, 256, 1, row, 1024)`.
pub fn ramp(stops: &[(f32, [u8; 4])], row: &mut [u8; RAMP_TEXELS * 4]) {
    let premul = |c: [u8; 4]| { let a = c[3] as f32 / 255.0; [c[0] as f32 * a, c[1] as f32 * a, c[2] as f32 * a, c[3] as f32] };
    for (i, texel) in row.chunks_exact_mut(4).enumerate() {
        let t = i as f32 / 255.0;
        let next = stops.iter().position(|s| s.0 >= t).unwrap_or(stops.len());
        let c = match (next.checked_sub(1).map(|k| stops[k]), stops.get(next)) {
            (None, Some(s)) => premul(s.1),
            (Some(s), None) => premul(s.1),
            (Some(a), Some(b)) => {
                let f = if b.0 > a.0 { (t - a.0) / (b.0 - a.0) } else { 1.0 };
                let (p, q) = (premul(a.1), premul(b.1));
                std::array::from_fn(|k| p[k] + (q[k] - p[k]) * f)
            }
            (None, None) => [0.0; 4],
        };
        let un = if c[3] > 0.0 { 255.0 / c[3] } else { 0.0 };
        texel.copy_from_slice(&[(c[0] * un).round() as u8, (c[1] * un).round() as u8, (c[2] * un).round() as u8, c[3].round() as u8]);
    }
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
    /// linear, for `flags::SMOOTH` images (scaled pictures); glyphs and the atlas stay nearest
    smooth: Sampler,
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
    layers: Vec<Option<Retained>>,
    retained_bytes: u64,
    blur: Option<Blur>,
    /// Scratch textures replaced by a resize, kept alive until the frames that used them are done.
    retired_textures: Vec<(Texture, usize)>,
}

/// The backdrop-blur pipeline and its two scratch targets (`size` texels each), made on first use.
struct Blur { pipeline: Pipeline, sampler: Sampler, a: Texture, b: Texture, size: (u32, u32) }

/// Highest retained layer index + 1.
pub const MAX_LAYERS: usize = 64;

/// Persistent instance storage of one layer: `FRAMES` copies of `cap` instances in one host buffer.
struct Retained {
    buffer: Buffer,
    ptr: *mut u8,
    cap: usize,
    /// Bytes between copies (256-aligned, so a copy can be bound at an offset).
    stride: usize,
    len: usize,
    /// Per copy: the one `[start, end)` range of instances that differs from the layer's current content.
    pending: [Option<(u32, u32)>; FRAMES],
}

fn union(a: Option<(u32, u32)>, b: (u32, u32)) -> Option<(u32, u32)> {
    if b.0 >= b.1 { return a; }
    Some(a.map_or(b, |a| (a.0.min(b.0), a.1.max(b.1))))
}

fn instance_bytes(instances: &[Instance]) -> &[u8] {
    // SAFETY: Instance is repr(C) plain data with no padding (64 bytes, asserted in pxui::draw).
    unsafe { std::slice::from_raw_parts(instances.as_ptr().cast::<u8>(), instances.len() * 64) }
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

/// (binding, shader kind, layout kind, stage).
type Slot = (i64, BindingKind, LayoutKind, Stage);
/// The painter: two vertex storage buffers, two textures, the nearest sampler, the colour atlas again
/// (always the atlas, for gradient ramps, even inside a break) and the linear sampler (`flags::SMOOTH`).
const UI_SLOTS: [Slot; 7] = [
    (0, BindingKind::StorageBuffer, LayoutKind::Buffer, Stage::Vertex),
    (1, BindingKind::StorageBuffer, LayoutKind::Buffer, Stage::Vertex),
    (2, BindingKind::SampledTexture, LayoutKind::Texture, Stage::Fragment),
    (3, BindingKind::SampledTexture, LayoutKind::Texture, Stage::Fragment),
    (4, BindingKind::Sampler, LayoutKind::Sampler, Stage::Fragment),
    (5, BindingKind::SampledTexture, LayoutKind::Texture, Stage::Fragment),
    (6, BindingKind::Sampler, LayoutKind::Sampler, Stage::Fragment),
];
/// The blur: one vertex storage buffer (params), the source texture and a sampler.
const BLUR_SLOTS: [Slot; 3] = [
    (0, BindingKind::StorageBuffer, LayoutKind::Buffer, Stage::Vertex),
    (1, BindingKind::SampledTexture, LayoutKind::Texture, Stage::Fragment),
    (2, BindingKind::Sampler, LayoutKind::Sampler, Stage::Fragment),
];

/// A pipeline from hand-written MSL or checked-in SPIR-V; `names` are the vertex and fragment entry points.
fn pipeline(device: &Device, format: ColorFormat, depth: DepthFormat, samples: i64, sources: (&str, &[u8], &[u8]), names: [&str; 2], slots: &[Slot], blend: Blend) -> Result<Pipeline, Error> {
    let name = ogpu_core::impl_::backend()?;
    let artifact = |stage: Stage| shader::create(shader::Descriptor {
        backend: name.clone(),
        label: Some("pxui_render".into()),
        bytes: if name == "metal" { sources.0.as_bytes().to_vec() } else if stage == Stage::Vertex { sources.1.to_vec() } else { sources.2.to_vec() },
        entry_points: vec![EntryPoint { name: names[0].into(), stage: Stage::Vertex }, EntryPoint { name: names[1].into(), stage: Stage::Fragment }],
        bindings: slots.iter().filter(|s| s.3 == stage).map(|s| shader::Binding { group: 0, binding: s.0, kind: s.1, visibility: vec![s.3] }).collect(),
    });
    let entries: Vec<LayoutEntry> = slots.iter().map(|s| LayoutEntry {
        binding: s.0, kind: s.2, visibility: vec![if s.3 == Stage::Vertex { LayoutStage::Vertex } else { LayoutStage::Fragment }],
    }).collect();
    let layout = binding::create_pipeline_layout(backend::device_handle(device), &backend::capabilities(device), &[(0, binding::create_layout(&entries)?)])?;
    backend::create_render_pipeline(device, &RenderDescriptor {
        backend: name.clone(), label: Some("pxui_render".into()), layout,
        vertex: artifact(Stage::Vertex)?, vertex_entry: names[0].into(),
        fragment: Some(artifact(Stage::Fragment)?), fragment_entry: Some(names[1].into()),
        color_format: format, depth_format: depth, sample_count: samples,
    }, Some(blend), Some(Primitive::TriangleList), Some(false))
}

impl Painter {
    /// A painter for passes of `format`/`depth`/`samples`, owning a `mask_size` R8 atlas and a
    /// `color_size` RGBA8 atlas (both zeroed).
    pub fn new(device: &Device, format: ColorFormat, depth: DepthFormat, samples: i64, mask_size: (u32, u32), color_size: (u32, u32)) -> Result<Painter, Error> {
        let pipeline = pipeline(device, format, depth, samples, (MSL, VERT_SPV, FRAG_SPV), ["ui_vertex", "ui_fragment"], &UI_SLOTS, Blend::Alpha)?;
        let filtered = |filter: SamplerFilter| backend::create_sampler(device, SamplerDescriptor {
            label: Some("pxui_render".into()), min_filter: filter, mag_filter: filter,
            mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge,
            lod_min: 0.0, lod_max: 0.0, max_anisotropy: 1,
        });
        let (sampler, smooth) = (filtered(SamplerFilter::Nearest)?, filtered(SamplerFilter::Linear)?);
        let mask = atlas(device, "pxui_render.mask", TextureFormat::R8Unorm, mask_size)?;
        let color = atlas(device, "pxui_render.color", TextureFormat::Rgba8Unorm, color_size)?;
        let region_size = 4096 * 64;
        let (ring, ring_ptr) = host_ring(device, region_size)?;
        Ok(Painter {
            device: device.clone(), pipeline, sampler, smooth, mask, color, depth: depth != DepthFormat::NoDepth,
            ring, ring_ptr, region_size, region: 0, cursor: 0, retired: Vec::new(),
            views: [View::IDENTITY; MAX_VIEWS], views_len: 1, draw_calls: 0, layers: Vec::new(), retained_bytes: 0, blur: None, retired_textures: Vec::new(),
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
        self.retained_bytes = 0;
        for (buffer, frames) in &mut self.retired {
            *frames -= 1;
            if *frames == 0 { let _ = backend::destroy_buffer(buffer); }
        }
        self.retired.retain(|(_, frames)| *frames > 0);
        for (texture, frames) in &mut self.retired_textures {
            *frames -= 1;
            if *frames == 0 { let _ = backend::destroy_texture(texture); }
        }
        self.retired_textures.retain(|(_, frames)| *frames > 0);
    }

    /// Draw calls issued since the painter was made.
    pub fn draw_calls(&self) -> u64 { self.draw_calls }
    /// Instance bytes [`Painter::draw_retained`] wrote into layer storage since [`Painter::begin_frame`].
    pub fn retained_bytes(&self) -> u64 { self.retained_bytes }
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

    /// Globals at `start`: target size in points, the first-instance offset, then the view table.
    fn write_header(&self, start: usize, size_px: [u32; 2], scale: f32, first: u32) {
        let globals = [size_px[0] as f32 / scale, size_px[1] as f32 / scale, f32::from_bits(first), 0.0];
        self.write_floats(start, &globals);
        for (i, v) in self.views[..self.views_len].iter().enumerate() {
            let record = [
                v.offset[0], v.offset[1], v.opacity, v.scale, v.clip[0], v.clip[1], v.clip[2], v.clip[3],
                v.transform[0], v.transform[1], v.transform[2], v.transform[3], v.pivot[0], v.pivot[1], 0.0, 0.0,
            ];
            self.write_floats(start + 16 + i * VIEW_BYTES, &record);
        }
    }

    /// Pipeline, state, the globals at `start` and the fixed textures.
    fn bind(&self, pass: &RenderEncoder, start: usize) -> Result<(), Error> {
        backend::set_render_pipeline(pass, &self.pipeline)?;
        backend::set_cull(pass, Cull::CullNone)?;
        if self.depth { backend::set_depth_state(pass, None)?; }
        backend::set_stage_buffer(pass, ShaderStage::Vertex, 1, Some(start as i64), &self.ring)?;
        backend::set_stage_texture(&pass, ShaderStage::Fragment, 2, &self.mask)?;
        backend::set_stage_sampler(pass, ShaderStage::Fragment, 4, &self.sampler)?;
        backend::set_stage_sampler(pass, ShaderStage::Fragment, 6, &self.smooth)?;
        backend::set_stage_texture(&pass, ShaderStage::Fragment, 5, &self.color)
    }

    fn write_floats(&self, at: usize, floats: &[f32]) {
        for (i, f) in floats.iter().enumerate() { self.write(at + 4 * i, &f.to_le_bytes()); }
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
        let header = align(16 + self.views_len * VIEW_BYTES);
        let need = header + (0..=breaks.len()).map(|k| align(segment(k).0.len() * 64)).sum::<usize>();
        let start = self.reserve(need)?;
        self.write_header(start, size_px, scale, 0);
        self.bind(pass, start)?;
        let mut at = start + header;
        let mut bound: Option<*const Texture> = None;
        for k in 0..=breaks.len() {
            let (slice, texture) = segment(k);
            if slice.is_empty() { continue; }
            let bytes = instance_bytes(slice);
            self.write(at, bytes);
            let texture = texture.unwrap_or(&self.color);
            if bound != Some(texture as *const Texture) {
                backend::set_stage_texture(&pass, ShaderStage::Fragment, 3, texture)?;
                bound = Some(texture);
            }
            backend::set_stage_buffer(pass, ShaderStage::Vertex, 0, Some(at as i64), &self.ring)?;
            backend::draw(pass, Primitive::TriangleList, 0, 6, Some(slice.len() as i64))?;
            self.draw_calls += 1;
            at += align(bytes.len());
        }
        backend::flush_host_range(&self.ring, start as i64, at - start)
    }

    /// Backdrop blur, one extra pass pair per blurred view: a separable gaussian of deviation `sigma_px`
    /// (source pixels) over the `src_rect_px` (`[x0, y0, x1, y1]`) region of `source`, run on `queue`
    /// into painter-owned scratch targets. Returns the blurred texture, `1/2` (sigma < 6) or `1/4` of the
    /// region's size per side (the first pass downsamples), RGBA8, whole texture = the region: draw it with
    /// `Instance::image(rect, [0, 0, 1, 1], tint)` through a [`Break`] (rounded corners, tint and view
    /// opacity then apply as for any image) after cloning the returned handle, since it borrows the painter.
    /// `source` is the caller's responsibility: a texture whose passes already ended (or a copy of the
    /// target), never the target of a pass that is still open. The result is valid until the next `blur`;
    /// call it once per blurred view per frame, before that view's draw. Scratch textures are created on
    /// first use and replaced only when the region's downsampled size changes.
    // ponytail: bilinear taps (D=4 skips source rows) and straight-alpha averaging assume an opaque backdrop;
    // a premultiplied/true-box downsample if translucent sources or fine detail matter. Needs the queue
    // to submit its own two passes; they order against the caller's by queue order, not a fence.
    pub fn blur_in(&mut self, commands: &Commands, source: &Texture, src_rect_px: [u32; 4], sigma_px: f32) -> Result<&Texture, Error> {
        const OP: &str = "Painter.blur";
        let d = backend::texture_descriptor(source);
        let (sw, sh) = (d.width as u32, d.height as u32);
        let [x0, y0, x1, y1] = src_rect_px;
        if x0 >= x1 || y0 >= y1 || x1 > sw || y1 > sh { return Err(invalid(OP, "region must be non-empty and inside the source")); }
        if !(sigma_px > 0.0) || !sigma_px.is_finite() { return Err(invalid(OP, "sigma must be positive")); }
        let down = if sigma_px < 6.0 { 2 } else { 4 };
        let size = (((x1 - x0) / down).max(1), ((y1 - y0) / down).max(1));
        let (sigma, radius) = (sigma_px / down as f32, (sigma_px / down as f32 * 3.0).ceil().clamp(1.0, 32.0));
        let target = |label: &str| backend::create_texture(&self.device, TextureDescriptor {
            label: Some(label.into()), width: size.0 as i64, height: size.1 as i64, depth: 1, mip_levels: 1, sample_count: 1,
            format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::RenderAttachment, TextureUsage::TextureBinding, TextureUsage::TextureCopySrc],
        });
        if self.blur.is_none() {
            let pipeline = pipeline(&self.device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, (BLUR_MSL, BLUR_VERT_SPV, BLUR_FRAG_SPV), ["blur_vertex", "blur_fragment"], &BLUR_SLOTS, Blend::Replace)?;
            let sampler = backend::create_sampler(&self.device, SamplerDescriptor {
                label: Some("pxui_render.blur".into()), min_filter: SamplerFilter::Linear, mag_filter: SamplerFilter::Linear,
                mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge,
                lod_min: 0.0, lod_max: 0.0, max_anisotropy: 1,
            })?;
            self.blur = Some(Blur { pipeline, sampler, a: target("pxui_render.blur.a")?, b: target("pxui_render.blur.b")?, size });
        } else if self.blur.as_ref().is_some_and(|b| b.size != size) {
            let (a, b) = (target("pxui_render.blur.a")?, target("pxui_render.blur.b")?);
            let blur = self.blur.as_mut().expect("checked above");
            self.retired_textures.push((std::mem::replace(&mut blur.a, a), FRAMES));
            self.retired_textures.push((std::mem::replace(&mut blur.b, b), FRAMES));
            blur.size = size;
        }
        // Two params records (uv region, then tap step / sigma / radius), 256-byte aligned for binding.
        let (sw, sh, h) = (sw as f32, sh as f32, size.1 as f32);
        let start = self.reserve(2 * ALIGN)?;
        let pass_one = [x0 as f32 / sw, y0 as f32 / sh, (x1 - x0) as f32 / sw, (y1 - y0) as f32 / sh, down as f32 / sw, 0.0, sigma, radius];
        let pass_two = [0.0, 0.0, 1.0, 1.0, 0.0, 1.0 / h, sigma, radius];
        self.write_floats(start, &pass_one);
        self.write_floats(start + ALIGN, &pass_two);
        backend::flush_host_range(&self.ring, start as i64, 2 * ALIGN)?;
        let blur = self.blur.as_ref().expect("created above");
        for (i, (src, dst)) in [(source, &blur.a), (&blur.a, &blur.b)].into_iter().enumerate() {
            let encoder = backend::render_encoder(commands, &RenderTarget {
                colors: vec![ColorAttachment { texture: dst.clone(), resolve: None, load: Load::DontCare, store: Store::Store, clear: (0.0, 0.0, 0.0, 0.0) }],
                depth: None,
            })?;
            backend::set_render_pipeline(&encoder, &blur.pipeline)?;
            backend::set_cull(&encoder, Cull::CullNone)?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, Some((start + i * ALIGN) as i64), &self.ring)?;
            backend::set_stage_texture(&encoder, ShaderStage::Fragment, 1, src)?;
            backend::set_stage_sampler(&encoder, ShaderStage::Fragment, 2, &blur.sampler)?;
            backend::draw(&encoder, Primitive::TriangleList, 0, 3, Some(1))?;
            backend::end_render(&encoder)?;
        }
        Ok(&blur.b)
    }

    /// [`Painter::blur_in`] in commands of its own, committed at once.
    pub fn blur(&mut self, queue: &Queue, source: &Texture, src_rect_px: [u32; 4], sigma_px: f32) -> Result<&Texture, Error> {
        let commands = backend::begin_commands(queue)?;
        self.blur_in(&commands, source, src_rect_px, sigma_px)?;
        backend::commit(&commands)?;
        Ok(&self.blur.as_ref().expect("blur_in made it").b)
    }

    /// Like [`Painter::draw`] for a layer whose instances mostly stay the same between frames. The
    /// painter keeps `instances` in persistent per-`layer` GPU storage (`layer < MAX_LAYERS`) and writes
    /// only `dirty` (`[start, end)` instance ranges changed since this layer's previous call). A first use,
    /// a length change or growth beyond capacity makes everything dirty; an unchanged layer writes nothing.
    /// Call it once per layer per frame, after [`Painter::begin_frame`]; `dirty` must be truthful, the
    /// painter does not compare instances.
    ///
    /// Frames in flight: storage has `FRAMES` (3) copies and frame `n` writes and draws copy `n % 3`, which
    /// the GPU finished with at least 3 frames ago. Every copy keeps the bounding range of the dirty ranges
    /// it missed while another copy was written; its next frame writes that range (ranges the previous 2
    /// frames dirtied are replayed), so a one-instance edit costs 64 bytes in each of 3 consecutive frames.
    /// A fresh or grown allocation is not yet in flight: all copies are filled at once and the old buffer is
    /// retired until its frames finish. Each segment after a [`Break`] gets its own globals record in the
    /// ring because storage bindings need 256-byte offsets (see [`Painter::retained_bytes`]).
    // ponytail: one bounding range per copy (scattered edits rewrite the span between them); a range list
    // per copy if a layer is edited in two far-apart places every frame.
    pub fn draw_retained(&mut self, pass: &RenderEncoder, size_px: [u32; 2], scale: f32, layer: u32, instances: &[Instance], dirty: &[(u32, u32)], breaks: &[Break]) -> Result<(), Error> {
        self.draw_retained_split(pass, size_px, scale, layer, instances, dirty, breaks, &[], &mut |_, _| Ok(None))
    }

    /// [`Painter::draw_retained`] that stops where the caller must end the pass: before the segment that
    /// starts at each instance in `splits` (each must also be some break's `at_instance`; one at the
    /// layer's end is ignored) it calls `split(self, at)`. The callback may record other passes and returns the
    /// pass the rest of the layer draws into, or `None` to go on in the same one.
    pub fn draw_retained_split(&mut self, pass: &RenderEncoder, size_px: [u32; 2], scale: f32, layer: u32, instances: &[Instance], dirty: &[(u32, u32)], breaks: &[Break], splits: &[u32], split: &mut dyn FnMut(&mut Painter, u32) -> Result<Option<RenderEncoder>, Error>) -> Result<(), Error> {
        const OP: &str = "Painter.draw_retained";
        let mut pass = pass.clone();
        if !(scale > 0.0) || size_px[0] == 0 || size_px[1] == 0 { return Err(invalid(OP, "target size and scale must be positive")); }
        if layer as usize >= MAX_LAYERS { return Err(invalid(OP, "layer must be below MAX_LAYERS")); }
        let len = instances.len();
        let mut previous = 0;
        for at in breaks.iter().map(|b| b.at_instance as usize) {
            if at < previous || at > len { return Err(invalid(OP, "breaks must be ascending and within the instances")); }
            previous = at;
        }
        if dirty.iter().any(|d| d.0 > d.1 || d.1 as usize > len) { return Err(invalid(OP, "dirty ranges must be start <= end <= instance count")); }
        let layer = layer as usize;
        if self.layers.len() <= layer { self.layers.resize_with(layer + 1, || None); }
        let fresh = self.layers[layer].as_ref().map_or(true, |s| len > s.cap);
        let changed = fresh || self.layers[layer].as_ref().is_some_and(|s| s.len != len);
        let check = |slice: &[Instance], views_len: usize| match slice.iter().find(|i| i.kind > kind::CURVE || i.view as usize >= views_len) {
            Some(bad) => Err(invalid(OP, if bad.kind > kind::CURVE { "unknown instance kind" } else { "instance names an unset view" })),
            None => Ok(()),
        };
        if changed { check(instances, self.views_len)?; } else { for d in dirty { check(&instances[d.0 as usize..d.1 as usize], self.views_len)?; } }
        if fresh && len > 0 {
            let cap = len.max(256).max(self.layers[layer].as_ref().map_or(0, |s| s.cap * 2));
            let stride = align(cap * 64);
            let descriptor = BufferDescriptor { label: Some("pxui_render.retained".into()), size: (stride * FRAMES) as i64, usage: vec![BufferUsage::Storage] };
            let buffer = backend::create_host_buffer(&self.device, descriptor)?;
            let ptr = backend::host_ptr(&buffer)?.0.as_ptr();
            if let Some(old) = self.layers[layer].replace(Retained { buffer, ptr, cap, stride, len: 0, pending: [None; FRAMES] }) { self.retired.push((old.buffer, FRAMES)); }
        }
        let Some(store) = self.layers[layer].as_mut() else { return Ok(()) };
        let copy = self.region;
        if fresh {
            // New storage is not in flight: fill every copy now.
            for c in 0..FRAMES {
                // SAFETY: copy `c` holds `cap >= len` instances at `c * stride` in the live host-mapped buffer.
                unsafe { std::ptr::copy_nonoverlapping(instances.as_ptr().cast::<u8>(), store.ptr.add(c * store.stride), len * 64) }
                backend::flush_host_range(&store.buffer, (c * store.stride) as i64, len * 64)?;
            }
            self.retained_bytes += (len * 64 * FRAMES) as u64;
        } else {
            let all = [(0, len as u32)];
            for d in if changed { &all[..] } else { dirty } { for p in &mut store.pending { *p = union(*p, *d); } }
            if let Some((lo, hi)) = store.pending[copy].take() {
                let (at, n) = (store.stride * copy + lo as usize * 64, (hi - lo) as usize * 64);
                // SAFETY: `hi <= len <= cap`, so the range lies inside copy `copy`.
                unsafe { std::ptr::copy_nonoverlapping(instance_bytes(instances)[lo as usize * 64..].as_ptr(), store.ptr.add(at), n) }
                backend::flush_host_range(&store.buffer, at as i64, n)?;
                self.retained_bytes += n as u64;
            }
        }
        store.len = len;
        if len == 0 { return Ok(()); }
        let header = align(16 + self.views_len * VIEW_BYTES);
        let mut bound = None;
        for k in 0..=breaks.len() {
            let from = if k == 0 { 0 } else { breaks[k - 1].at_instance as usize };
            let to = breaks.get(k).map_or(len, |b| b.at_instance as usize);
            if k > 0 && splits.contains(&(from as u32)) && to != from {
                if let Some(next) = split(self, from as u32)? { pass = next; bound = None; }
            }
            if from == to { continue; }
            let start = self.reserve(header)?;
            // Bind 4-instance (256-byte) aligned and skip the remainder with the globals' first-instance field.
            self.write_header(start, size_px, scale, (from % 4) as u32);
            backend::flush_host_range(&self.ring, start as i64, header)?;
            self.bind(&pass, start)?;
            let texture = if k == 0 { None } else { breaks[k - 1].texture }.unwrap_or(&self.color);
            if bound != Some(texture as *const Texture) {
                backend::set_stage_texture(&pass, ShaderStage::Fragment, 3, texture)?;
                bound = Some(texture);
            }
            let store = self.layers[layer].as_ref().expect("layer was just stored");
            backend::set_stage_buffer(&pass, ShaderStage::Vertex, 0, Some((store.stride * copy + from / 4 * 256) as i64), &store.buffer)?;
            backend::draw(&pass, Primitive::TriangleList, 0, 6, Some((to - from) as i64))?;
            self.draw_calls += 1;
        }
        Ok(())
    }
}

impl Drop for Painter {
    fn drop(&mut self) {
        let _ = backend::destroy_pipeline(&self.pipeline);
        let _ = backend::destroy_sampler(&self.sampler);
        let _ = backend::destroy_sampler(&self.smooth);
        let _ = backend::destroy_texture(&self.mask);
        let _ = backend::destroy_texture(&self.color);
        let _ = backend::destroy_buffer(&self.ring);
        for (buffer, _) in &self.retired { let _ = backend::destroy_buffer(buffer); }
        for store in self.layers.iter().flatten() { let _ = backend::destroy_buffer(&store.buffer); }
        for (texture, _) in &self.retired_textures { let _ = backend::destroy_texture(texture); }
        if let Some(blur) = &self.blur {
            let _ = backend::destroy_pipeline(&blur.pipeline);
            let _ = backend::destroy_sampler(&blur.sampler);
            let _ = backend::destroy_texture(&blur.a);
            let _ = backend::destroy_texture(&blur.b);
        }
    }
}
