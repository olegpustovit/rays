use crate::renderer_retained_plans::Resource;
use crate::scene_execution::{self, Bytes, Draw, empty_bytes, Mesh, MeshKey, new_bytes, PipelineFamily, SampledDraw, SampledTexture, State, TextureLevel};
use crate::side_lru::{self, Item, TableKey};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::pipeline::Blend;
use ogpu_core::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu_core::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
use runtime::PresentationFacts;
use scene_command::render_ir::{self as ir, Command, Geometry, Rect, Transform};
use scene_command::ui_batch::Table as Shapes;
use std::cell::RefCell;
use std::rc::Rc;


pub fn default_state(viewport: (i64, i64, i64, i64), scissor: (i64, i64, i64, i64)) -> State {
    State {
        viewport,
        scissor,
        cull: Cull::CullNone,
        depth_compare: Comparison::Always,
        depth_write: false,
        depth_load: Load::Load,
        depth_clear: 1.0,
        transform_uniforms: None,
    }
}

fn put(target: &mut [u8], index: usize, value: f32) {
    target[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
}

fn put_word(target: &mut [u8], index: usize, value: i32) {
    target[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
}

pub fn write_affine(target: &mut [u8], transform: &Transform) {
    put(target, 0, transform.xx as f32);
    put(target, 1, transform.yx as f32);
    put(target, 2, transform.tx as f32);
    put(target, 3, transform.xy as f32);
    put(target, 4, transform.yy as f32);
    put(target, 5, transform.ty as f32);
}

pub fn identity_transform(transform: &Transform) -> bool {
    transform.xx == 1.0 && transform.xy == 0.0 && transform.yx == 0.0 && transform.yy == 1.0 && transform.tx == 0.0 && transform.ty == 0.0
}

pub fn affine_uniforms(transform: &Transform) -> Bytes {
    let mut target = vec![0u8; 24];
    if identity_transform(transform) {
        put(&mut target, 0, 1.0);
        put(&mut target, 4, 1.0);
    } else {
        write_affine(&mut target, transform);
    }
    new_bytes(target)
}

/// A Scene2 vertex (marks.rays, geometry-vertex): its float2 position and colour word
fn put_vertex(vertices: &mut [u8], index: usize, x: f64, y: f64, color: i32) {
    put(vertices, index * 3, x as f32);
    put(vertices, index * 3 + 1, y as f32);
    put_word(vertices, index * 3 + 2, color);
}

fn scene2_draw(key: String, vertices: Vec<u8>, vertex_count: i64, indices: Vec<u8>, index_count: i64, viewport: (i64, i64, i64, i64), clip: (i64, i64, i64, i64), transform: &Transform) -> SampledDraw {
    scene_execution::SampledDraw { ui: None,
        family: PipelineFamily::Scene2,
        blend: Blend::Alpha,
        texture: None,
        auxiliary: None,
        vertex_attributes: None,
        samples: 1,
        draw: scene_execution::GeometryDraw {
            mesh: Mesh {
                key: MeshKey::Host(key),
                vertices: new_bytes(vertices),
                shared_vertices: None,
                vertex_count,
                indices: new_bytes(indices),
                index_count,
                primitive: Primitive::TriangleList,
            },
            state: State {
                transform_uniforms: if identity_transform(transform) { None } else { Some(affine_uniforms(transform)) },
                ..default_state(viewport, clip)
            },
        },
    }
}

pub fn mesh_of_geometry(number: i64, transform: &Transform, viewport: (i64, i64, i64, i64), clip: (i64, i64, i64, i64), geometry: &Geometry) -> SampledDraw {
    let count = geometry.vertices.len() / 2;
    let mut vertices = vec![0u8; count * 12];
    for index in 0..count {
        put_vertex(&mut vertices, index, geometry.vertices[index * 2], geometry.vertices[index * 2 + 1], geometry.color);
    }
    let mut indices = vec![0u8; geometry.indices.len() * 4];
    for (index, value) in geometry.indices.iter().enumerate() {
        put_word(&mut indices, index, *value as i32);
    }
    scene2_draw(format!("ir-0-{}", number), vertices, count as i64, indices, geometry.indices.len() as i64, viewport, clip, transform)
}

pub fn mesh_of_geometry_run(number: i64, transform: &Transform, viewport: (i64, i64, i64, i64), clip: (i64, i64, i64, i64), commands: &[Command], first: usize, stop: usize) -> SampledDraw {
    let geometries: Vec<&Geometry> = commands[first..stop]
        .iter()
        .map(|command| match command {
            Command::Geometry(g) => g,
            _ => unreachable!("mesh_of_geometry_run: not a geometry"),
        })
        .collect();
    let vertex_count: usize = geometries.iter().map(|g| g.vertices.len() / 2).sum();
    let index_count: usize = geometries.iter().map(|g| g.indices.len()).sum();
    let mut vertices = vec![0u8; vertex_count * 12];
    let mut indices = vec![0u8; index_count * 4];
    let (mut vertex_offset, mut index_offset) = (0usize, 0usize);
    for g in geometries {
        let count = g.vertices.len() / 2;
        for j in 0..count {
            put_vertex(&mut vertices, vertex_offset + j, g.vertices[j * 2], g.vertices[j * 2 + 1], g.color);
        }
        for j in 0..g.indices.len() {
            put_word(&mut indices, index_offset + j, (g.indices[j] + vertex_offset as i64) as i32);
        }
        vertex_offset += count;
        index_offset += g.indices.len();
    }
    scene2_draw(format!("ir-run-{}", number), vertices, vertex_count as i64, indices, index_count as i64, viewport, clip, transform)
}

pub fn compose_raster(a: &Transform, b: &Transform) -> Transform {
    if identity_transform(b) {
        *a
    } else if identity_transform(a) {
        *b
    } else {
        Transform {
            xx: (a.xx * b.xx) + (a.yx * b.xy),
            xy: (a.xy * b.xx) + (a.yy * b.xy),
            yx: (a.xx * b.yx) + (a.yx * b.yy),
            yy: (a.xy * b.yx) + (a.yy * b.yy),
            tx: (a.xx * b.tx) + (a.yx * b.ty) + a.tx,
            ty: (a.xy * b.tx) + (a.yy * b.ty) + a.ty,
        }
    }
}

/// `Shapes` instances sample nearest physical texels.
pub fn ui_sampler() -> SamplerDescriptor {
    SamplerDescriptor {
        label: Some("ui".to_owned()),
        min_filter: SamplerFilter::Nearest,
        mag_filter: SamplerFilter::Nearest,
        mip_filter: MipFilter::NoMip,
        address_u: AddressMode::ClampToEdge,
        address_v: AddressMode::ClampToEdge,
        lod_min: 0.0,
        lod_max: 0.0,
        max_anisotropy: 1,
    }
}

thread_local! {
    static UI_WHITE: Rc<SampledTexture> = Rc::new(SampledTexture {
        key: "ui-white".to_owned(),
        levels: Rc::new(vec![TextureLevel { width: 1, height: 1, bytes: new_bytes(vec![255; 4]) }]),
        sampler: ui_sampler(),
        gpu: None,
    });
}

/// The one shared white texel texture: draws that share it may merge.
pub fn ui_white_texture() -> Rc<SampledTexture> {
    UI_WHITE.with(|texture| texture.clone())
}

struct CachedQuadPayload {
    mesh_key: String,
    vertices: Bytes,
    indices: Bytes,
}

pub struct UiSlot {
    pub vertices: Bytes,
    pub indices: Bytes,
    pub affine: Bytes,
    pub source: Bytes,
}

pub type UiSlotRef = Rc<RefCell<UiSlot>>;

const UI_SLOT_CAPACITY: i64 = 256;
pub const SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY: i64 = 1024 * 1024;

pub fn sampled_texture_bytes(texture: &SampledTexture) -> i64 {
    texture.levels.iter().fold(0i64, |total, level| total.saturating_add(level.bytes.capacity() as i64))
}

pub struct Lowering {
    quad_cache: side_lru::Table,
    quad_payload_cache: side_lru::Table,
    out_slots: Vec<Draw>,
    out_list: Vec<Draw>,
    ui_slots: side_lru::Table,
    ui_indices_scratch: Bytes,
}

pub fn create() -> Lowering {
    Lowering {
        quad_cache: side_lru::create(lru::cache("renderer.quad"), true, 64 * 1024 * 1024, Box::new(|_, _| {}), 1024),
        quad_payload_cache: side_lru::create(lru::cache("renderer.quad_payload"), true, side_lru::UNBOUNDED_BYTES, Box::new(|_, _| {}), 256),
        out_slots: Vec::new(),
        out_list: Vec::new(),
        ui_slots: side_lru::create(lru::cache("renderer.ui_slots"), true, side_lru::UNBOUNDED_BYTES, Box::new(|_, _| {}), UI_SLOT_CAPACITY),
        ui_indices_scratch: empty_bytes(),
    }
}

pub fn ui_affine(transform: &Transform) -> Bytes {
    let mut target = vec![0u8; 24];
    write_affine(&mut target, transform);
    new_bytes(target)
}

pub fn ui_index_bytes(value: &mut Lowering, count: i64) -> Result<Bytes, Error> {
    let operation = "Renderer.ui_index_bytes";
    if count < 0 || count > i32::MAX as i64 / 4 {
        return Err(error::make(operation, ErrorKind::Capacity, "UI indices exceed signed 32-bit vertex addressing"));
    }
    let needed = usize::try_from(count * 24).map_err(|_| error::make(operation, ErrorKind::Capacity, "UI indices exceed native address space"))?;
    let length = value.ui_indices_scratch.len();
    if length < needed {
        let maximum = (i32::MAX as usize / 4).saturating_mul(24);
        let capacity = needed.max(length.saturating_mul(2).min(maximum));
        let mut data = runtime_resources::byte_buffer(operation, capacity)?;
        data.resize(capacity, 0);
        let mut shared = new_bytes(data);
        let data = Rc::get_mut(&mut shared).expect("a fresh buffer is unique");
        for quad in 0..capacity / 24 {
            let base = (quad * 4) as i32;
            let at = quad * 24;
            for (offset, word) in [base, base + 1, base + 2, base, base + 2, base + 3].into_iter().enumerate() {
                data[at + offset * 4..at + offset * 4 + 4].copy_from_slice(&word.to_le_bytes());
            }
        }
        value.ui_indices_scratch = shared;
    }
    if value.ui_indices_scratch.len() == needed {
        Ok(value.ui_indices_scratch.clone())
    } else {
        runtime_resources::copy_bytes(operation, &value.ui_indices_scratch[..needed])
    }
}

pub fn clear(value: &mut Lowering) {
    side_lru::clear(&mut value.quad_cache);
    side_lru::clear(&mut value.quad_payload_cache);
    value.out_slots = Vec::new();
    value.out_list = Vec::new();
    side_lru::clear(&mut value.ui_slots);
}

pub fn find_ui_slot(value: &mut Lowering, key: i64) -> UiSlotRef {
    if let Some(item) = side_lru::find(&mut value.ui_slots, &TableKey::Int(key)) {
        return item.downcast::<RefCell<UiSlot>>().unwrap_or_else(|_| unreachable!("ui slot table item"));
    }
    let slot: UiSlotRef = Rc::new(RefCell::new(UiSlot {
        vertices: empty_bytes(),
        indices: empty_bytes(),
        affine: empty_bytes(),
        source: empty_bytes(),
    }));
    let item: Item = slot.clone();
    side_lru::add(&mut value.ui_slots, 0, TableKey::Int(key), item);
    slot
}


fn rect_bits(r: &Rect) -> [f64; 4] {
    [r.x, r.y, r.width, r.height]
}

fn transform_bits(t_: &Transform) -> [f64; 6] {
    [t_.xx, t_.xy, t_.yx, t_.yy, t_.tx, t_.ty]
}

/// A quad's bits as two independent 64-bit hashes: a table key without text.
fn wide(words: &[u64]) -> TableKey {
    use std::hash::{Hash, Hasher};
    let hash = |seed: u64| {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        seed.hash(&mut hasher);
        words.hash(&mut hasher);
        hasher.finish()
    };
    TableKey::Wide(hash(0x9e37_79b9_7f4a_7c15), hash(0xc2b2_ae3d_27d4_eb4f))
}

/// Cached quads retain texture owners, so pointer identity cannot be reused while the draw is cached.
fn quad(
    state_: &mut Lowering,
    texture: &Rc<SampledTexture>,
    destination: &Rect,
    uv: (f64, f64, f64, f64),
    transform: &Transform,
    clip: (i64, i64, i64, i64),
    framebuffer: (i64, i64, i64, i64),
) -> Draw {
    let (u0, v0, u1, v1) = uv;
    // exact bits on the stack: the payload key, then the texture and framebuffer complete a draw's
    let mut words = [0u64; 24];
    let geometry = rect_bits(destination).into_iter().chain(transform_bits(transform)).map(f64::to_bits)
        .chain([clip.0, clip.1, clip.2, clip.3].map(|v| v as u64)).chain([u0, v0, u1, v1].map(f64::to_bits));
    for (slot, word) in words.iter_mut().zip(geometry) {
        *slot = word;
    }
    let payload_key = wide(&words);
    words[18] = Rc::as_ptr(texture) as usize as u64;
    for (slot, v) in words[19..].iter_mut().zip([framebuffer.0, framebuffer.1, framebuffer.2, framebuffer.3]) {
        *slot = v as u64;
    }
    let quad_key = wide(&words);
    if let Some(item) = side_lru::find(&mut state_.quad_cache, &quad_key) {
        return item.downcast::<SampledDraw>().unwrap_or_else(|_| unreachable!("quad table item"));
    }
    let (vertices, indices, key) = match side_lru::find(&mut state_.quad_payload_cache, &payload_key) {
        Some(item) => {
            let cached = item.downcast::<CachedQuadPayload>().unwrap_or_else(|_| unreachable!("quad payload table item"));
            (cached.vertices.clone(), cached.indices.clone(), cached.mesh_key.clone())
        }
        None => {
            // the quad's constants (marks.rays, quad-vertex): the affine, a white ink, the
            // rectangle and its texture's
            let mut vertices = vec![0u8; 80];
            write_affine(&mut vertices, transform);
            for (index, value) in [1.0, 1.0, 1.0, 1.0, destination.x, destination.y, destination.width, destination.height, u0, v0, u1, v1]
                .into_iter()
                .enumerate()
            {
                put(&mut vertices, 8 + index, value as f32);
            }
            let mut indices = vec![0u8; 24];
            for (i, n) in [0, 1, 2, 0, 2, 3].into_iter().enumerate() {
                put_word(&mut indices, i, n);
            }
            let (cx, cy, cw, ch) = clip;
            let g = |v: f64| cfmt::g(v, 8);
            let key = format!(
                "quad:{},{},{},{}:{},{},{},{},{},{}:{},{},{},{}:{},{},{},{}",
                g(destination.x),
                g(destination.y),
                g(destination.width),
                g(destination.height),
                g(transform.xx),
                g(transform.xy),
                g(transform.yx),
                g(transform.yy),
                g(transform.tx),
                g(transform.ty),
                cx,
                cy,
                cw,
                ch,
                g(u0),
                g(v0),
                g(u1),
                g(v1)
            );
            let payload = Rc::new(CachedQuadPayload {
                mesh_key: key.clone(),
                vertices: new_bytes(vertices),
                indices: new_bytes(indices),
            });
            let result = (payload.vertices.clone(), payload.indices.clone(), key);
            side_lru::add(&mut state_.quad_payload_cache, 0, payload_key, payload);
            result
        }
    };
    let result: Draw = Rc::new(SampledDraw { ui: None,
        family: PipelineFamily::Scene2Textured,
        blend: Blend::Alpha,
        texture: Some(texture.clone()),
        auxiliary: None,
        vertex_attributes: None,
        samples: 1,
        draw: scene_execution::GeometryDraw {
            mesh: Mesh { key: MeshKey::Host(key), vertices, shared_vertices: None, vertex_count: 4, indices, index_count: 6, primitive: Primitive::TriangleList },
            state: default_state(framebuffer, clip),
        },
    });
    let bytes = sampled_texture_bytes(texture);
    if bytes <= SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY {
        let item: Item = result.clone();
        // Cached draws own their snapshots, including pixels from older image generations.
        side_lru::add(&mut state_.quad_cache, bytes, quad_key, item);
    }
    result
}

fn emit(state_: &mut Lowering, number: &mut usize, blend: Blend, draw_: Draw) {
    let draw_ = if draw_.blend == blend {
        draw_
    } else {
        let mut copy = (*draw_).clone();
        copy.blend = blend;
        Rc::new(copy)
    };
    if *number < state_.out_slots.len() {
        state_.out_slots[*number] = draw_;
    } else {
        state_.out_slots.push(draw_);
    }
    *number += 1;
}

fn to_blend(mode: ir::Blend) -> Blend {
    match mode {
        ir::Blend::Replace | ir::Blend::Copy => Blend::Replace,
        ir::Blend::Alpha | ir::Blend::SourceOver => Blend::Alpha,
        ir::Blend::Add => Blend::Add,
        ir::Blend::Multiply => Blend::Multiply,
        ir::Blend::Screen => Blend::Screen,
        ir::Blend::Subtract => Blend::Subtract,
    }
}

pub fn lower_scene2(
    state_: &mut Lowering,
    facts: &PresentationFacts,
    density: i64,
    snapshot: &mut dyn FnMut(i64, Resource) -> Result<(i64, i64, Rc<SampledTexture>), Error>,
    resolve_gpu: &dyn Fn(&Shapes) -> Result<Option<String>, String>,
    rollback: &dyn Fn(),
    resolve: &dyn Fn(i64) -> Option<Resource>,
    commands: &[Command],
) -> Result<Vec<Draw>, Error> {
    let identity = Transform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 };
    let native_projection = Transform {
        xx: 2.0 / facts.logical_width as f64,
        xy: 0.0,
        yx: 0.0,
        yy: -2.0 / facts.logical_height as f64,
        tx: -1.0,
        ty: 1.0,
    };
    let render_transform = |transform: &Transform| compose_raster(&native_projection, transform);
    let framebuffer = (0, 0, facts.logical_width, facts.logical_height);
    let mut transforms: Vec<Transform> = vec![identity];
    let mut clips: Vec<(i64, i64, i64, i64)> = vec![framebuffer];
    let mut blend = Blend::Alpha;
    let mut number: usize = 0;
    let mut failure: Option<String> = None;
    let clip_live = |clips: &Vec<(i64, i64, i64, i64)>| {
        let (_, _, width, height) = *clips.last().expect("clip");
        width > 0 && height > 0
    };
    let mut cursor = 0;
    while cursor < commands.len() && failure.is_none() {
        let first = cursor;
        let command = &commands[first];
        cursor += 1;
        match command {
            Command::Clear(_) => {}
            Command::SetBlend(mode) => blend = to_blend(*mode),
            Command::PushTransform(transform) => {
                let top = *transforms.last().expect("transform");
                transforms.push(compose_raster(&top, transform));
            }
            Command::PopTransform => {
                if transforms.len() > 1 {
                    transforms.pop();
                }
            }
            Command::PushClip(rect) => {
                let (px, py, pw, ph) = *clips.last().expect("clip");
                let x = rect.x.floor() as i64;
                let y = rect.y.floor() as i64;
                let right = (rect.x + rect.width).ceil() as i64;
                let bottom = (rect.y + rect.height).ceil() as i64;
                let x = (px + pw).min(px.max(x));
                let y = (py + ph).min(py.max(y));
                let right = (px + pw).min(right);
                let bottom = (py + ph).min(bottom);
                clips.push((x, y, 0.max(right - x), 0.max(bottom - y)));
            }
            Command::PopClip => {
                if clips.len() > 1 {
                    clips.pop();
                }
            }
            Command::Shapes(shapes) => {
                let gpu_key = match resolve_gpu(shapes) {
                    Ok(key) => key.map(Rc::new),
                    Err(message) => {
                        failure = Some(message);
                        None
                    }
                };
                if failure.is_none() && clip_live(&clips) && shapes.count() > 0 {
                    let count = shapes.count();
                    let instances = shapes.instances_shared();
                    let slot_key = -1 - number as i64;
                    let slot = find_ui_slot(state_, slot_key);
                    {
                        // Published UI bytes remain immutable; the draw retains their Arc directly.
                        let mut s = slot.borrow_mut();
                        if !s.vertices.is_empty() { s.vertices=empty_bytes(); }
                        s.source=s.vertices.clone();
                        if s.indices.len() as i64 != count * 24 {
                            s.indices = match ui_index_bytes(state_, count) {
                                Ok(indices) => indices,
                                Err(error) => { drop(s); rollback(); return Err(error); }
                            };
                        }
                        let affine = ui_affine(&render_transform(transforms.last().expect("transform")));
                        if s.affine != affine {
                            s.affine = affine;
                        }
                    }
                    let s = slot.borrow();
                    let family = match &gpu_key {
                        None => PipelineFamily::Ui,
                        Some(key) if key.starts_with("marks-fill:") => PipelineFamily::MarksFill,
                        Some(_) => PipelineFamily::Marks,
                    };
                    let draw_: Draw = Rc::new(SampledDraw { ui: None,
                        family,
                        blend,
                        texture: if family == PipelineFamily::Ui { Some(ui_white_texture()) } else { None },
                        auxiliary: None,
                        vertex_attributes: None,
                        samples: 1,
                        draw: scene_execution::GeometryDraw {
                            mesh: Mesh {
                                key: match &gpu_key {
                                    Some(key) => MeshKey::GpuVertices { name: key.clone(), indices: None },
                                    None => MeshKey::Host(format!("shapes:{}", slot_key)),
                                },
                                vertices: if gpu_key.is_none() { s.vertices.clone() } else { empty_bytes() },
                                shared_vertices: if gpu_key.is_none() { Some(instances.clone()) } else { None },
                                vertex_count: 4 * count,
                                indices: s.indices.clone(),
                                index_count: 6 * count,
                                primitive: Primitive::TriangleList,
                            },
                            state: State {
                                transform_uniforms: Some(s.affine.clone()),
                                ..default_state(framebuffer, *clips.last().expect("clip"))
                            },
                        },
                    });
                    emit(state_, &mut number, blend, draw_);
                }
            }
            Command::Geometry(geometry) => {
                // A run of more than 64 geometries shares transform, clip, and blend: pack it into
                // one draw. Shorter runs keep one draw per geometry so a moving member does not
                // re-upload its unchanged neighbours.
                let mut stop = cursor;
                while stop < commands.len() && matches!(commands[stop], Command::Geometry(_)) {
                    stop += 1;
                }
                let clip = *clips.last().expect("clip");
                let transform = render_transform(transforms.last().expect("transform"));
                if stop - first > 64 {
                    cursor = stop;
                    if clip_live(&clips) {
                        let d = mesh_of_geometry_run(number as i64, &transform, framebuffer, clip, commands, first, stop);
                        emit(state_, &mut number, blend, Rc::new(d));
                    }
                } else if clip_live(&clips) {
                    let d = mesh_of_geometry(number as i64, &transform, framebuffer, clip, geometry);
                    emit(state_, &mut number, blend, Rc::new(d));
                }
            }
            Command::Image(command) => {
                if clip_live(&clips) {
                    match resolve(command.resource_id) {
                        None => failure = Some("resource id is unbound".to_owned()),
                        Some(source) => match snapshot(density, source) {
                            Err(e) => failure = Some(error::to_string(&e)),
                            Ok((width, height, texture)) => {
                                let s = command.source;
                                if width <= 0 || height <= 0 {
                                    failure = Some("resource extent is invalid".to_owned());
                                } else {
                                    let (w, h) = (width as f64, height as f64);
                                    let uv = (s.x / w, s.y / h, (s.x + s.width) / w, (s.y + s.height) / h);
                                    let transform = render_transform(transforms.last().expect("transform"));
                                    let clip = *clips.last().expect("clip");
                                    let d = quad(state_, &texture, &command.destination, uv, &transform, clip, framebuffer);
                                    emit(state_, &mut number, blend, d);
                                }
                            }
                        },
                    }
                }
            }
            Command::Glyphs(glyphs) => {
                if clip_live(&clips) && !glyphs.glyphs.is_empty() {
                    match resolve(glyphs.resource_id) {
                        None => failure = Some("glyph resource id is unbound".to_owned()),
                        Some(source) => match snapshot(density, source) {
                            Err(e) => failure = Some(error::to_string(&e)),
                            Ok((width, height, texture)) => {
                                for glyph in &glyphs.glyphs {
                                    let destination = Rect { x: glyph.x, y: glyph.y, width: width as f64, height: height as f64 };
                                    let transform = render_transform(transforms.last().expect("transform"));
                                    let clip = *clips.last().expect("clip");
                                    let d = quad(state_, &texture, &destination, (0.0, 0.0, 1.0, 1.0), &transform, clip, framebuffer);
                                    emit(state_, &mut number, blend, d);
                                }
                            }
                        },
                    }
                }
            }
        }
    }
    if let Some(message) = failure {
        rollback();
        return Err(error::make("Renderer.lower_scene2", ErrorKind::InvalidState, &message));
    }
    let same = state_.out_list.len() == number && (0..number).all(|i| Rc::ptr_eq(&state_.out_slots[i], &state_.out_list[i]));
    if same {
        return Ok(state_.out_list.clone());
    }
    let list: Vec<Draw> = state_.out_slots[..number].to_vec();
    state_.out_list = list.clone();
    Ok(list)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn animated_quad_snapshots_obey_the_byte_budget() {
        let mut state = create();
        let destination = Rect { x: 0., y: 0., width: 512., height: 512. };
        let transform = Transform { xx: 1., xy: 0., yx: 0., yy: 1., tx: 0., ty: 0. };
        let viewport = (0, 0, 512, 512);
        let mut snapshots = Vec::new();
        for generation in 0..70 {
            let pixels = new_bytes(vec![generation; 512 * 512 * 4]);
            snapshots.push(Rc::downgrade(&pixels));
            let texture = Rc::new(SampledTexture {
                key: format!("image:frame:{generation}"),
                levels: Rc::new(vec![TextureLevel { width: 512, height: 512, bytes: pixels }]),
                sampler: ui_sampler(), gpu: None,
            });
            let draw = quad(&mut state, &texture, &destination, (0., 0., 1., 1.), &transform, viewport, viewport);
            if generation == 69 {
                let cached = quad(&mut state, &texture, &destination, (0., 0., 1., 1.), &transform, viewport, viewport);
                assert!(Rc::ptr_eq(&draw, &cached), "unchanged snapshots should reuse their draw");
            }
            assert!(side_lru::bytes(&state.quad_cache) <= 64 * 1024 * 1024);
        }
        assert!(snapshots[..6].iter().all(|pixels| pixels.upgrade().is_none()), "old frame pixels stayed alive through cached quads");
        assert_eq!(side_lru::length(&mut state.quad_cache), 64);
    }
}
