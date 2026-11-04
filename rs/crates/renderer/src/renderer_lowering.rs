use crate::renderer_retained_plans::resource;
use crate::scene_execution::{self, bytes, draw, empty_bytes, mesh, mesh_key, new_bytes, pipeline_family, sampled_draw, sampled_texture, state, texture_level};
use crate::side_lru::{self, Item, TableKey};
use ogpu::error::{self, Error, ErrorKind};
use ogpu::pipeline::Blend;
use ogpu::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
use runtime::presentation_facts;
use scene_command::render_ir::{self as ir, Command, Geometry, Rect, Transform};
use scene_command::ui_batch::Table as Shapes;
use std::cell::RefCell;
use std::rc::Rc;


pub fn default_state(viewport: (i64, i64, i64, i64), scissor: (i64, i64, i64, i64)) -> state {
    state {
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

pub fn affine_uniforms(transform: &Transform) -> bytes {
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

fn scene2_draw(key: String, vertices: Vec<u8>, vertex_count: i64, indices: Vec<u8>, index_count: i64, viewport: (i64, i64, i64, i64), clip: (i64, i64, i64, i64), transform: &Transform) -> sampled_draw {
    scene_execution::sampled_draw {
        family: pipeline_family::Scene2,
        blend: Blend::Alpha,
        texture: None,
        auxiliary: None,
        vertex_attributes: None,
        samples: 1,
        draw: scene_execution::geometry_draw {
            mesh: mesh {
                key: mesh_key::Host(key),
                vertices: new_bytes(vertices),
                vertex_count,
                indices: new_bytes(indices),
                index_count,
                primitive: Primitive::TriangleList,
            },
            state: state {
                transform_uniforms: if identity_transform(transform) { None } else { Some(affine_uniforms(transform)) },
                ..default_state(viewport, clip)
            },
        },
    }
}

pub fn mesh_of_geometry(number: i64, transform: &Transform, viewport: (i64, i64, i64, i64), clip: (i64, i64, i64, i64), geometry: &Geometry) -> sampled_draw {
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

pub fn mesh_of_geometry_run(number: i64, transform: &Transform, viewport: (i64, i64, i64, i64), clip: (i64, i64, i64, i64), commands: &[Command], first: usize, stop: usize) -> sampled_draw {
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

/// PXUI instances: one `Ui` draw per batch. Glyphs sample nearest physical texels.
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
    static UI_WHITE: Rc<sampled_texture> = Rc::new(sampled_texture {
        key: "ui-white".to_owned(),
        levels: Rc::new(vec![texture_level { width: 1, height: 1, bytes: new_bytes(vec![255; 4]) }]),
        sampler: ui_sampler(),
        gpu: None,
    });
}

/// The one shared white texel texture: draws that share it may merge.
pub fn ui_white_texture() -> Rc<sampled_texture> {
    UI_WHITE.with(|texture| texture.clone())
}

/// Whether `target` equals the bytes of `source` from `offset` (a multiple of 8 long).
pub fn same_bytes_range(source: &[u8], offset: usize, target: &[u8]) -> bool {
    target.len() % 8 == 0 && source[offset..offset + target.len()] == *target
}

struct cached_quad_payload {
    payload_mesh_key: String,
    payload_vertices: bytes,
    payload_indices: bytes,
}

pub struct ui_slot {
    pub ui_vertices: bytes,
    pub ui_indices: bytes,
    pub ui_affine: bytes,
    pub ui_source: bytes,
}

pub type ui_slot_ref = Rc<RefCell<ui_slot>>;

const ui_slot_capacity: i64 = 256;
pub const snapshot_cache_entry_byte_capacity: i64 = 1024 * 1024;

pub fn sampled_texture_bytes(texture: &sampled_texture) -> i64 {
    if texture.gpu.is_some() {
        return 0;
    }
    let mut total: i64 = 0;
    for level in texture.levels.iter() {
        let bytes_ = level.bytes.borrow().len() as i64;
        if total > snapshot_cache_entry_byte_capacity - bytes_ {
            total = snapshot_cache_entry_byte_capacity + 1;
        } else {
            total += bytes_;
        }
    }
    total
}

pub struct t {
    quad_cache: side_lru::Table,
    quad_payload_cache: side_lru::Table,
    out_slots: Vec<draw>,
    out_list: Vec<draw>,
    ui_slots: side_lru::Table,
    ui_indices_scratch: bytes,
    ui_affine_scratch: bytes,
}

pub fn create() -> t {
    t {
        quad_cache: side_lru::create(lru::cache("renderer.quad"), true, (1i64 << 62) - 1, Box::new(|_, _| {}), 1024),
        quad_payload_cache: side_lru::create(lru::cache("renderer.quad_payload"), true, (1i64 << 62) - 1, Box::new(|_, _| {}), 256),
        out_slots: Vec::new(),
        out_list: Vec::new(),
        ui_slots: side_lru::create(lru::cache("renderer.ui_slots"), true, (1i64 << 62) - 1, Box::new(|_, _| {}), ui_slot_capacity),
        ui_indices_scratch: empty_bytes(),
        ui_affine_scratch: new_bytes(vec![0; 24]),
    }
}

/// Writes the affine into the shared scratch and returns it: copy it to keep it.
pub fn write_ui_affine(value: &t, transform: &Transform) -> bytes {
    write_affine(&mut value.ui_affine_scratch.borrow_mut(), transform);
    value.ui_affine_scratch.clone()
}

pub fn ui_index_bytes(value: &mut t, count: i64) -> bytes {
    let needed = (count * 24) as usize;
    let length = value.ui_indices_scratch.borrow().len();
    if length < needed {
        let capacity = needed.max(2 * length);
        let mut data = vec![0u8; capacity];
        for quad in 0..capacity / 24 {
            let base = (quad * 4) as i32;
            let at = quad * 24;
            for (offset, word) in [base, base + 1, base + 2, base, base + 2, base + 3].into_iter().enumerate() {
                data[at + offset * 4..at + offset * 4 + 4].copy_from_slice(&word.to_le_bytes());
            }
        }
        value.ui_indices_scratch = new_bytes(data);
    }
    if value.ui_indices_scratch.borrow().len() == needed {
        value.ui_indices_scratch.clone()
    } else {
        new_bytes(value.ui_indices_scratch.borrow()[..needed].to_vec())
    }
}

pub fn clear(value: &mut t) {
    side_lru::clear(&mut value.quad_cache);
    side_lru::clear(&mut value.quad_payload_cache);
    value.out_slots = Vec::new();
    value.out_list = Vec::new();
    side_lru::clear(&mut value.ui_slots);
}

pub fn find_ui_slot(value: &mut t, key: i64) -> ui_slot_ref {
    if let Some(item) = side_lru::find(&mut value.ui_slots, &TableKey::Int(key)) {
        return item.downcast::<RefCell<ui_slot>>().unwrap_or_else(|_| unreachable!("ui slot table item"));
    }
    let slot: ui_slot_ref = Rc::new(RefCell::new(ui_slot {
        ui_vertices: empty_bytes(),
        ui_indices: empty_bytes(),
        ui_affine: empty_bytes(),
        ui_source: empty_bytes(),
    }));
    let item: Item = slot.clone();
    side_lru::add(&mut value.ui_slots, 0, TableKey::Int(key), item);
    slot
}

fn bits(values: &[f64]) -> String {
    values.iter().map(|v| v.to_bits().to_string()).collect::<Vec<_>>().join(",")
}

fn rect_bits(r: &Rect) -> [f64; 4] {
    [r.x, r.y, r.width, r.height]
}

fn transform_bits(t_: &Transform) -> [f64; 6] {
    [t_.xx, t_.xy, t_.yx, t_.yy, t_.tx, t_.ty]
}

/// The lowered scene's quads: the texture is identified by pointer (the OCaml key compares it
/// physically; a cached draw keeps the texture alive, so a pointer cannot be reused while cached).
fn quad(
    state_: &mut t,
    texture: &Rc<sampled_texture>,
    destination: &Rect,
    uv: (f64, f64, f64, f64),
    transform: &Transform,
    clip: (i64, i64, i64, i64),
    framebuffer: (i64, i64, i64, i64),
) -> draw {
    let (u0, v0, u1, v1) = uv;
    let quad_key = format!(
        "{:p}|{}|{}|{:?}|{}|{:?}",
        Rc::as_ptr(texture),
        bits(&rect_bits(destination)),
        bits(&transform_bits(transform)),
        clip,
        bits(&[u0, v0, u1, v1]),
        framebuffer
    );
    if let Some(item) = side_lru::find(&mut state_.quad_cache, &TableKey::Str(quad_key.clone())) {
        return item.downcast::<sampled_draw>().unwrap_or_else(|_| unreachable!("quad table item"));
    }
    let payload_key = format!(
        "{}|{}|{:?}|{}",
        bits(&rect_bits(destination)),
        bits(&transform_bits(transform)),
        clip,
        bits(&[u0, v0, u1, v1])
    );
    let (vertices, indices, key) = match side_lru::find(&mut state_.quad_payload_cache, &TableKey::Str(payload_key.clone())) {
        Some(item) => {
            let cached = item.downcast::<cached_quad_payload>().unwrap_or_else(|_| unreachable!("quad payload table item"));
            (cached.payload_vertices.clone(), cached.payload_indices.clone(), cached.payload_mesh_key.clone())
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
            let payload = Rc::new(cached_quad_payload {
                payload_mesh_key: key.clone(),
                payload_vertices: new_bytes(vertices),
                payload_indices: new_bytes(indices),
            });
            let result = (payload.payload_vertices.clone(), payload.payload_indices.clone(), key);
            side_lru::add(&mut state_.quad_payload_cache, 0, TableKey::Str(payload_key), payload);
            result
        }
    };
    let result: draw = Rc::new(sampled_draw {
        family: pipeline_family::Scene2_textured,
        blend: Blend::Alpha,
        texture: Some(texture.clone()),
        auxiliary: None,
        vertex_attributes: None,
        samples: 1,
        draw: scene_execution::geometry_draw {
            mesh: mesh { key: mesh_key::Host(key), vertices, vertex_count: 4, indices, index_count: 6, primitive: Primitive::TriangleList },
            state: default_state(framebuffer, clip),
        },
    });
    if sampled_texture_bytes(texture) <= snapshot_cache_entry_byte_capacity {
        let item: Item = result.clone();
        side_lru::add(&mut state_.quad_cache, 0, TableKey::Str(quad_key), item);
    }
    result
}

fn emit(state_: &mut t, number: &mut usize, blend: Blend, draw_: draw) {
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
    state_: &mut t,
    facts: &presentation_facts,
    density: i64,
    snapshot: &mut dyn FnMut(i64, resource) -> Result<(i64, i64, Rc<sampled_texture>), Error>,
    resolve_gpu: &dyn Fn(&Shapes) -> Result<Option<String>, String>,
    rollback: &dyn Fn(),
    resolve: &dyn Fn(i64) -> Option<resource>,
    commands: &[Command],
) -> Result<Vec<draw>, Error> {
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
                    Ok(key) => key,
                    Err(message) => {
                        failure = Some(message);
                        None
                    }
                };
                if failure.is_none() && clip_live(&clips) && shapes.count() > 0 {
                    let count = shapes.count();
                    let instances = shapes.instances();
                    let slot_key = -1 - number as i64;
                    let slot = find_ui_slot(state_, slot_key);
                    {
                        // changed content takes the batch's bytes; unchanged keeps the slot's (so the
                        // executor's mesh cache hits by physical identity)
                        let mut s = slot.borrow_mut();
                        if s.ui_vertices.borrow()[..] != *instances {
                            s.ui_vertices = new_bytes(instances.to_vec());
                        }
                        s.ui_source = s.ui_vertices.clone();
                        if s.ui_indices.borrow().len() as i64 != count * 24 {
                            s.ui_indices = ui_index_bytes(state_, count);
                        }
                        let affine = write_ui_affine(state_, &render_transform(transforms.last().expect("transform")));
                        if s.ui_affine.borrow()[..] != affine.borrow()[..] {
                            s.ui_affine = new_bytes(affine.borrow().clone());
                        }
                    }
                    let s = slot.borrow();
                    let family = if gpu_key.is_none() { pipeline_family::Ui } else { pipeline_family::Marks };
                    let draw_: draw = Rc::new(sampled_draw {
                        family,
                        blend,
                        texture: if family == pipeline_family::Marks { None } else { Some(ui_white_texture()) },
                        auxiliary: None,
                        vertex_attributes: None,
                        samples: 1,
                        draw: scene_execution::geometry_draw {
                            mesh: mesh {
                                key: match &gpu_key {
                                    Some(key) => mesh_key::Gpu_vertices(key.clone()),
                                    None => mesh_key::Host(format!("shapes:{}", slot_key)),
                                },
                                vertices: if gpu_key.is_none() { s.ui_vertices.clone() } else { empty_bytes() },
                                vertex_count: 4 * count,
                                indices: s.ui_indices.clone(),
                                index_count: 6 * count,
                                primitive: Primitive::TriangleList,
                            },
                            state: state {
                                transform_uniforms: Some(s.ui_affine.clone()),
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
    let list: Vec<draw> = state_.out_slots[..number].to_vec();
    state_.out_list = list.clone();
    Ok(list)
}
