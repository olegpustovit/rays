//! Port of lib/flow_gpu/image_sink.ml: initial-thread conversion of validated packed vec4 output to
//! a resident RGBA8 texture, drawn by the generated image.rays raster stages: the unorm store
//! clamps channels to [0,1] and rounds to nearest, ties to even. No pixels are read back.
use crate::pipelines::is_vulkan;
use crate::run::{self, gpu};
use ogpu::backend::{self as b, Buffer, ColorAttachment, Commands, Device, Pipeline, Queue, RenderTarget, Texture};
use ogpu::binding::{self, LayoutEntry};
use ogpu::pipeline::{ColorFormat, DepthFormat, RenderDescriptor};
use ogpu::render_pass::{Load, Primitive, Rect, Store};
use ogpu::shader::{self as sh, Binding, BindingKind, EntryPoint, Stage};
use ogpu::types::{BufferDescriptor, BufferUsage, TextureDescriptor, TextureFormat, TextureUsage};
use rays_diagnostic::Diagnostic;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::LazyLock;
use std::thread::ThreadId;

/// image.rays printed as MSL (the Metal build's `image_rgba8.metal`).
static MSL: LazyLock<Result<String, Diagnostic>> = LazyLock::new(|| {
    flow_shader::raster::msl(false, include_str!("../../../../lib/renderer/shaders/image.rays"), "image-vertex", "image-fragment")
});
const VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/image_rgba8.vert.spv");
const FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/image_rgba8.frag.spv");

struct Storage {
    texture: Texture,
    width: i64,
    height: i64,
}

struct Data {
    device: Device,
    queue: Queue,
    pipeline: Pipeline,
    size: Buffer,
    domain: ThreadId,
    storage: RefCell<Option<Storage>>,
    generation: Cell<i64>,
    closed: Cell<bool>,
    textures: Cell<i64>,
}

#[derive(Clone)]
pub struct T(Rc<Data>);

#[derive(Clone)]
pub struct Output {
    owner: T,
    texture: Texture,
    generation: i64,
}

/// image.rays: the packed colours at fragment buffer 0, the width at 1
const RESOURCES: [i64; 2] = [0, 1];

fn error<X>(code: &str, message: &str) -> Result<X, Diagnostic> {
    Err(flow_syntax::value::fail(code, message))
}

pub fn create_owned(device: &Device, queue: &Queue) -> Result<T, Diagnostic> {
    let vulkan = is_vulkan();
    let backend = if vulkan { "vulkan" } else { "metal" };
    let shader = |stage: Stage, bindings: &[i64]| -> Result<sh::Shader, Diagnostic> {
        let bytes = if !vulkan {
            match &*MSL {
                Ok(s) => s.clone().into_bytes(),
                Err(d) => return Err(d.clone()),
            }
        } else if stage == Stage::Vertex {
            VERT.to_vec()
        } else {
            FRAG.to_vec()
        };
        gpu(sh::create(sh::Descriptor {
            backend: backend.to_owned(),
            label: Some("Flow image RGBA8".to_owned()),
            bytes,
            entry_points: vec![EntryPoint { name: "image_vertex".to_owned(), stage: Stage::Vertex }, EntryPoint { name: "image_fragment".to_owned(), stage: Stage::Fragment }],
            bindings: bindings.iter().map(|&binding| Binding { group: 0, binding, kind: BindingKind::StorageBuffer, visibility: vec![stage] }).collect(),
        }))
    };
    let vertex = shader(Stage::Vertex, &[])?;
    let fragment = shader(Stage::Fragment, &RESOURCES)?;
    let bindings = gpu(binding::create_layout(&RESOURCES.iter().map(|&binding| LayoutEntry { binding, kind: binding::Kind::Buffer, visibility: vec![binding::Stage::Fragment] }).collect::<Vec<_>>()))?;
    let layout = gpu(binding::create_pipeline_layout(b::device_handle(device), &b::capabilities(device), &[(0, bindings)]))?;
    let size = gpu(b::create_buffer(device, BufferDescriptor { label: Some("Flow image width".to_owned()), size: 16, usage: vec![BufferUsage::Storage] }, None))?;
    match b::create_render_pipeline(
        device,
        &RenderDescriptor {
            backend: backend.to_owned(),
            label: Some("Flow image RGBA8".to_owned()),
            layout,
            vertex,
            vertex_entry: "image_vertex".to_owned(),
            fragment: Some(fragment),
            fragment_entry: Some("image_fragment".to_owned()),
            color_format: ColorFormat::Rgba8Unorm,
            depth_format: DepthFormat::NoDepth,
            sample_count: 1,
        },
        Some(ogpu::pipeline::Blend::Replace),
        Some(Primitive::TriangleList),
        Some(false),
    ) {
        Err(e) => {
            let _ = b::destroy_buffer(&size);
            Err(run::diagnostic(&e))
        }
        Ok(pipeline) => Ok(T(Rc::new(Data { device: device.clone(), queue: queue.clone(), pipeline, size, domain: std::thread::current().id(), storage: RefCell::new(None), generation: Cell::new(0), closed: Cell::new(false), textures: Cell::new(0) }))),
    }
}

pub fn create(gpu_: &renderer::renderer::gpu) -> Result<T, Diagnostic> {
    create_owned(&renderer::renderer::gpu_device(gpu_), &renderer::renderer::gpu_queue(gpu_))
}

fn release(storage: &Storage) {
    let _ = b::destroy_texture(&storage.texture);
}

fn allocate(t: &T, width: i64, height: i64) -> Result<Storage, Diagnostic> {
    let descriptor = TextureDescriptor {
        label: Some("Flow image RGBA8".to_owned()),
        width,
        height,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage: vec![TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc],
    };
    gpu(ogpu::validation::validate_texture(&b::capabilities(&t.0.device), &descriptor))?;
    let texture = gpu(b::create_texture(&t.0.device, descriptor))?;
    t.0.textures.set(t.0.textures.get() + 1);
    Ok(Storage { texture, width, height })
}

/// The conversion pass: `source`'s width x height vec4s drawn into `texture`.
fn encode_pass(t: &T, commands: &Commands, source: &Buffer, width: i64, height: i64, texture: &Texture) -> Result<(), Diagnostic> {
    let size = (width as f32).to_bits() as i32;
    gpu(b::write_buffer(&t.0.size, 0, &size.to_le_bytes()))?;
    let render = gpu(b::render_encoder(
        commands,
        &RenderTarget { colors: vec![ColorAttachment { texture: texture.clone(), resolve: None, load: Load::DontCare, store: Store::Store, clear: (0.0, 0.0, 0.0, 0.0) }], depth: None },
    ))?;
    let r = (|| -> Result<(), Diagnostic> {
        gpu(b::set_render_pipeline(&render, &t.0.pipeline))?;
        gpu(b::set_viewport(&render, Rect { x: 0, y: 0, width, height }))?;
        gpu(b::set_stage_buffer(&render, b::ShaderStage::Fragment, 0, None, source))?;
        gpu(b::set_stage_buffer(&render, b::ShaderStage::Fragment, 1, None, &t.0.size))?;
        gpu(b::draw(&render, Primitive::TriangleList, 0, 3, None))
    })();
    // the encoder is always ended, as `Fun.protect ~finally` did
    let ended = gpu(b::end_render(&render));
    r.and(ended)
}

/// The conversion pass alone, in the caller's `commands`: `source`'s width*height packed vec4s
/// into an RGBA8 texture with `RenderAttachment` usage (the path tracer's film).
pub fn encode(t: &T, commands: &Commands, source: &Buffer, width: i64, height: i64, texture: &Texture) -> Result<(), Diagnostic> {
    encode_pass(t, commands, source, width, height, texture)
}

/// Completes the conversion pass before publication. The producer must be current and contain
/// exactly width*height vec4 pixels. Unchanged dimensions reuse one texture. Resize allocates a
/// replacement, retaining the old texture until successful completion. Every attempted write
/// invalidates previously borrowed images, including failed conversions.
pub fn convert(t: &T, width: i64, height: i64, input: &run::Output) -> Result<Output, Diagnostic> {
    let d = &*t.0;
    if d.closed.get() || std::thread::current().id() != d.domain {
        return error("E_GPU", "Image sink is closed or called from another domain.");
    }
    if width <= 0 || height <= 0 || width > run::MAX_STRING / 4 / height {
        return error("E_IMAGE", "Image dimensions exceed native storage bounds.");
    }
    if run::width(input) != 4 || run::count(input) != width * height {
        return error("E_IMAGE", "Image sink needs one vec4 output per pixel.");
    }
    let Some(source) = run::buffer(input, None) else {
        return error("E_GPU", "Image producer output was closed or superseded.");
    };
    // Invalidate all borrowed images before any allocation, write or submission.
    d.generation.set(d.generation.get() + 1);
    let reuse = matches!(&*d.storage.borrow(), Some(old) if old.width == width && old.height == height);
    let (storage, fresh) = if reuse {
        let old = d.storage.borrow();
        let old = old.as_ref().unwrap();
        (Storage { texture: old.texture.clone(), width, height }, false)
    } else {
        (allocate(t, width, height)?, true)
    };
    let attempt = (|| -> Result<(), Diagnostic> {
        let commands = gpu(b::begin_commands(&d.queue))?;
        let receipt = (|| {
            encode_pass(t, &commands, &source, width, height, &storage.texture)?;
            gpu(b::commit(&commands))
        })();
        let receipt = match receipt {
            Ok(r) => r,
            Err(e) => {
                let _ = b::abandon(&commands);
                return Err(e);
            }
        };
        gpu(b::complete_through(&d.queue, receipt.epoch))
    })();
    match attempt {
        Ok(()) => {
            let texture = storage.texture.clone();
            if fresh {
                if let Some(old) = d.storage.borrow().as_ref() {
                    release(old);
                }
            }
            *d.storage.borrow_mut() = Some(storage);
            Ok(Output { owner: t.clone(), texture, generation: d.generation.get() })
        }
        Err(e) => {
            if fresh {
                release(&storage);
            }
            Err(e)
        }
    }
}

/// Borrow until the next conversion or close. Another thread returns `None`.
pub fn texture(output: &Output) -> Option<Texture> {
    let d = &*output.owner.0;
    if d.closed.get() || std::thread::current().id() != d.domain || d.generation.get() != output.generation {
        None
    } else {
        Some(output.texture.clone())
    }
}

/// Idempotent on the creating thread. Close before releasing its GPU.
pub fn close(t: &T) {
    let d = &*t.0;
    assert!(std::thread::current().id() == d.domain, "Image_sink.close: creating domain required");
    if !d.closed.get() {
        if let Some(s) = d.storage.borrow_mut().take() {
            release(&s);
        }
        let _ = b::destroy_pipeline(&d.pipeline);
        let _ = b::destroy_buffer(&d.size);
        d.closed.set(true);
    }
}

/// One: the width the stages read, created with the sink.
pub fn buffer_creations(_t: &T) -> i64 {
    1
}

pub fn texture_creations(t: &T) -> i64 {
    t.0.textures.get()
}

pub mod Private {
    pub use super::create_owned;
}
