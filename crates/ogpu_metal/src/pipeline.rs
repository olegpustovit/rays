//! Ogpu_metal.Pipeline: compute pipelines from a shared library and owned render pipelines.
use ogpu::error::Error;
use crate::{
    device::{fail, of_metal_error, Device},
    library::{self, Library},
};
use metal::{BlendFactor, BlendOperation, BlendState, ColorAttachment, PrimitiveTopology};
use ogpu::{
    error::ErrorKind,
    handle::{self, ResourceHandle},
    pipeline::{self, Blend, ColorFormat, RenderDescriptor},
    shader::{self, Binding, BindingKind, ConstantValue, EntryPoint, Stage},
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

#[derive(Clone)]
pub enum Native {
    Compute(metal::ComputePipeline),
    Render(metal::RenderPipeline),
}

struct PipelineInner {
    native: Native,
    library: metal::Library,
    shared_library: Option<Library>,
    functions: RefCell<Vec<metal::Function>>,
    handle: ResourceHandle,
    device: Device,
    dead: Cell<bool>,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
}

#[derive(Clone)]
pub struct Pipeline(Rc<PipelineInner>);

fn finish_destroy(value: &PipelineInner) -> Result<(), Error> {
    let operation = "Ogpu_metal.Pipeline.destroy";
    match &value.native {
            Native::Compute(x) => x.destroy(),
            Native::Render(x) => x.destroy(),
        }.map_err(|value| of_metal_error(operation, &value))?;
    let mut first: Option<metal::Error> = None;
    for function in value.functions.borrow().iter() {
        if let Err(e) = function.destroy() {
            first.get_or_insert(e);
        }
    }
    match &value.shared_library {
        Some(shared) => shared.detach_pipeline(),
        None => {
            if let Err(e) = value.library.destroy() {
                first.get_or_insert(e);
            }
        }
    }
    if let Some(e) = first {
        return Err(of_metal_error(operation, &e));
    }
    value.device.detach_resource();
    Ok(())
}

/// Resource classes compared between the OGPU interface and Metal's reflection.
const BUFFER: u8 = 0;
const TEXTURE: u8 = 1;
const SAMPLER: u8 = 2;
const ACCEL: u8 = 3;
const OTHER: u8 = 4;

fn expected_class(kind: BindingKind) -> u8 {
    match kind {
        BindingKind::UniformBuffer | BindingKind::StorageBuffer => BUFFER,
        BindingKind::SampledTexture | BindingKind::StorageTexture => TEXTURE,
        BindingKind::Sampler => SAMPLER,
        BindingKind::AccelerationStructure => ACCEL,
    }
}

fn reflected_class(value: &metal::ShaderBinding) -> u8 {
    match value.kind {
        metal::BindingKind::Buffer { .. } => BUFFER,
        metal::BindingKind::Texture { .. } => TEXTURE,
        metal::BindingKind::Sampler => SAMPLER,
        metal::BindingKind::PrimitiveAccelerationStructure | metal::BindingKind::InstanceAccelerationStructure => ACCEL,
        _ => OTHER,
    }
}

fn show(items: &[(i64, u8)]) -> String {
    items
        .iter()
        .map(|(index, class)| {
            let name = match *class {
                BUFFER => "buffer",
                TEXTURE => "texture",
                SAMPLER => "sampler",
                ACCEL => "accel",
                _ => "other",
            };
            format!("{index}:{name}")
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn validate_reflection(op: &str, expected: &[Binding], bindings: &[metal::ShaderBinding]) -> Result<(), Error> {
    if expected.iter().any(|b| b.group != 0) {
        return Err(fail(op, ErrorKind::InvalidArgument, "Metal foundation maps only bind group zero"));
    }
    let mut expected: Vec<(i64, u8)> = expected.iter().map(|b| (b.binding, expected_class(b.kind))).collect();
    let mut actual: Vec<(i64, u8)> = bindings.iter().filter(|b| b.used).map(|b| (b.index as i64, reflected_class(b))).collect();
    expected.sort();
    actual.sort();
    if expected != actual {
        return Err(fail(
            op,
            ErrorKind::InvalidArgument,
            &format!(
                "native Metal reflection does not match OGPU slots and resource classes (interface {}, reflected {})",
                show(&expected),
                show(&actual)
            ),
        ));
    }
    Ok(())
}

fn metal_constants(constants: &[(String, ConstantValue)]) -> Vec<(String, metal::FunctionConstantValue)> {
    constants
        .iter()
        .map(|(name, value)| {
            (
                name.clone(),
                match *value {
                    ConstantValue::Bool(v) => metal::FunctionConstantValue::Bool(v),
                    ConstantValue::Int32(v) => metal::FunctionConstantValue::Int32(v),
                    ConstantValue::Uint32(v) => metal::FunctionConstantValue::Uint32((v as i64) & 0xffff_ffff),
                    ConstantValue::Float32(v) => metal::FunctionConstantValue::Float32(v),
                },
            )
        })
        .collect()
}

pub fn create_compute_from_library(device: &Device, library_value: &Library, entry: &str, constants: &[(String, ConstantValue)], interface: &[Binding]) -> Result<Pipeline, Error> {
    let op = "Ogpu_metal.Pipeline.create_compute_from_library";
    library_value.validate(device)?;
    let native = library_value.metal();
    let found = if constants.is_empty() {
        metal::Function::find(native, entry)
    } else {
        metal::Function::specialize(native, &metal_constants(constants), entry)
    };
    let function = found.map_err(|value| of_metal_error(op, &value))?;
    let pipeline_value = match metal::ComputePipeline::create(&function, Some(entry), true) {
        Ok(p) => p,
        Err(e) => {
            let _ = function.destroy();
            return Err(of_metal_error(op, &e));
        }
    };
    let declared = shader::create(shader::Descriptor {
        backend: "metal".to_owned(),
        label: None,
        bytes: b"x".to_vec(),
        entry_points: vec![EntryPoint { name: entry.to_owned(), stage: Stage::Compute }],
        bindings: interface.to_vec(),
    });
    let checked = declared.and_then(|declared| validate_reflection(op, shader::bindings(&declared), pipeline_value.bindings().unwrap_or(&[])));
    if let Err(e) = checked {
        let _ = pipeline_value.destroy();
        let _ = function.destroy();
        return Err(e);
    }
    library_value.attach_pipeline();
    device.attach_resource();
    Ok(Pipeline(Rc::new(PipelineInner {
        native: Native::Compute(pipeline_value),
        library: native.clone(),
        shared_library: Some(library_value.clone()),
        functions: RefCell::new(vec![function]),
        handle: handle::create(device.handle()),
        device: device.clone(),
        dead: Cell::new(false),
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
    })))
}

fn attachment(blend: Blend, format: metal::metal_format::Format) -> ColorAttachment {
    let mut a = ColorAttachment::new(format);
    let mut set = |source_rgb, destination_rgb, source_alpha, destination_alpha| {
        a.blending = BlendState::Enabled;
        a.source_rgb = source_rgb;
        a.destination_rgb = destination_rgb;
        a.source_alpha = source_alpha;
        a.destination_alpha = destination_alpha;
    };
    match blend {
        Blend::Replace => {}
        Blend::Alpha => set(BlendFactor::SourceAlpha, BlendFactor::OneMinusSourceAlpha, BlendFactor::One, BlendFactor::OneMinusSourceAlpha),
        Blend::Add => set(BlendFactor::One, BlendFactor::One, BlendFactor::One, BlendFactor::One),
        Blend::Multiply => set(BlendFactor::DestinationColor, BlendFactor::Zero, BlendFactor::One, BlendFactor::OneMinusSourceAlpha),
        Blend::Screen => set(BlendFactor::OneMinusDestinationColor, BlendFactor::One, BlendFactor::One, BlendFactor::OneMinusSourceAlpha),
        Blend::Subtract => {
            set(BlendFactor::One, BlendFactor::One, BlendFactor::One, BlendFactor::OneMinusSourceAlpha);
            a.rgb_operation = BlendOperation::ReverseSubtract;
        }
    }
    a
}

fn build_render(support_indirect: bool, topology: PrimitiveTopology, blend: Blend, op: &str, device: &Device, descriptor: &RenderDescriptor) -> Result<Pipeline, Error> {
    let library_value = library::compile_native(op, device, &descriptor.vertex)?;
    let color = match descriptor.color_format {
        ColorFormat::Rgba8Unorm => metal::metal_format::Format::Rgba8Unorm,
        ColorFormat::Bgra8Unorm => metal::metal_format::Format::Bgra8Unorm,
    };
    let compiler = match metal::Compiler::create(device.metal()) {
        Ok(c) => c,
        Err(e) => {
            let _ = library_value.destroy();
            return Err(of_metal_error(op, &e));
        }
    };
    let options = metal::RenderPipelineOptions {
        label: descriptor.label.clone(),
        fragment: descriptor.fragment.as_ref().map(|_| descriptor.fragment_entry.clone().expect("fragment entry")),
        reflection: true,
        raster_sample_count: descriptor.sample_count as usize,
        color_attachments: Some(vec![attachment(blend, color)]),
        primitive_topology: topology,
        support_indirect_command_buffers: support_indirect,
        ..metal::RenderPipelineOptions::default()
    };
    let created = compiler.create_render_pipeline(&library_value, &descriptor.vertex_entry, &options);
    let _ = compiler.destroy();
    let pipeline_value = match created {
        Ok(p) => p,
        Err(e) => {
            let _ = library_value.destroy();
            return Err(of_metal_error(op, &e));
        }
    };
    let vertex_bindings: Vec<metal::ShaderBinding> = pipeline_value.reflection().map(|r| r.vertex_bindings.clone()).unwrap_or_default();
    if let Err(e) = validate_reflection(op, shader::bindings(&descriptor.vertex), &vertex_bindings) {
        let _ = pipeline_value.destroy();
        let _ = library_value.destroy();
        return Err(e);
    }
    device.attach_resource();
    Ok(Pipeline(Rc::new(PipelineInner {
        native: Native::Render(pipeline_value),
        library: library_value,
        shared_library: None,
        functions: RefCell::new(Vec::new()),
        handle: handle::create(device.handle()),
        device: device.clone(),
        dead: Cell::new(false),
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
    })))
}

/// An owned, uncached render pipeline for the portable driver. With `indirect`, the fragment
/// function is retained for argument encoders and indirect command buffers.
pub fn create_render_owned(indirect: bool, topology: PrimitiveTopology, blend: Blend, device: &Device, descriptor: &RenderDescriptor) -> Result<Pipeline, Error> {
    let op = "Ogpu_metal.Pipeline.create_render_owned";
    if device.destroyed() {
        return Err(fail(op, ErrorKind::StaleHandle, "device is destroyed"));
    }
    pipeline::create_render(&device.capabilities(), descriptor)?;
    let value = build_render(indirect, topology, blend, op, device, descriptor)?;
    if !indirect {
        return Ok(value);
    }
    match (&value.0.native, &descriptor.fragment_entry) {
        (Native::Render(native), Some(entry)) => match native.supports_indirect_command_buffers() {
            Err(e) => {
                let _ = value.destroy();
                Err(of_metal_error(op, &e))
            }
            Ok(false) => {
                let _ = value.destroy();
                Err(fail(op, ErrorKind::Unsupported, "native pipeline does not support indirect commands"))
            }
            Ok(true) => match metal::Function::find(&value.0.library, entry) {
                Err(e) => {
                    let _ = value.destroy();
                    Err(of_metal_error(op, &e))
                }
                Ok(function) => {
                    value.0.functions.borrow_mut().push(function);
                    Ok(value)
                }
            },
        },
        _ => Ok(value),
    }
}

impl Pipeline {
    pub fn destroy(&self) -> Result<(), Error> {
        let value = &self.0;
        if value.dead.get() {
            Ok(())
        } else if value.submission_uses.get() > 0 {
            handle::destroy(&value.handle);
            value.dead.set(true);
            value.destroy_requested.set(true);
            Ok(())
        } else {
            finish_destroy(value)?;
            handle::destroy(&value.handle);
            value.dead.set(true);
            Ok(())
        }
    }

    pub fn native(&self) -> &Native {
        &self.0.native
    }

    pub fn argument_encoder(&self, buffer_index: u64) -> Result<metal::ShaderArgumentEncoder, Error> {
        let operation = "Ogpu_metal.Pipeline.argument_encoder";
        // Metal asserts (and aborts) on an index that is no buffer slot; the Rust binding does not range-check it.
        if buffer_index >= 31 {
            return Err(fail(operation, ErrorKind::InvalidArgument, "argument buffer index is outside [0, 31)"));
        }
        let function = match (&self.0.native, self.0.functions.borrow().first()) {
            (Native::Render(_), Some(function)) => function.clone(),
            _ => return Err(fail(operation, ErrorKind::InvalidState, "fragment function was not retained")),
        };
        function.argument_encoder(buffer_index).map_err(|value| of_metal_error(operation, &value))
    }

    pub fn retain_submission(&self) -> Result<(), Error> {
        if self.0.dead.get() {
            return Err(fail("Ogpu_metal.Pipeline.retain_submission", ErrorKind::StaleHandle, "pipeline is destroyed"));
        }
        self.0.submission_uses.set(self.0.submission_uses.get() + 1);
        Ok(())
    }

    pub fn release_submission(&self) {
        self.0.submission_uses.set(self.0.submission_uses.get() - 1);
        if self.0.submission_uses.get() == 0 && self.0.destroy_requested.get() {
            let _ = finish_destroy(&self.0);
        }
    }
}
