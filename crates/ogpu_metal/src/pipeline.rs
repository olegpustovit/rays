//! Ogpu_metal.Pipeline: compute pipelines from a shared library and owned render pipelines.
use crate::native;
use ogpu_core::error::Error;
use crate::{
    device::{fail, Device},
    library::{self, Library},
};
use ogpu_core::{
    error::ErrorKind,
    handle::{self, ResourceHandle},
    pipeline::RenderDescriptor,
    shader::{self, Binding, BindingKind},
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

#[derive(Clone)]
pub enum Native {
    Compute(native::ComputePipeline),
    Render(native::RenderPipeline),
}

struct PipelineInner {
    native: Native,
    library: native::Library,
    shared_library: Option<Library>,
    functions: RefCell<Vec<native::Function>>,
    handle: ResourceHandle,
    device: Device,
    dead: Cell<bool>,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
}

#[derive(Clone)]
pub struct Pipeline(Rc<PipelineInner>);

fn finish_destroy(value: &PipelineInner) -> Result<(), Error> {
    match &value.native {
            Native::Compute(x) => x.destroy(),
            Native::Render(x) => x.destroy(),
        }?;
    let mut first: Option<native::Error> = None;
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
        return Err(e);
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

fn reflected_class(value: &native::ShaderBinding) -> u8 {
    match value.kind {
        native::BindingKind::Buffer { .. } => BUFFER,
        native::BindingKind::Texture { .. } => TEXTURE,
        native::BindingKind::Sampler => SAMPLER,
        native::BindingKind::PrimitiveAccelerationStructure | native::BindingKind::InstanceAccelerationStructure => ACCEL,
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

fn validate_reflection(op: &str, expected: &[Binding], bindings: &[native::ShaderBinding]) -> Result<(), Error> {
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

pub fn create_compute_from_library(device: &Device, library_value: &Library, admitted: &ogpu_core::shader::AdmittedFunction, interface: &[Binding]) -> Result<Pipeline, Error> {
    let op = "Ogpu_metal.Pipeline.create_compute_from_library";
    let native = library_value.metal();
    let function = native::Function::create(native, admitted)?;
    let pipeline_value = match native::ComputePipeline::create(&function, &admitted.pipeline_options(true)) {
        Ok(p) => p,
        Err(e) => {
            let _ = function.destroy();
            return Err(e);
        }
    };
    let checked = validate_reflection(op, interface, pipeline_value.bindings().unwrap_or(&[]));
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

fn build_render(op: &str, device: &Device, descriptor: &RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Pipeline, Error> {
    let library_value = library::compile_native(op, device, &descriptor.vertex)?;
    let compiler = match native::Compiler::create(device.metal()) {
        Ok(c) => c,
        Err(e) => {
            let _ = library_value.destroy();
            return Err(e);
        }
    };
    let created = compiler.create_render_pipeline(&library_value, admitted);
    let _ = compiler.destroy();
    let pipeline_value = match created {
        Ok(p) => p,
        Err(e) => {
            let _ = library_value.destroy();
            return Err(e);
        }
    };
    let vertex_bindings: Vec<native::ShaderBinding> = pipeline_value.reflection().map(|r| r.vertex_bindings.clone()).unwrap_or_default();
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
pub fn create_render_owned(device: &Device, descriptor: &RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Pipeline, Error> {
    let op = "Ogpu_metal.Pipeline.create_render_owned";
    let value = build_render(op, device, descriptor, admitted)?;
    if !admitted.options().support_indirect_command_buffers {
        return Ok(value);
    }
    match (&value.0.native, &descriptor.fragment_entry) {
        (Native::Render(native), Some(_)) => match native.supports_indirect_command_buffers() {
            Err(e) => {
                let _ = value.destroy();
                Err(e)
            }
            Ok(false) => {
                let _ = value.destroy();
                Err(fail(op, ErrorKind::Unsupported, "native pipeline does not support indirect commands"))
            }
            Ok(true) => match native::Function::create(&value.0.library, &admitted.fragment_function().expect("Core admitted fragment")) {
                Err(e) => {
                    let _ = value.destroy();
                    Err(e)
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

    pub fn argument_encoder(&self, buffer_index: ogpu_core::validation::AdmittedSlot) -> Result<native::ShaderArgumentEncoder, Error> {
        let operation = "Ogpu_metal.Pipeline.argument_encoder";
        let function = match (&self.0.native, self.0.functions.borrow().first()) {
            (Native::Render(_), Some(function)) => function.clone(),
            _ => return Err(fail(operation, ErrorKind::InvalidState, "fragment function was not retained")),
        };
        function.argument_encoder(buffer_index)
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
