use crate::binding::{self, Kind as LayoutKind, PipelineLayout, Stage as LayoutStage};
use crate::caps::Caps;
use crate::error::{self, Error, ErrorKind};
use crate::shader::{self, BindingKind, Shader, Stage};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorFormat {
    Rgba8Unorm,
    Bgra8Unorm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DepthFormat {
    NoDepth,
    Depth32Float,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Blend {
    Replace,
    Alpha,
    Add,
    Multiply,
    Screen,
    Subtract,
}

#[derive(Clone, Debug)]
pub struct RenderDescriptor {
    pub backend: String,
    pub label: Option<String>,
    pub layout: PipelineLayout,
    pub vertex: Shader,
    pub vertex_entry: String,
    pub fragment: Option<Shader>,
    pub fragment_entry: Option<String>,
    pub color_format: ColorFormat,
    pub depth_format: DepthFormat,
    pub sample_count: i64,
}

fn invalid(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidArgument, message)
}

fn valid_text(value: &str) -> bool {
    !value.is_empty() && !value.contains('\0')
}

fn layout_kind(kind: BindingKind) -> LayoutKind {
    match kind {
        BindingKind::UniformBuffer | BindingKind::StorageBuffer => LayoutKind::Buffer,
        BindingKind::SampledTexture | BindingKind::StorageTexture => LayoutKind::Texture,
        BindingKind::Sampler => LayoutKind::Sampler,
        BindingKind::AccelerationStructure => LayoutKind::AccelerationStructure,
    }
}

fn layout_stage(stage: Stage) -> LayoutStage {
    match stage {
        Stage::Vertex => LayoutStage::Vertex,
        Stage::Fragment => LayoutStage::Fragment,
        Stage::Compute => LayoutStage::Compute,
    }
}

fn entry(operation: &str, shader_value: &Shader, name: &str, stage: Stage) -> Result<(), Error> {
    if !valid_text(name) {
        return Err(invalid(operation, "entry-point name is empty or contains NUL"));
    }
    if shader::entry_points(shader_value)
        .iter()
        .any(|value| value.name == name && value.stage == stage)
    {
        Ok(())
    } else {
        Err(invalid(operation, "entry point is absent or has the wrong stage"))
    }
}

fn reflection(operation: &str, layout: &PipelineLayout, shaders: &[(&Shader, Stage)]) -> Result<(), Error> {
    let expected: Vec<_> = binding::pipeline_layouts(layout)
        .into_iter()
        .flat_map(|(group, entries)| {
            entries
                .into_iter()
                .map(move |entry| (group, entry.binding, entry))
        })
        .collect();
    let reflected: Vec<_> = shaders
        .iter()
        .flat_map(|(shader_value, stage)| {
            shader::bindings(shader_value)
                .into_iter()
                .map(move |value| (value, *stage))
        })
        .collect();
    let mut slots: Vec<_> = reflected
        .iter()
        .map(|(value, _)| (value.group, value.binding))
        .collect();
    slots.sort_unstable();
    slots.dedup();
    let mut expected_slots: Vec<_> = expected
        .iter()
        .map(|(group, binding, _)| (*group, *binding))
        .collect();
    expected_slots.sort_unstable();
    expected_slots.dedup();
    if slots != expected_slots {
        return Err(invalid(
            operation,
            "shader reflection and pipeline layout have different bindings",
        ));
    }
    // ponytail: linear slot lookup assumes small reflected layouts; index it if counts grow.
    for (value, stage) in reflected {
        let Some((_, _, expected)) = expected
            .iter()
            .find(|(group, binding, _)| *group == value.group && *binding == value.binding)
        else {
            return Err(invalid(operation, "reflected binding is missing from pipeline layout"));
        };
        if layout_kind(value.kind) != expected.kind {
            return Err(invalid(
                operation,
                "reflected binding type does not match pipeline layout",
            ));
        }
        if !expected.visibility.contains(&layout_stage(stage)) {
            return Err(invalid(
                operation,
                "pipeline layout visibility excludes shader stage",
            ));
        }
    }
    Ok(())
}

pub fn create_render(capabilities: &Caps, descriptor: &RenderDescriptor) -> Result<(), Error> {
    let operation = "Ogpu.Pipeline.create_render";
    if descriptor.label.as_deref().is_some_and(|label| !valid_text(label)) {
        return Err(invalid(operation, "pipeline label is empty or contains NUL"));
    }
    if descriptor.backend != "metal" && descriptor.backend != "vulkan" && descriptor.backend != "mock" {
        return Err(invalid(operation, "unknown pipeline backend"));
    }
    let shaders: Vec<_> = std::iter::once(&descriptor.vertex)
        .chain(descriptor.fragment.iter())
        .collect();
    if shaders
        .iter()
        .any(|shader_value| shader::backend(shader_value) != descriptor.backend)
    {
        return Err(invalid(operation, "shader backend does not match pipeline backend"));
    }
    if ![1, 4, 9, 16].contains(&descriptor.sample_count)
        || descriptor.sample_count > capabilities.limits.max_sample_count
    {
        return Err(invalid(operation, "sample count is unsupported"));
    }
    entry(operation, &descriptor.vertex, &descriptor.vertex_entry, Stage::Vertex)?;
    let fragment = match (&descriptor.fragment, &descriptor.fragment_entry) {
        (None, None) => None,
        (Some(shader_value), Some(name)) => {
            entry(operation, shader_value, name, Stage::Fragment)?;
            Some(shader_value)
        }
        (None, Some(_)) | (Some(_), None) => {
            return Err(invalid(
                operation,
                "fragment shader and entry must be supplied together",
            ));
        }
    };
    let mut stages = vec![(&descriptor.vertex, Stage::Vertex)];
    if let Some(shader_value) = fragment {
        stages.push((shader_value, Stage::Fragment));
    }
    reflection(operation, &descriptor.layout, &stages)
}

