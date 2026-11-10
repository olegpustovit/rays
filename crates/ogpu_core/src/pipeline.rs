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

pub fn create_render(capabilities: &Caps, descriptor: &RenderDescriptor) -> Result<AdmittedRenderPipeline, Error> {
    let operation = "Ogpu.Pipeline.create_render";
    if descriptor.label.as_deref().is_some_and(str::is_empty) {
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
    if descriptor.depth_format == DepthFormat::Depth32Float && !capabilities.limits.supports_texture_samples(crate::types::TextureFormat::Depth32Float, descriptor.sample_count) {
        return Err(invalid(operation, "render depth sample count is unsupported"));
    }
    let admitted = admit_render_profile(capabilities, descriptor.vertex_entry.clone(), RenderPipelineOptions {
        label: descriptor.label.clone(), fragment: descriptor.fragment_entry.clone(), reflection: true,
        raster_sample_count: descriptor.sample_count as usize,
        color_attachments: Some(vec![ColorAttachment::new(match descriptor.color_format {
            ColorFormat::Rgba8Unorm => crate::types::TextureFormat::Rgba8Unorm,
            ColorFormat::Bgra8Unorm => crate::types::TextureFormat::Bgra8Unorm })]),
        ..RenderPipelineOptions::default()
    })?;
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
    reflection(operation, &descriptor.layout, &stages)?;
    Ok(admitted)
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BlendState { Disabled, Enabled }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BlendFactor {
    Zero, One, SourceColor, OneMinusSourceColor, SourceAlpha, OneMinusSourceAlpha,
    DestinationColor, OneMinusDestinationColor, DestinationAlpha, OneMinusDestinationAlpha,
    SourceAlphaSaturated, Color, OneMinusColor, Alpha, OneMinusAlpha,
    Source1Color, OneMinusSource1Color, Source1Alpha, OneMinusSource1Alpha,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BlendOperation { Add, Subtract, ReverseSubtract, Min, Max }
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ColorWrite { Red, Green, Blue, Alpha }
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ColorAttachment {
    pub format: crate::types::TextureFormat,
    pub blending: BlendState,
    pub source_rgb: BlendFactor,
    pub destination_rgb: BlendFactor,
    pub rgb_operation: BlendOperation,
    pub source_alpha: BlendFactor,
    pub destination_alpha: BlendFactor,
    pub alpha_operation: BlendOperation,
    pub write_mask: Vec<ColorWrite>,
}
impl ColorAttachment {
    pub fn new(format: crate::types::TextureFormat) -> Self {
        Self {
            format, blending: BlendState::Disabled, source_rgb: BlendFactor::One,
            destination_rgb: BlendFactor::Zero, rgb_operation: BlendOperation::Add,
            source_alpha: BlendFactor::One, destination_alpha: BlendFactor::Zero,
            alpha_operation: BlendOperation::Add,
            write_mask: vec![ColorWrite::Red, ColorWrite::Green, ColorWrite::Blue, ColorWrite::Alpha],
        }
    }
}

#[derive(Clone, Debug)]
pub struct RenderPipelineOptions {
    pub label: Option<String>,
    pub fragment: Option<String>,
    pub reflection: bool,
    pub raster_sample_count: usize,
    pub color_formats: Option<Vec<crate::types::TextureFormat>>,
    pub color_attachments: Option<Vec<ColorAttachment>>,
    pub primitive_topology: crate::render_pass::Primitive,
    pub support_indirect_command_buffers: bool,
}
impl Default for RenderPipelineOptions {
    fn default() -> Self {
        Self {
            label: None, fragment: None, reflection: false, raster_sample_count: 1,
            color_formats: None, color_attachments: None, primitive_topology: crate::render_pass::Primitive::TriangleList,
            support_indirect_command_buffers: false,
        }
    }
}


#[derive(Clone, Debug)]
pub struct AdmittedRenderPipeline(std::rc::Rc<RenderProfile>);
#[derive(Clone, Debug)]
struct RenderProfile { capabilities: Caps, vertex: String, options: RenderPipelineOptions }
impl AdmittedRenderPipeline {
    pub fn admitted_for(&self, capabilities: &Caps) -> bool { self.0.capabilities.limits == capabilities.limits && self.0.capabilities.render_requires_fragment == capabilities.render_requires_fragment }
    pub fn fragment_function(&self) -> Option<crate::shader::AdmittedFunction> {
        self.0.options.fragment.as_ref().map(|name| crate::shader::AdmittedFunction::from_render_entry(name.clone()))
    }
    pub fn vertex(&self) -> &str { &self.0.vertex }
    pub fn options(&self) -> &RenderPipelineOptions { &self.0.options }
    pub fn with_options(mut self, options: crate::backend::RenderPipelineOptions) -> Self {
        let profile = std::rc::Rc::make_mut(&mut self.0);
        profile.options.primitive_topology = options.topology;
        profile.options.support_indirect_command_buffers = options.indirect;
        if let Some(colors) = profile.options.color_attachments.as_mut() {
            for color in colors { *color = color_attachment(options.blend, color.format); }
        }
        self
    }
}

pub fn color_attachment(blend: Blend, format: crate::types::TextureFormat) -> ColorAttachment {
    let mut a = ColorAttachment::new(format);
    let mut set = |source_rgb, destination_rgb, source_alpha, destination_alpha| {
        a.blending = BlendState::Enabled;
        a.source_rgb = source_rgb; a.destination_rgb = destination_rgb;
        a.source_alpha = source_alpha; a.destination_alpha = destination_alpha;
    };
    match blend {
        Blend::Replace => {},
        Blend::Alpha => set(BlendFactor::SourceAlpha, BlendFactor::OneMinusSourceAlpha, BlendFactor::One, BlendFactor::OneMinusSourceAlpha),
        Blend::Add => set(BlendFactor::One, BlendFactor::One, BlendFactor::One, BlendFactor::One),
        Blend::Multiply => set(BlendFactor::DestinationColor, BlendFactor::Zero, BlendFactor::One, BlendFactor::OneMinusSourceAlpha),
        Blend::Screen => set(BlendFactor::OneMinusDestinationColor, BlendFactor::One, BlendFactor::One, BlendFactor::OneMinusSourceAlpha),
        Blend::Subtract => { set(BlendFactor::One, BlendFactor::One, BlendFactor::One, BlendFactor::OneMinusSourceAlpha); a.rgb_operation = BlendOperation::ReverseSubtract; },
    }
    a
}

pub fn admit_render_profile(capabilities: &Caps, vertex: String, mut options: RenderPipelineOptions) -> Result<AdmittedRenderPipeline, Error> {
    const OP: &str = "Ogpu.Pipeline.admit_render_profile";
    crate::validation::validate_label(OP, options.label.as_deref())?;
    if !valid_text(&vertex) || options.fragment.as_deref().is_some_and(|s| !valid_text(s)) {
        return Err(invalid(OP, "render function name is empty or contains NUL"));
    }
    if capabilities.render_requires_fragment && options.fragment.is_none() {
        return Err(invalid(OP, "rasterization requires a fragment function"));
    }
    if options.raster_sample_count == 0 || !options.raster_sample_count.is_power_of_two() {
        return Err(invalid(OP, "render sample count is invalid"));
    }
    if !capabilities.limits.supports_texture_samples(crate::types::TextureFormat::Rgba8Unorm, options.raster_sample_count as i64) {
        return Err(error::make(OP, ErrorKind::Unsupported, "render sample count is unsupported"));
    }
    let colors = match (options.color_formats.take(), options.color_attachments.take()) {
        (Some(_), Some(_)) => return Err(invalid(OP, "color_formats and color_attachments are mutually exclusive")),
        (Some(formats), None) => formats.into_iter().map(ColorAttachment::new).collect(),
        (None, Some(colors)) => colors,
        (None, None) => vec![ColorAttachment::new(crate::types::TextureFormat::Bgra8Unorm)],
    };
    if colors.is_empty() || colors.len() > 8 || colors.iter().any(|c| c.format == crate::types::TextureFormat::Depth32Float) {
        return Err(invalid(OP, "render color attachments require one to eight color formats"));
    }
    if colors.iter().any(|c| c.write_mask.iter().enumerate().any(|(i, channel)| c.write_mask[..i].contains(channel))) {
        return Err(invalid(OP, "render color-write masks contain duplicate channels"));
    }
    options.color_attachments = Some(colors);
    Ok(AdmittedRenderPipeline(std::rc::Rc::new(RenderProfile { capabilities: *capabilities, vertex, options })))
}
