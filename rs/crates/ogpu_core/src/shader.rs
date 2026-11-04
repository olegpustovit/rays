use crate::error::{self, Error, ErrorKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
    Vertex,
    Fragment,
    Compute,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindingKind {
    UniformBuffer,
    StorageBuffer,
    SampledTexture,
    StorageTexture,
    Sampler,
    AccelerationStructure,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EntryPoint {
    pub name: String,
    pub stage: Stage,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub group: i64,
    pub binding: i64,
    pub kind: BindingKind,
    pub visibility: Vec<Stage>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConstantValue {
    Bool(bool),
    Int32(i32),
    Uint32(i32),
    Float32(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Descriptor {
    pub backend: String,
    pub label: Option<String>,
    pub bytes: Vec<u8>,
    pub entry_points: Vec<EntryPoint>,
    pub bindings: Vec<Binding>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Shader {
    backend: String,
    label: Option<String>,
    artifact: Vec<u8>,
    entries: Vec<EntryPoint>,
    layout: Vec<Binding>,
}

fn error(message: &str) -> Error {
    error::make("Ogpu.Shader.create", ErrorKind::InvalidArgument, message)
}

fn valid_text(value: &str) -> bool {
    !value.is_empty() && !value.contains('\0')
}

fn index_space(kind: BindingKind) -> u8 {
    match kind {
        BindingKind::UniformBuffer | BindingKind::StorageBuffer | BindingKind::AccelerationStructure => 0,
        BindingKind::SampledTexture | BindingKind::StorageTexture => 1,
        BindingKind::Sampler => 2,
    }
}

// ponytail: quadratic duplicate checks assume small reflection tables; use sets if they become hot.
pub fn validate_bindings(bindings: &[Binding]) -> Result<(), Error> {
    let mut seen = Vec::with_capacity(bindings.len());
    for value in bindings {
        if value.group < 0 || value.binding < 0 {
            return Err(error("binding indices must be nonnegative"));
        }
        if value.visibility.is_empty() {
            return Err(error("binding visibility is empty"));
        }
        if value.visibility.windows(2).any(|pair| pair[0] >= pair[1]) {
            return Err(error("binding visibility must be sorted and unique"));
        }
        let key = (value.group, value.binding, index_space(value.kind));
        if seen.contains(&key) {
            return Err(error("duplicate reflected binding"));
        }
        seen.push(key);
    }
    Ok(())
}

pub fn create(descriptor: Descriptor) -> Result<Shader, Error> {
    if descriptor.backend != "metal" && descriptor.backend != "vulkan" && descriptor.backend != "mock" {
        return Err(error("unknown shader backend"));
    }
    if descriptor.label.as_deref().is_some_and(|label| !valid_text(label)) {
        return Err(error("shader label is empty or contains NUL"));
    }
    if descriptor.bytes.is_empty() {
        return Err(error("shader artifact is empty"));
    }
    let mut names: Vec<&str> = Vec::with_capacity(descriptor.entry_points.len());
    for entry in &descriptor.entry_points {
        if !valid_text(&entry.name) {
            return Err(error("entry-point name is empty or contains NUL"));
        }
        if names.contains(&entry.name.as_str()) {
            return Err(error("duplicate entry point"));
        }
        names.push(&entry.name);
    }
    validate_bindings(&descriptor.bindings)?;
    Ok(Shader {
        backend: descriptor.backend,
        label: descriptor.label,
        artifact: descriptor.bytes,
        entries: descriptor.entry_points,
        layout: descriptor.bindings,
    })
}

pub fn of_source(descriptor: Descriptor) -> Result<Shader, Error> {
    create(descriptor)
}

pub fn backend(shader: &Shader) -> &str {
    &shader.backend
}

pub fn label(shader: &Shader) -> Option<&str> {
    shader.label.as_deref()
}

pub fn bytes(shader: &Shader) -> &[u8] {
    &shader.artifact
}

pub fn entry_points(shader: &Shader) -> &[EntryPoint] {
    &shader.entries
}

pub fn bindings(shader: &Shader) -> &[Binding] {
    &shader.layout
}

pub fn validate_constants(constants: &[(String, ConstantValue)]) -> Result<(), Error> {
    let mut names: Vec<&str> = Vec::with_capacity(constants.len());
    for (name, _) in constants {
        if !valid_text(name) || names.contains(&name.as_str()) {
            return Err(error("function constant names must be nonempty and unique"));
        }
        names.push(name);
    }
    Ok(())
}
