use crate::error::{self, Error, ErrorKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    artifact: Artifact,
    entries: Vec<EntryPoint>,
    layout: Vec<Binding>,
}

#[derive(Clone, Debug, PartialEq)]
enum Artifact { Source(String), Binary(Vec<u8>) }

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
        if value.visibility.windows(2).any(|pair| pair[0] as u8 >= pair[1] as u8) {
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
    let artifact = if descriptor.backend == "metal" {
        source_artifact(String::from_utf8(descriptor.bytes).map_err(|_| error("Metal source is not UTF-8"))?)?
    } else { Artifact::Binary(descriptor.bytes) };
    Ok(Shader {
        backend: descriptor.backend,
        label: descriptor.label,
        artifact,
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
    match &shader.artifact { Artifact::Source(source) => source.as_bytes(), Artifact::Binary(bytes) => bytes }
}

pub fn source(shader: &Shader) -> Option<&str> {
    match &shader.artifact { Artifact::Source(source) => Some(source), Artifact::Binary(_) => None }
}

pub fn entry_points(shader: &Shader) -> &[EntryPoint] {
    &shader.entries
}

pub fn bindings(shader: &Shader) -> &[Binding] {
    &shader.layout
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionConstantValue {
    // The narrow integer kinds carry an i64 so an out-of-range value reaches the range check (as in lib/metal).
    Bool(bool), Int8(i64), Uint8(i64), Int16(i64), Uint16(i64), Int32(i32), Uint32(i64),
    Int64(i64), Uint64Bits(i64), Float16(f64), Float32(f64),
}
impl FunctionConstantValue {
    fn raw(self) -> Result<(i32, i64, f64), &'static str> {
        let within = |value: i64, low: i64, high: i64| (low..=high).contains(&value);
        Ok(match self {
            Self::Bool(value) => (0, i64::from(value), 0.0),
            Self::Int8(value) if within(value, -128, 127) => (1, value, 0.0),
            Self::Uint8(value) if within(value, 0, 255) => (2, value, 0.0),
            Self::Int16(value) if within(value, -32_768, 32_767) => (3, value, 0.0),
            Self::Uint16(value) if within(value, 0, 65_535) => (4, value, 0.0),
            Self::Int32(value) => (5, i64::from(value), 0.0),
            Self::Uint32(value) if within(value, 0, 0xffff_ffff) => (6, value, 0.0),
            Self::Int64(value) => (7, value, 0.0),
            Self::Uint64Bits(value) => (8, value, 0.0),
            Self::Float16(value) => (9, 0, value),
            Self::Float32(value) => (10, 0, value),
            Self::Int8(_) => return Err("int8 function constant is out of range"),
            Self::Uint8(_) => return Err("uint8 function constant is out of range"),
            Self::Int16(_) => return Err("int16 function constant is out of range"),
            Self::Uint16(_) => return Err("uint16 function constant is out of range"),
            Self::Uint32(_) => return Err("uint32 function constant is out of range"),
        })
    }
}

/// Names and constants admitted once for both direct native and ordinary compute creation.
#[derive(Clone, Debug)]
pub struct AdmittedFunction { name: String, constants: Vec<(String, i32, i64, f64)>, specialize: bool }
impl AdmittedFunction {
    pub(crate) fn from_render_entry(name: String) -> Self { Self { name, constants: Vec::new(), specialize: false } }
    pub fn name(&self) -> &str { &self.name }
    pub fn constants(&self) -> &[(String, i32, i64, f64)] { &self.constants }
    pub fn specialized(&self) -> bool { self.specialize }
    pub fn pipeline_options(&self, reflection: bool) -> AdmittedComputeOptions {
        AdmittedComputeOptions { label: Some(self.name.clone()), reflection }
    }
}
pub fn admit_function(name: String, constants: &[(String, FunctionConstantValue)], specialize: bool) -> Result<AdmittedFunction, Error> {
    if !valid_text(&name) { return Err(error("function name must be nonempty and contain no NUL byte")); }
    let mut raw: Vec<(String, i32, i64, f64)> = Vec::with_capacity(constants.len());
    for (name, value) in constants {
        if !valid_text(name) || raw.iter().any(|(seen, _, _, _)| seen == name) {
            return Err(error("function-constant names must be nonempty, unique, and contain no NUL byte"));
        }
        let (tag, integral, floating) = value.raw().map_err(error)?;
        raw.push((name.clone(), tag, integral, floating));
    }
    Ok(AdmittedFunction { name, constants: raw, specialize })
}
pub fn admit_compute_function(name: String, constants: &[(String, ConstantValue)]) -> Result<AdmittedFunction, Error> {
    let constants = constants.iter().map(|(name, value)| (name.clone(), match *value {
        ConstantValue::Bool(v) => FunctionConstantValue::Bool(v), ConstantValue::Int32(v) => FunctionConstantValue::Int32(v),
        ConstantValue::Uint32(v) => FunctionConstantValue::Uint32((v as i64) & 0xffff_ffff),
        ConstantValue::Float32(v) => FunctionConstantValue::Float32(v),
    })).collect::<Vec<_>>();
    admit_function(name, &constants, !constants.is_empty())
}
#[derive(Clone, Debug)]
pub struct AdmittedComputeOptions { label: Option<String>, reflection: bool }
impl AdmittedComputeOptions {
    pub fn label(&self) -> Option<&str> { self.label.as_deref() }
    pub fn reflection(&self) -> bool { self.reflection }
}
pub fn admit_compute_options(label: Option<String>, reflection: bool) -> Result<AdmittedComputeOptions, Error> {
    crate::validation::validate_label("Ogpu.Shader.admit_compute_options", label.as_deref())?;
    Ok(AdmittedComputeOptions { label, reflection })
}

fn source_artifact(source: String) -> Result<Artifact, Error> {
    if source.contains('\0') { Err(error("Metal source contains NUL")) } else { Ok(Artifact::Source(source)) }
}
/// Native source libraries need no declared entry metadata and allow an empty debug label.
pub fn of_metal_source(source: String, label: Option<String>) -> Result<Shader, Error> {
    if source.is_empty() { return Err(error("shader source is empty")); }
    crate::validation::validate_label("Ogpu.Shader.of_metal_source",label.as_deref())?;
    Ok(Shader { backend: "metal".into(), label, artifact: source_artifact(source)?, entries: Vec::new(), layout: Vec::new() })
}
