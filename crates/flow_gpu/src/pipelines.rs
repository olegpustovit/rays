//! Port of lib/flow_gpu/pipelines.ml: compiled compute pipelines by kernel entry, at most 64.
use crate::small_lru::List;
use flow_shader::emit::Kernel;
use flow_syntax::value;
use ogpu_core::backend::{self as b, Library, Pipeline};
use ogpu_core::shader::{self, EntryPoint, Stage};
use rays_diagnostic::Diagnostic;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::thread::ThreadId;

#[derive(Clone)]
pub struct Compiled {
    pub pipeline: Pipeline,
}

struct Owned {
    compiled: Compiled,
    #[allow(dead_code)]
    library: Library,
}

struct Data {
    device: b::Device,
    cache: RefCell<List<String, Owned>>,
    domain: ThreadId,
    closed: Cell<bool>,
    compilations: Cell<i64>,
    releases: Rc<Cell<i64>>,
    /// by kernel entry
    specializing: RefCell<HashMap<String, Specializing>>,
}

/// A kernel's capture words at its last dispatch, and the specialization last asked for with its
/// background compile, once one started
struct Specializing {
    last: Vec<u32>,
    key: String,
    warm: Option<b::Warm>,
}

#[derive(Clone)]
pub struct T(Rc<Data>);

pub fn diagnostic(error: &ogpu_core::error::Error) -> Diagnostic {
    value::fail("E_GPU", &ogpu_core::error::to_string(error))
}

pub fn is_vulkan() -> bool {
    ogpu_core::impl_::backend().map(|name| name == "vulkan").unwrap_or(false)
}

pub fn create(_clock: &dyn Fn() -> f64, device: &b::Device) -> T {
    let releases = Rc::new(Cell::new(0));
    let counter = releases.clone();
    // Never shed, count only: a pipeline is small and slow to compile again.
    let release = Box::new(move |_: &String, owned: Owned| {
        counter.set(counter.get() + 1);
        let _ = b::destroy_pipeline(&owned.compiled.pipeline);
        let _ = b::destroy_library(&owned.library);
    });
    T(Rc::new(Data {
        device: device.clone(),
        cache: RefCell::new(List::new(64, release)),
        domain: std::thread::current().id(),
        closed: Cell::new(false),
        compilations: Cell::new(0),
        releases,
        specializing: RefCell::new(HashMap::new()),
    }))
}

fn body(kernel: &Kernel) -> Result<(Vec<u8>, String, Vec<shader::Binding>), Diagnostic> {
    if is_vulkan() {
        kernel.spirv.force()
    } else {
        Ok((kernel.source.clone().into_bytes(), kernel.entry.clone(), kernel.interface.clone()))
    }
}

pub fn get(t: &T, kernel: &Kernel) -> Result<Compiled, Diagnostic> {
    let d = &t.0;
    if d.closed.get() || std::thread::current().id() != d.domain {
        return Err(value::fail("E_GPU", "GPU pipeline owner is closed or called from another domain."));
    }
    if let Some(owned) = d.cache.borrow_mut().find(&|k, _| *k == kernel.entry) {
        return Ok(owned.compiled.clone());
    }
    let (bytes, entry, interface) = body(kernel)?;
    compile(t, kernel.entry.clone(), &shader_of(kernel, bytes, &entry, &interface)?, &entry, &interface)
}

fn shader_of(kernel: &Kernel, bytes: Vec<u8>, entry: &str, interface: &[shader::Binding]) -> Result<shader::Shader, Diagnostic> {
    crate::run::gpu(shader::of_source(shader::Descriptor {
        backend: (if is_vulkan() { "vulkan" } else { "metal" }).to_owned(),
        label: Some(kernel.entry.clone()),
        bytes,
        entry_points: vec![EntryPoint { name: entry.to_owned(), stage: Stage::Compute }],
        bindings: interface.to_vec(),
    }))
}

/// `kernel` with its captured uniforms folded into the source where they held still since its
/// last dispatch (`words` are their byte offsets in `uniforms`): a division by a constant is a
/// shift, a branch on one is gone, while a value that moves every frame stays a uniform. None
/// until that compile, started in the driver's background once the same values come twice, has
/// finished: a scrubbed value never waits for a compile, at most one runs per kernel.
pub fn specialized(t: &T, kernel: &Kernel, words: &[usize], uniforms: &[u8]) -> Option<Compiled> {
    let d = &t.0;
    if words.is_empty() || is_vulkan() || d.closed.get() {
        return None;
    }
    let values: Vec<u32> = words.iter().map(|w| u32::from_le_bytes(uniforms[*w..*w + 4].try_into().unwrap())).collect();
    let mut specializing = d.specializing.borrow_mut();
    let slot = specializing.entry(kernel.entry.clone()).or_insert_with(|| Specializing { last: vec![], key: String::new(), warm: None });
    let held: Vec<(usize, u32)> = words.iter().zip(&values).zip(&slot.last).filter(|((_, v), last)| v == last).map(|((w, v), _)| (*w / 4, *v)).collect();
    slot.last = values;
    if held.is_empty() {
        return None;
    }
    let key = format!("{}{held:x?}", kernel.entry);
    if let Some(owned) = d.cache.borrow_mut().find(&|k, _| *k == key) {
        return Some(owned.compiled.clone());
    }
    // the same words read as their literal bits: `uniforms[w]` is a uint, and so is `0x..u`
    let source = || held.iter().fold(kernel.source.clone(), |s, (w, v)| s.replace(&format!("uniforms[{w}]"), &format!("0x{v:x}u")));
    if slot.key != key {
        if slot.warm.as_ref().is_some_and(|warm| warm().is_none()) {
            return None;
        }
        slot.key = key;
        slot.warm = None;
        return None;
    }
    match &slot.warm {
        None => {
            let shader = shader_of(kernel, source().into_bytes(), &kernel.entry, &kernel.interface).ok()?;
            slot.warm = Some(b::warm_library(&d.device, &shader).unwrap_or_else(|| Box::new(|| Some(false))));
            None
        }
        Some(warm) if warm() == Some(true) => {
            slot.warm = None;
            drop(specializing);
            let shader = shader_of(kernel, source().into_bytes(), &kernel.entry, &kernel.interface).ok()?;
            compile(t, key, &shader, &kernel.entry, &kernel.interface).ok()
        }
        // compiling, or failed: the generic kernel stays
        Some(_) => None,
    }
}

fn compile(t: &T, key: String, shader: &shader::Shader, entry: &str, interface: &[shader::Binding]) -> Result<Compiled, Diagnostic> {
    let d = &t.0;
    let library = crate::run::gpu(b::create_library(&d.device, shader))?;
    match b::create_compute_pipeline_from(&library, entry, None, interface) {
        Err(error) => {
            let _ = b::destroy_library(&library);
            Err(diagnostic(&error))
        }
        Ok(pipeline) => {
            d.compilations.set(d.compilations.get() + 1);
            let compiled = Compiled { pipeline };
            d.cache.borrow_mut().add(key, Owned { compiled: compiled.clone(), library });
            Ok(compiled)
        }
    }
}

pub fn close(t: &T) {
    let d = &t.0;
    assert!(std::thread::current().id() == d.domain, "Pipelines.close: creating domain required");
    if !d.closed.get() {
        d.cache.borrow_mut().clear();
        d.specializing.borrow_mut().clear();
        d.closed.set(true);
    }
}

pub mod private {
    use super::*;
    pub fn count(t: &T) -> i64 {
        t.0.cache.borrow().length() as i64
    }
    pub fn compilations(t: &T) -> i64 {
        t.0.compilations.get()
    }
    pub fn releases(t: &T) -> i64 {
        t.0.releases.get()
    }
}
