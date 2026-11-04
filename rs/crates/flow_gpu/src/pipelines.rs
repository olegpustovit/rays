//! Port of lib/flow_gpu/pipelines.ml: compiled compute pipelines by kernel entry, at most 64.
use crate::small_lru::List;
use flow_shader::emit::Kernel;
use flow_syntax::value;
use ogpu::backend::{self as b, Library, Pipeline};
use ogpu::shader::{self, EntryPoint, Stage};
use rays_diagnostic::Diagnostic;
use std::cell::{Cell, RefCell};
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
}

#[derive(Clone)]
pub struct T(Rc<Data>);

pub fn diagnostic(error: &ogpu::error::Error) -> Diagnostic {
    value::fail("E_GPU", &ogpu::error::to_string(error))
}

pub fn is_vulkan() -> bool {
    ogpu::impl_::backend().map(|name| name == "vulkan").unwrap_or(false)
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
    let entry_key = kernel.entry.clone();
    if let Some(owned) = d.cache.borrow_mut().find(&|k, _| *k == entry_key) {
        return Ok(owned.compiled.clone());
    }
    
    let (bytes, entry, interface) = body(kernel)?;
    let shader = crate::run::gpu(shader::of_source(shader::Descriptor {
        backend: (if is_vulkan() { "vulkan" } else { "metal" }).to_owned(),
        label: Some(kernel.entry.clone()),
        bytes,
        entry_points: vec![EntryPoint { name: entry.clone(), stage: Stage::Compute }],
        bindings: interface.clone(),
    }))?;
    let library = crate::run::gpu(b::create_library(&d.device, &shader))?;
    match b::create_compute_pipeline_from(&library, &entry, None, &interface) {
        Err(error) => {
            let _ = b::destroy_library(&library);
            Err(diagnostic(&error))
        }
        Ok(pipeline) => {
            d.compilations.set(d.compilations.get() + 1);
            let compiled = Compiled { pipeline };
            d.cache.borrow_mut().add(entry_key, Owned { compiled: compiled.clone(), library });
            Ok(compiled)
        }
    }
}

pub fn close(t: &T) {
    let d = &t.0;
    assert!(std::thread::current().id() == d.domain, "Pipelines.close: creating domain required");
    if !d.closed.get() {
        d.cache.borrow_mut().clear();
        d.closed.set(true);
    }
}

pub mod Private {
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
