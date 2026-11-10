//! Native test helpers select Metal or fatal-validation Vulkan. Tests report device absence explicitly.
#![allow(dead_code)]
pub use flow_ir::test_support as test_program;
#[path = "../../../renderer/tests/common/window.rs"]
pub mod window;

use flow_ir::core_exec::{self, Packed};
use flow_syntax::{ty::Ty, value::Value};
use input::Frame;
use ogpu_core::error::{self, ErrorKind};
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

thread_local! {
    static KEEP: std::cell::RefCell<Option<::renderer::renderer::Gpu>> = const { std::cell::RefCell::new(None) };
}

pub fn release_keepalive() {
    flow_gpu::host::clear_thread_state();
    if let Some(lease) = KEEP.with(|k| k.borrow_mut().take()) {
        ::renderer::renderer::release_gpu(&lease);
    }
}

/// The shared lease `gpu()` took.
pub fn lease() -> ::renderer::renderer::Gpu {
    KEEP.with(|k| k.borrow().clone()).expect("gpu() first")
}

fn select() {
    use ogpu_core::impl_::Impl;
    #[cfg(windows)]
    ogpu_core::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND.to_owned(),
        create_driver: Box::new(|| ogpu_vulkan::backend::create_with_validation((true, true))),
        live_handles: Box::new(ogpu_vulkan::impl_::live_handles),
    });
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    #[cfg(not(any(windows, target_os = "macos")))]
    let _ = Impl { backend: String::new(), create_driver: Box::new(|| panic!("no implementation")), live_handles: Box::new(|| 0) };
}

pub fn gpu() -> bool {
    if KEEP.with(|k| k.borrow().is_some()) {
        return true;
    }
    if !cfg!(any(windows, target_os = "macos")) {
        println!("skipped: no native OGPU implementation on this platform");
        return false;
    }
    select();
    match ::renderer::renderer::acquire_gpu() {
        Ok(lease) => {
            KEEP.with(|k| *k.borrow_mut() = Some(lease));
            true
        }
        Err(e) if e.kind == ErrorKind::NoAdapter => {
            println!("skipped: no device ({})", error::to_string(&e));
            false
        }
        Err(e) => panic!("{}", error::to_string(&e)),
    }
}

/// What an `Observe`d resident reports after a call of its base.
pub enum Seen<'a> {
    Invoke(&'a Option<Value>),
    Unfold(&'a Option<Value>),
    Finish(&'a Value, &'a Result<Value, Diagnostic>),
    Count(&'a Option<i64>),
    Settle,
    Close,
}

/// A resident that does what `base` does and tells `seen` of the calls it watches (a call that
/// failed is reported only for `Finish`). `one_by_one` declines `invoke_all`, which leaves fused
/// siblings to run one by one.
pub struct Observe {
    pub base: Arc<dyn core_exec::Resident>,
    pub one_by_one: bool,
    pub seen: Box<dyn Fn(Seen) + Send + Sync>,
}

impl core_exec::Resident for Observe {
    fn fill(&self, ty: &Ty, length: i64, value: &Value) -> Result<Option<Value>, Diagnostic> { self.base.fill(ty, length, value) }
    fn invoke(&self, region: &Packed, args: &[Value], locals: &[Value], live: &Frame) -> Result<Option<Value>, Diagnostic> {
        let value = self.base.invoke(region, args, locals, live)?;
        (self.seen)(Seen::Invoke(&value));
        Ok(value)
    }
    fn invoke_all(&self, region: &Packed, tys: &[Ty], args: &[Value], locals: &[Value], live: &Frame) -> Result<Option<Value>, Diagnostic> {
        if self.one_by_one { Ok(None) } else { self.base.invoke_all(region, tys, args, locals, live) }
    }
    fn unfold(&self, region: &Packed, args: &[Value], steps: i64, source: &Value, locals: &[Value], live: &Frame) -> Result<Option<Value>, Diagnostic> {
        let value = self.base.unfold(region, args, steps, source, locals, live)?;
        (self.seen)(Seen::Unfold(&value));
        Ok(value)
    }
    fn compact(&self, value: &Value) -> Result<Option<Value>, Diagnostic> { self.base.compact(value) }
    fn coerce(&self, ty: &Ty, value: &Value) -> Result<Option<Value>, Diagnostic> { self.base.coerce(ty, value) }
    fn concat(&self, a: &Value, b: &Value) -> Result<Option<Value>, Diagnostic> { self.base.concat(a, b) }
    fn finish(&self, value: &Value) -> Result<Value, Diagnostic> {
        let result = self.base.finish(value);
        (self.seen)(Seen::Finish(value, &result));
        result
    }
    fn keep(&self, value: &Value) -> Result<Option<Value>, Diagnostic> { self.base.keep(value) }
    fn count(&self, value: &Value) -> Result<Option<i64>, Diagnostic> {
        let n = self.base.count(value)?;
        (self.seen)(Seen::Count(&n));
        Ok(n)
    }
    fn capacity(&self, value: &Value) -> Option<i64> { self.base.capacity(value) }
    fn close(&self) {
        (self.seen)(Seen::Close);
        self.base.close()
    }
    fn settle(&self) -> Result<(), Diagnostic> {
        self.base.settle()?;
        (self.seen)(Seen::Settle);
        Ok(())
    }
    fn range(&self, ty: &Ty, value: &Value, div: i64, length: i64) -> Result<Option<Value>, Diagnostic> { self.base.range(ty, value, div, length) }
}
