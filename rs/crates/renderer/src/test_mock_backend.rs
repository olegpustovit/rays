//! lib/renderer/test_mock_backend.ml: the mock backend has no renderer shaders, and caps live allocations.
use crate::renderer;
use ogpu::backend as b;
use ogpu::error::{self, ErrorKind};
use ogpu::types::{BufferDescriptor, BufferUsage};

fn select_mock() {
    ogpu::impl_::select(ogpu_mock::impl_::implementation());
}

fn ok<A>(result: Result<A, ogpu::error::Error>) -> A {
    result.unwrap_or_else(|e| panic!("{}", error::to_string(&e)))
}

#[test]
fn mock_renderer_returns_typed_unsupported() {
    select_mock();
    assert_eq!(ok(ogpu::impl_::backend()), "mock", "mock renderer selected the wrong OGPU implementation");
    match renderer::create_offscreen(&renderer::default_configuration()) {
        Err(e) if e.kind == ErrorKind::Unsupported => {}
        Err(e) => panic!("{}", error::to_string(&e)),
        Ok(value) => {
            let _ = renderer::destroy(&value);
            panic!("renderer silently created a mock shader path");
        }
    }
}

/// the mock is a discrete device: a driver caps live allocations, as Vulkan's 4,096
#[test]
fn mock_caps_live_allocations() {
    select_mock();
    let device = ok(b::create_device(ok(ogpu::impl_::create_driver())));
    let descriptor = BufferDescriptor { label: None, size: 16, usage: vec![BufferUsage::Storage] };
    let held: Vec<b::Buffer> = (0..4096).map(|_| ok(b::create_buffer(&device, descriptor.clone(), None))).collect();
    match b::create_buffer(&device, descriptor.clone(), None) {
        Err(e) if e.kind == ErrorKind::Capacity => {}
        _ => panic!("the 4,097th live allocation was accepted"),
    }
    for buffer in &held {
        let _ = b::destroy_buffer(buffer);
    }
    let _ = b::destroy_buffer(&ok(b::create_buffer(&device, descriptor, None)));
    let _ = b::destroy_device(&device);
}
