//! : the mock backend has no renderer shaders, and caps live allocations.
use crate::renderer;
use ogpu_core::backend as b;
use ogpu_core::error::{self, ErrorKind};
use ogpu_core::types::{BufferDescriptor, BufferUsage};

fn select_mock() {
    ogpu_core::impl_::select(ogpu_mock::impl_::implementation());
}

#[test]
fn mock_renderer_returns_typed_unsupported() {
    select_mock();
    assert_eq!(ogpu_core::impl_::backend().unwrap(), "mock", "mock renderer selected the wrong OGPU implementation");
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
    let device = b::create_device(ogpu_core::impl_::create_driver().unwrap()).unwrap();
    let descriptor = BufferDescriptor { label: None, size: 16, usage: vec![BufferUsage::Storage] };
    let held: Vec<b::Buffer> = (0..4096).map(|_| b::create_buffer(&device, descriptor.clone(), None).unwrap()).collect();
    match b::create_buffer(&device, descriptor.clone(), None) {
        Err(e) if e.kind == ErrorKind::Capacity => {}
        _ => panic!("the 4,097th live allocation was accepted"),
    }
    for buffer in &held {
        let _ = b::destroy_buffer(buffer);
    }
    let _ = b::destroy_buffer(&b::create_buffer(&device, descriptor, None).unwrap());
    let _ = b::destroy_device(&device);
}
