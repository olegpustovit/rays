use crate::impl_;
use ogpu_core::{
    backend,
    error::ErrorKind,
    impl_ as virtual_impl,
    types::{BufferDescriptor, BufferUsage, Extent, Memory, Origin, TextureDescriptor, TextureFormat, TextureUsage},
};

fn select() {
    virtual_impl::select(impl_::implementation());
}

fn buffer(size: i64) -> BufferDescriptor {
    BufferDescriptor { label: None, size, usage: vec![BufferUsage::CopySrc, BufferUsage::CopyDst] }
}

#[test]
fn mock_implementation_tag_and_live_handles() {
    select();
    assert_eq!(virtual_impl::backend().unwrap(), "mock", "mock implementation tag changed");
    assert_eq!(virtual_impl::live_handles().unwrap(), 0);
    let device = backend::create_device(virtual_impl::create_driver().unwrap()).unwrap();
    let b = backend::create_buffer(&device, buffer(16), Some(Memory::Shared)).unwrap();
    assert_eq!(virtual_impl::live_handles().unwrap(), 1);
    backend::write_buffer(&b, 4, &[1, 2, 3, 4]).unwrap();
    assert_eq!(backend::read_buffer(&b, 4, 4).unwrap(), vec![1, 2, 3, 4]);
    assert_eq!(backend::allocated_size(&device), 16);
    assert!(backend::write_buffer(&b, 14, &[0; 4]).is_err());
    backend::destroy_buffer(&b).unwrap();
    assert_eq!(virtual_impl::live_handles().unwrap(), 0);
}

#[test]
fn mock_refuses_what_it_cannot_run() {
    select();
    let device = backend::create_device(virtual_impl::create_driver().unwrap()).unwrap();
    let caps = backend::capabilities(&device);
    assert!(!caps.compute_pipeline && !caps.render_pipeline && !caps.ray_tracing);
    let queue = backend::create_queue(&device).unwrap();
    let commands = backend::begin_commands(&queue).unwrap();
    match backend::compute_encoder(&commands) {
        Err(error) => assert_eq!(error.kind, ErrorKind::Unsupported),
        Ok(_) => panic!("mock executed compute"),
    }
}

#[test]
fn queue_epochs_and_blit_replay() {
    select();
    let device = backend::create_device(virtual_impl::create_driver().unwrap()).unwrap();
    let queue = backend::create_queue(&device).unwrap();
    let source = backend::create_buffer(&device, buffer(512), Some(Memory::Shared)).unwrap();
    let pixels: Vec<u8> = (0..16).collect();
    backend::write_buffer(&source, 0, &pixels[..8]).unwrap();
    backend::write_buffer(&source, 256, &pixels[8..]).unwrap();
    let texture = backend::create_texture(
        &device,
        TextureDescriptor {
            label: None,
            width: 2,
            height: 2,
            depth: 1,
            mip_levels: 1,
            sample_count: 1,
            format: TextureFormat::Rgba8Unorm,
            usage: vec![TextureUsage::TextureCopyDst, TextureUsage::TextureBinding],
        },
    )
    .unwrap();
    let commands = backend::begin_commands(&queue).unwrap();
    let blit = backend::blit_encoder(&commands).unwrap();
    backend::buffer_to_texture(
        &blit,
        &source,
        None,
        256,
        512,
        &texture,
        None,
        Some(Origin { x: 0, y: 0, z: 0 }),
        Extent { width: 2, height: 2, depth: 1 },
    )
    .unwrap();
    backend::end_blit(&blit).unwrap();
    let receipt = backend::commit(&commands).unwrap();
    assert_eq!(receipt.epoch, 1);
    assert_eq!(backend::completed_epoch(&queue), 0);
    assert!(backend::poll_through(&queue, 1).unwrap());
    assert_eq!(backend::completed_epoch(&queue), 1);
    assert_eq!(backend::read_texture(&texture, 8).unwrap(), pixels);
    assert!(backend::complete_through(&queue, 1).is_err(), "epoch 1 is already complete");
}

#[test]
fn mapped_view_is_a_host_copy_synchronised_by_flush_and_invalidate() {
    select();
    let device = backend::create_device(virtual_impl::create_driver().unwrap()).unwrap();
    let mapped = backend::create_mapped_buffer(&device, buffer(8)).unwrap();
    assert!(backend::flush_mapped(&mapped, 8).is_err(), "no host view was written");
    let view = backend::mapped_view(&mapped).unwrap();
    view.borrow_mut()[0] = 1.5;
    backend::flush_mapped(&mapped, 8).unwrap();
    assert_eq!(backend::read_buffer(&mapped, 0, 4).unwrap(), 1.5f32.to_le_bytes());
    backend::release_mapped_view(&mapped).unwrap();
    assert!(view.borrow()[0].is_nan(), "a kept view turns NaN");
    assert!(backend::mapped_view(&mapped).unwrap().borrow()[1].is_nan(), "the successor starts NaN");
    backend::invalidate_mapped(&mapped, 8).unwrap();
    let again = backend::mapped_view(&mapped).unwrap();
    assert_eq!(again.borrow()[0], 1.5);
    assert_eq!(again.borrow()[1], 0.0);
}

#[test]
fn owned_readback_is_lazy_cross_thread_and_retains_the_allocation() {
    let (driver, _) = crate::backend_mock::create();
    let device = backend::create_device(driver).unwrap();
    let plain = backend::create_buffer(&device, buffer(8), None).unwrap();
    assert!(matches!(backend::owned_readback(&plain), Err(error) if error.kind == ErrorKind::InvalidArgument));
    backend::destroy_buffer(&plain).unwrap();

    let mapped = backend::create_mapped_buffer(&device, buffer(8)).unwrap();
    backend::write_buffer(&mapped, 0, &[1; 8]).unwrap();
    let readback = backend::owned_readback(&mapped).unwrap();
    backend::write_buffer(&mapped, 0, &[2, 3, 4, 5, 6, 7, 8, 9]).unwrap();
    // No GPU work is pending; the owner does not write while the foreign reader runs.
    let foreign = readback.clone();
    assert_eq!(std::thread::spawn(move || foreign(2, 4).unwrap()).join().unwrap(), [4, 5, 6, 7]);
    for (offset, length) in [(-1, 1), (9, 0), (7, 2), (i64::MAX, 1), (1, usize::MAX)] {
        assert_eq!(readback(offset, length).unwrap_err().kind, ErrorKind::InvalidArgument);
    }
    assert!(readback(8, 0).unwrap().is_empty());

    backend::destroy_buffer(&mapped).unwrap();
    assert!(matches!(backend::owned_readback(&mapped), Err(error) if error.kind == ErrorKind::StaleHandle));
    drop(mapped);
    backend::destroy_device(&device).unwrap();
    drop(device);
    // Allocation-lifetime conformance only: normal callers stop using readers at semantic free.
    assert_eq!(std::thread::spawn(move || readback(0, 8).unwrap()).join().unwrap(), [2, 3, 4, 5, 6, 7, 8, 9]);
}
