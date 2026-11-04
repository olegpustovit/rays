use crate::{
    caps,
    error::{Error, ErrorKind},
    handle, impl_, types, validation,
};

fn expect(kind: ErrorKind, result: Result<(), Error>) {
    match result {
        Err(error) if error.kind == kind => {}
        _ => panic!("unexpected result"),
    }
}

#[test]
fn unselected_implementation_is_a_typed_error() {
    match impl_::backend() {
        Err(error) => assert_eq!(error.kind, ErrorKind::NoAdapter),
        Ok(_) => panic!("no implementation was selected"),
    }
}

#[test]
fn handle_and_descriptor_foundation() {
    let d1 = handle::create_device();
    let d2 = handle::create_device();
    let h = handle::create(&d1);
    assert!(handle::validate_for("test", &d1, &h).is_ok(), "live handle rejected");
    expect(ErrorKind::CrossDevice, handle::validate_for("test", &d2, &h));
    handle::destroy(&h);
    handle::destroy(&h);
    handle::destroy_device(&d1);
    handle::destroy_device(&d1);
    let capabilities = caps::minimum_m1();
    for size in [0, -1, i64::MAX] {
        expect(
            ErrorKind::InvalidArgument,
            validation::validate_buffer(
                &capabilities,
                &types::BufferDescriptor { label: None, size, usage: vec![types::BufferUsage::CopyDst] },
            ),
        );
    }
    let bad_textures = [
        (0, 1, 1, 1, 1),
        (1, 0, 1, 1, 1),
        (1, 1, 0, 1, 1),
        (1, 1, 1, 0, 1),
        (1, 1, 1, 1, 0),
        (20000, 1, 1, 1, 1),
    ];
    for (width, height, depth, mip_levels, sample_count) in bad_textures {
        expect(
            ErrorKind::InvalidArgument,
            validation::validate_texture(
                &capabilities,
                &types::TextureDescriptor {
                    label: None,
                    width,
                    height,
                    depth,
                    mip_levels,
                    sample_count,
                    format: types::TextureFormat::Rgba8Unorm,
                    usage: vec![types::TextureUsage::TextureBinding],
                },
            ),
        );
    }
}
