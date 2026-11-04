use crate::{
    binding::{self, Kind, LayoutEntry, Stage},
    caps,
    error::{Error, ErrorKind},
    handle,
};

fn ok<T>(result: Result<T, Error>) -> T {
    match result {
        Ok(value) => value,
        Err(error) => panic!("{}", crate::error::to_string(&error)),
    }
}

fn expect<T>(kind: ErrorKind, result: Result<T, Error>) {
    match result {
        Err(error) if error.kind == kind => {}
        Err(error) => panic!("unexpected error: {}", crate::error::to_string(&error)),
        Ok(_) => panic!("expected rejection"),
    }
}

fn entry(binding: i64, kind: Kind, visibility: &[Stage]) -> LayoutEntry {
    LayoutEntry { binding, kind, visibility: visibility.to_vec() }
}

#[test]
fn immutable_binding_tables() {
    let capabilities = caps::minimum_m1();
    let device = handle::create_device();
    let foreign = handle::create_device();
    let _buffer = handle::create(&device);
    let texture = handle::create(&device);
    let _sampler = handle::create(&device);
    let _foreign_buffer = handle::create(&foreign);
    expect(
        ErrorKind::InvalidArgument,
        binding::create_layout(&[entry(0, Kind::Buffer, &[Stage::Vertex]), entry(0, Kind::Texture, &[Stage::Fragment])]),
    );
    expect(ErrorKind::InvalidArgument, binding::create_layout(&[entry(0, Kind::Buffer, &[])]));
    expect(
        ErrorKind::InvalidArgument,
        binding::create_layout(&[entry(0, Kind::Buffer, &[Stage::Vertex, Stage::Vertex])]),
    );
    handle::destroy(&texture);
    let too_many: Vec<_> = (0..capabilities.limits.max_bind_groups + 1)
        .map(|group| (group, ok(binding::create_layout(&[]))))
        .collect();
    expect(ErrorKind::Capacity, binding::create_pipeline_layout(&device, &capabilities, &too_many));
    expect(
        ErrorKind::InvalidArgument,
        binding::create_pipeline_layout(&device, &capabilities, &[(1, ok(binding::create_layout(&[])))]),
    );
    expect(
        ErrorKind::InvalidArgument,
        binding::create_pipeline_layout(
            &device,
            &capabilities,
            &[(0, ok(binding::create_layout(&[]))), (0, ok(binding::create_layout(&[])))],
        ),
    );
    handle::destroy_device(&device);
    handle::destroy_device(&foreign);
}
